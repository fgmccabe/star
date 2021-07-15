/*
 Buffer implementation
 Copyright (c) 2016, 2017. Francis G. McCabe
 */

#include "stringBufferP.h"

#include <stdlib.h>
#include <assert.h>
#include <starOptions.h>
#include <ooio.h>

/* Set up the buffer file class */

static void initBufferClass(classPo class, classPo req);

static void BufferDestroy(objectPo o);

static void BufferInit(objectPo o, va_list *args);

static retCode bufferInBytes(ioPo io, byte *ch, integer count, integer *actual);

static retCode bufferOutBytes(ioPo io, byte *b, integer count, integer *actual);

static retCode bufferBackByte(ioPo io, byte b);

static retCode bufferAtEof(ioPo io);

static retCode bufferFlusher(ioPo io, long count);

static retCode bufferClose(ioPo io);

BufferClassRec StrBufferClass = {
  {(classPo) &IoClass,                    /* parent class is io object */
    "buffer",                             /* this is the buffer class */
    initBufferClass,                      /* Buffer class initializer */
    O_INHERIT_DEF,                        /* Buffer object element creation */
    BufferDestroy,                        /* Buffer objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    BufferInit,                           /* initialization of a buffer object */
    sizeof(BufferObject),                 /* size of a buffer object */
    NULL,                                 /* pool of buffer values */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,                    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {bufferInBytes,                         /* inByte  */
    bufferOutBytes,                       /* outBytes  */
    bufferBackByte,                       /* backByte */
    bufferAtEof,                          /* at end of file? */
    bufferFlusher,                        /* flush  */
    bufferClose                           /* close  */
  }};

classPo strBufferClass = (classPo) &StrBufferClass;

static void initBufferClass(classPo class, classPo req) {
}

// IO initialization should already be done at this point
static void BufferInit(objectPo o, va_list *args) {
  strBufferPo f = O_BUFFER(o);

  // Set up the buffer pointers
  f->buffer.in_pos = 0;
  f->buffer.out_pos = 0;
  setEncoding(O_IO(f), va_arg(*args, ioEncoding)); /* set up the encoding */
  f->buffer.buffer = va_arg(*args, char *);
  f->buffer.bufferSize = va_arg(*args, integer); /* set up the buffer */
  f->io.mode = va_arg(*args, ioDirection); /* set up the access mode */
  f->buffer.resizeable = va_arg(*args, logical); /* is this buffer resizeable? */
  if (isReadingFile(O_IO(f)) && !isWritingFile(O_IO(f)))
    f->buffer.size = f->buffer.bufferSize;
  else
    f->buffer.size = 0;
}

static void BufferDestroy(objectPo o) {
  strBufferPo str = O_BUFFER(o);
  if (str->buffer.resizeable)
    free(str->buffer.buffer);
}

// Implement class buffer functions

static retCode bufferInBytes(ioPo io, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  strBufferPo f = O_BUFFER(io);

  while (remaining > 0) {
    if (f->buffer.in_pos >= f->buffer.size) {
      if (remaining == count)
        ret = Eof;
      break;
    } else {
      *ch++ = (byte) f->buffer.buffer[f->buffer.in_pos++];
      remaining--;
    }
  }
  *actual = count - remaining;

  return ret;
}

static retCode ensureSpace(strBufferPo f, integer count) {
  if (f->buffer.out_pos + count >= f->buffer.bufferSize) {
    if (f->buffer.resizeable) {
      integer nlen = f->buffer.bufferSize + (f->buffer.bufferSize >> 1) + count; /* allow for some growth */
      char *nbuff = realloc(f->buffer.buffer, sizeof(char) * nlen);
      if (nbuff != NULL) {
        f->buffer.buffer = nbuff;
        f->buffer.bufferSize = nlen;
      } else
        syserr("could not allocate more space for buffer");
    } else {
      return Fail;
    }
  }
  return Ok;
}

static retCode bufferOutBytes(ioPo io, byte *b, integer count, integer *actual) {
  strBufferPo f = O_BUFFER(io);

  retCode ret = ensureSpace(f, count);

  for (int ix = 0; ret == Ok && ix < count; ix++)
    f->buffer.buffer[f->buffer.out_pos++] = b[ix];
  f->buffer.size = f->buffer.out_pos;
  *actual = count;
  return ret;
}

static retCode bufferBackByte(ioPo io, byte b) {
  strBufferPo f = O_BUFFER(io);

  if (f->buffer.in_pos > 0) {
    f->buffer.buffer[--f->buffer.in_pos] = b;
    return Ok;
  } else
    return Error;
}

static retCode bufferAtEof(ioPo io) {
  strBufferPo f = O_BUFFER(io);

  if (f->buffer.out_pos < f->buffer.size)
    return Ok;
  else
    return Eof;
}

static retCode bufferFlusher(ioPo io, long count) {
  return Ok;
}

static retCode bufferClose(ioPo io) {
  destroyObject(O_OBJECT(io)); /* this will get rid of all the buffer objects attributes */
  return Ok;
}

retCode clearStrBuffer(strBufferPo b) {
  b->buffer.in_pos = 0;
  b->buffer.out_pos = 0;
  b->io.inBpos = b->io.inCpos = 0;
  b->buffer.size = 0;
  return Ok;
}

static integer bufferNo = 0;

strBufferPo newStringBuffer() {
  char name[MAX_SYMB_LEN];

  strMsg(name, NumberOf(name), "<buffer%d>", bufferNo++);

  byte *buffer = (byte *) malloc(sizeof(byte) * 128);

  return O_BUFFER(newObject(strBufferClass, name, utf8Encoding, buffer, 128, ioWRITE, True));
}

strBufferPo newReadStringBuffer(char *text, integer textLen) {
  char name[MAX_SYMB_LEN];

  strMsg(name, NumberOf(name), "<buffer%d>", bufferNo++);

  return O_BUFFER(newObject(strBufferClass, name, utf8Encoding, text, textLen, ioREAD, False));
}

strBufferPo newIoStringBuffer() {
  char name[MAX_SYMB_LEN];

  strMsg(name, NumberOf(name), "<buffer%d>", bufferNo++);

  byte *buffer = (byte *) malloc(sizeof(byte) * 128);

  return O_BUFFER(newObject(strBufferClass, name, utf8Encoding, buffer, 128, ioWRITE | ioREAD, True));
}

strBufferPo fixedStringBuffer(char *buffer, long len) {
  char *name = "buffer";
  return O_BUFFER(newObject(strBufferClass, name, utf8Encoding, buffer, len, ioWRITE, False));
}

char *getTextFromBuffer(strBufferPo s, integer *len) {
  *len = s->buffer.size;

  ensureSpace(s, 1);
  s->buffer.buffer[s->buffer.size] = '\0';

  return s->buffer.buffer;
}

retCode readTextFromBuffer(strBufferPo b, char *tgt, integer len){
  if(len<=b->buffer.size)
    return Error;
  else{
    for(integer ix=0;ix<b->buffer.size;ix++){
      tgt[ix] = b->buffer.buffer[ix];
    }
    tgt[b->buffer.size] = '\0';
    return Ok;
  }
}

logical isTrivialBuffer(strBufferPo b) {
  return uniIsTrivial(b->buffer.buffer, b->buffer.size);
}

integer strBufferLength(strBufferPo b) {
  return b->buffer.size;
}

integer strBufferOutPos(strBufferPo b) {
  return b->buffer.out_pos;
}

integer strBufferBumpOutPos(strBufferPo b, integer incr) {
  b->buffer.out_pos = clamp(0, b->buffer.out_pos + incr, b->buffer.size);
  return b->buffer.out_pos;
}

retCode rewindStrBuffer(strBufferPo b) {
  b->buffer.in_pos = 0;
  b->io.inBpos = b->io.inCpos = 0;

  b->buffer.out_pos = 0;
  return Ok;
}

retCode seekStrBuffer(strBufferPo b, integer pos) {
  pos = clamp(0, pos, b->buffer.size);
  b->buffer.in_pos = b->buffer.out_pos = pos;
  return Ok;
}

retCode insertIntoStringBuffer(strBufferPo b, codePoint ch) {
  integer chSize = codePointSize(ch);
  integer insertPos = b->buffer.out_pos;

  ensureSpace(b, chSize);

  for (integer ix = b->buffer.size + chSize; ix > insertPos; ix--) {
    b->buffer.buffer[ix] = b->buffer.buffer[ix - chSize];
  }
  appendCodePoint(b->buffer.buffer, &insertPos, b->buffer.bufferSize, ch);
  b->buffer.out_pos += chSize;
  b->buffer.size += chSize;
  return Ok;
}

retCode appendIntoStrBuffer(strBufferPo b, const char *text, integer txtLen) {
  ensureSpace(b, txtLen);
  for (integer ix = 0; ix < txtLen; ix++)
    b->buffer.buffer[b->buffer.out_pos++] = text[ix];
  b->buffer.size += txtLen;

  return Ok;
}

retCode appendToStrBuffer(strBufferPo b, const char *text, integer txtLen) {
  ensureSpace(b, txtLen);
  for (integer ix = 0; ix < txtLen; ix++)
    b->buffer.buffer[b->buffer.size++] = text[ix];

  return Ok;
}

retCode appendCodePointToStrBuffer(strBufferPo b, codePoint ch){
  ensureSpace(b,5);
  return appendCodePoint(b->buffer.buffer,&b->buffer.out_pos,b->buffer.bufferSize,ch);
}

retCode twizzleStrBuffer(strBufferPo b, integer pos) {
  if (pos < 0 || pos >= b->buffer.size - 1)
    return Error;
  else {
    char ch = b->buffer.buffer[pos];
    b->buffer.buffer[pos] = b->buffer.buffer[pos + 1];
    b->buffer.buffer[pos + 1] = ch;
    b->buffer.out_pos++;
    return Ok;
  }
}

retCode stringIntoStrBuffer(strBufferPo b, strgPo str) {
  return appendIntoStrBuffer(b, strgVal(str), strgLen(str));
}

retCode deleteFromStrBuffer(strBufferPo b, integer len) {
  integer endPt = strBufferOutPos(b);
  integer from = strBufferOutPos(b);

  if (len > 0) {
    endPt = advanceCodePoint(b->buffer.buffer, endPt, strBufferLength(b), len);

    integer remaining = strBufferLength(b) - endPt;

    for (integer px = 0; px < remaining; px++)
      b->buffer.buffer[from + px] = b->buffer.buffer[endPt + px];

    b->buffer.size -= (endPt - from);
  } else {
    integer tgtPt = backCodePoint(b->buffer.buffer, endPt, -len);
    integer remaining = strBufferLength(b) - from;

    for (integer px = 0; px < remaining; px++)
      b->buffer.buffer[tgtPt + px] = b->buffer.buffer[from + px];
    const integer delta = from - tgtPt;
    b->buffer.size -= delta;
    b->buffer.out_pos -= delta;
  }

  b->buffer.out_pos = clamp(0, b->buffer.out_pos, b->buffer.size);
  return Ok;
}

strgPo stringFromBuffer(strBufferPo b) {
  return newStrng(b->buffer.size, b->buffer.buffer);
}

retCode showStringBuffer(ioPo f, void *data, long depth, long precision, logical alt) {
  strBufferPo b = O_BUFFER(data);
  integer bufLen;
  char *content = getTextFromBuffer(b, &bufLen);

  return outMsg(f, "«%S»", content,bufLen);
}
