//
// Byte buffer, an IO device oriented to reading/writing bytes
// Created by Francis McCabe on 2/3/24.
//

#include "byteBufferP.h"

#include <stdlib.h>
#include <string.h>
#include <starOptions.h>
#include <ooio.h>

/* Set up the buffer file class */

static void initBufferClass(classPo class, classPo req);
static void BufferDestroy(objectPo o);
static void BufferInit(objectPo o, va_list *args);
static retCode bufferInBytes(ioPo io, byte *ch, integer count, integer *actual);
static retCode bufferOutBytes(ioPo io, byte *b, integer count, integer *actual);
static retCode bufferBackByte(ioPo io, byte b);
static retCode bufferInputReady(ioPo io, integer count);
static retCode bufferOutputReady(ioPo io, integer count);
static retCode bufferAtEof(ioPo io);
static retCode bufferClose(ioPo io);
static integer bufferPos(ioPo io);
static retCode bufferSeek(ioPo io, integer pos);

ByteBufferClassRec ByteBufferClass = {
  .objectPart = {
    .parent = (classPo) &IoClass,                    /* parent class is io object */
    .className = "bytes",                             /* this is the buffer class */
    .classInit = initBufferClass,                      /* Buffer class initializer */
    .classInherit = O_INHERIT_DEF,
    .create = O_INHERIT_DEF,                        /* Buffer object element creation */
    .destroy = BufferDestroy,                        /* Buffer objectdestruction */
    .erase = O_INHERIT_DEF,                        /* erasure */
    .init = BufferInit,                           /* initialization of a buffer object */
    .size = sizeof(BufferObject),                 /* size of a buffer object */
    .pool = NULL,                                 /* pool of buffer values */
    .hashCode = O_INHERIT_DEF,                        // No special hash function
    .equality = O_INHERIT_DEF,                        // No special equality
    .inited = PTHREAD_ONCE_INIT,                    /* not yet initialized */
    .mutex = PTHREAD_MUTEX_INITIALIZER
  },
  .lockPart = {},
  .ioPart = {
    .read =bufferInBytes,                           /* inByte  */
    .write = bufferOutBytes,                        /* outBytes  */
    .backByte = bufferBackByte,                     /* backByte */
    .inputReady = bufferInputReady,
    .outputReady = bufferOutputReady,
    .isAtEof = bufferAtEof,                         /* at end of file? */
    .close = bufferClose,                           /* close  */
    .position = bufferPos,                          // Report position in buffer
    .seek = bufferSeek,                             // Move buffer position
  }};

classPo byteBufferClass = (classPo) &ByteBufferClass;

static void initBufferClass(classPo class, classPo req) {
}

logical isAByteBuffer(objectPo o) {
  return objectHasClass(o, byteBufferClass);
}

// IO initialization should already be done at this point
static void BufferInit(objectPo o, va_list *args) {
  byteBufferPo f = O_BYTEBUFFER(o);

  // Set up the buffer pointers
  f->buffer.pos = 0;
  f->buffer.buffer = va_arg(*args, byte *);
  f->buffer.bufferSize = va_arg(*args, integer); /* set up the buffer */
  f->io.mode = va_arg(*args, ioDirection); /* set up the access mode */
  f->buffer.resizeable = va_arg(*args, logical); /* is this buffer resizeable? */
  if (f->buffer.buffer == Null && f->buffer.bufferSize == 0 && f->buffer.resizeable) {
    f->buffer.buffer = f->buffer.line;
    f->buffer.bufferSize = NumberOf(f->buffer.line);
  }

  if (isReadingFile(O_IO(f)) && !isWritingFile(O_IO(f)))
    f->buffer.size = f->buffer.bufferSize;
  else
    f->buffer.size = 0;
}

static void BufferDestroy(objectPo o) {
  byteBufferPo str = O_BYTEBUFFER(o);
  if (str->buffer.resizeable && str->buffer.buffer != str->buffer.line)
    free(str->buffer.buffer);
}

// Implement class buffer functions


retCode bufferInputReady(ioPo io, integer count) {
  if (isReadingFile(io) && isAStringBuffer(O_OBJECT(io))) {
    byteBufferPo str = O_BYTEBUFFER(io);

    if (str->buffer.pos + count <= str->buffer.size)
      return Ok;
    else
      return Fail;
  } else
    return Error;
}

retCode bufferOutputReady(ioPo io, integer count) {
  if (isWritingFile(io) && isAStringBuffer(O_OBJECT(io))) {
    byteBufferPo str = O_BYTEBUFFER(io);

    if (str->buffer.resizeable)
      return Ok;
    else if (str->buffer.pos + count <= str->buffer.bufferSize)
      return Ok;
    else
      return Fail;
  } else
    return Error;
}

static retCode bufferInBytes(ioPo io, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  byteBufferPo f = O_BYTEBUFFER(io);

  while (remaining > 0) {
    if (f->buffer.pos >= f->buffer.size) {
      if (remaining == count)
        ret = Eof;
      break;
    } else {
      *ch++ = (byte) f->buffer.buffer[f->buffer.pos++];
      remaining--;
    }
  }
  *actual = count - remaining;

  return ret;
}

static retCode ensureSpace(byteBufferPo f, integer count) {
  if (f->buffer.pos + count >= f->buffer.bufferSize) {
    if (f->buffer.resizeable) {
      integer nlen = f->buffer.bufferSize + (f->buffer.bufferSize >> 1) + count; /* allow for some growth */

      byte *nbuff;

      if (f->buffer.buffer == f->buffer.line) {
        nbuff = malloc(sizeof(char) * nlen);
        memcpy(nbuff, f->buffer.line, f->buffer.bufferSize);
      } else
        nbuff = realloc(f->buffer.buffer, sizeof(char) * nlen);

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
  byteBufferPo f = O_BYTEBUFFER(io);

  retCode ret = ensureSpace(f, count);

  for (int ix = 0; ret == Ok && ix < count; ix++)
    f->buffer.buffer[f->buffer.pos++] = b[ix];
  f->buffer.size = f->buffer.pos;
  *actual = count;
  return ret;
}

static retCode bufferBackByte(ioPo io, byte b) {
  byteBufferPo f = O_BYTEBUFFER(io);

  if (f->buffer.pos > 0) {
    f->buffer.buffer[--f->buffer.pos] = b;
    return Ok;
  } else
    return Error;
}

static retCode bufferAtEof(ioPo io) {
  byteBufferPo f = O_BYTEBUFFER(io);

  if (f->buffer.pos < f->buffer.size)
    return Ok;
  else
    return Eof;
}

static retCode bufferClose(ioPo io) {
  decReference(O_OBJECT(io)); /* this will get rid of all the buffer objects attributes */
  return Ok;
}

static integer bufferPos(ioPo io) {
  byteBufferPo f = O_BYTEBUFFER(io);
  return f->buffer.pos;
}

static retCode bufferSeek(ioPo io, integer pos){
  byteBufferPo f = O_BYTEBUFFER(io);
  if(pos<0 || pos>f->buffer.size)
    return Error;
  else{
    f->buffer.pos = pos;
    if(isWritingFile(io))
      f->buffer.size = pos;
    return Ok;
  }
}

retCode clearByteBuffer(byteBufferPo b) {
  b->buffer.pos = 0;
  b->buffer.size = 0;
  return Ok;
}

static integer bufferNo = 0;

byteBufferPo newByteBuffer() {
  return O_BYTEBUFFER(newObject(byteBufferClass, "buffer", Null, 0, ioWRITE, True));
}

byteBufferPo newReadByteBuffer(byte *text, integer textLen) {
  char name[MAX_SYMB_LEN];

  strMsg(name, NumberOf(name), "<buffer%d>", bufferNo++);

  return O_BYTEBUFFER(newObject(byteBufferClass, name, text, textLen, ioREAD, False));
}

byteBufferPo newIoByteBuffer() {
  char name[MAX_SYMB_LEN];

  strMsg(name, NumberOf(name), "<buffer%d>", bufferNo++);

  byte *buffer = (byte *) malloc(sizeof(byte) * 128);

  return O_BYTEBUFFER(newObject(byteBufferClass, name, buffer, 128, ioWRITE | ioREAD, True));
}

byteBufferPo fixedByteBuffer(byte *buffer, long len) {
  char *name = "buffer";
  return O_BYTEBUFFER(newObject(byteBufferClass, name, buffer, len, ioWRITE, False));
}

byte *getBytesFromBuffer(byteBufferPo s, integer *len) {
  *len = s->buffer.size;

  ensureSpace(s, 1);
  s->buffer.buffer[s->buffer.size] = '\0';

  return s->buffer.buffer;
}

integer byteBufferLength(byteBufferPo b) {
  return b->buffer.size;
}

retCode appendByteToBuffer(byteBufferPo b, byte by) {
  ensureSpace(b, 1);

  b->buffer.buffer[b->buffer.pos++] = by;
  b->buffer.size++;
  return Ok;
}

retCode showByteBuffer(ioPo f, void *data, long depth, long precision, logical alt) {
  byteBufferPo b = O_BYTEBUFFER(data);

  return outMsg(f, "«%S»", b->buffer.buffer, b->buffer.pos);
}
