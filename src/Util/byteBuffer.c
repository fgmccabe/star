/*
 Byte Buffer implementation
 Copyright (c) 2016, 2017, 2020. Francis G. McCabe
 */

#include "byteBufferP.h"

#include <stdlib.h>
#include <starOptions.h>
#include <ooio.h>

/* Set up the buffer file class */

static void initByteBufferClass(classPo class, classPo req);

static void byteBufferDestroy(objectPo o);

static void byteBufferInit(objectPo o, va_list *args);

static retCode byteBufferInBytes(ioPo io, byte *ch, integer count, integer *actual);

static retCode byteBufferOutBytes(ioPo io, byte *b, integer count, integer *actual);

static retCode byteBufferBackByte(ioPo io, byte b);

static retCode byteBufferAtEof(ioPo io);

static retCode byteBufferFlusher(ioPo io, long count);

static retCode byteBufferClose(ioPo io);

ByteBufferClassRec ByteBufferClass = {
  {(classPo) &IoClass,                    /* parent class is io object */
    "bytes",                         /* this is the byte buffer class */
    initByteBufferClass,                      /* Buffer class initializer */
    O_INHERIT_DEF,                        /* Buffer object element creation */
    byteBufferDestroy,                        /* Buffer objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    byteBufferInit,                           /* initialization of a buffer object */
    sizeof(ByteBufferObject),                 /* size of a buffer object */
    NULL,                                 /* pool of buffer values */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,                    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {byteBufferInBytes,                         /* inByte  */
    byteBufferOutBytes,                       /* outBytes  */
    byteBufferBackByte,                       /* backByte */
    byteBufferAtEof,                          /* at end of file? */
    byteBufferFlusher,                        /* flush  */
    byteBufferClose                           /* close  */
  }};

classPo byteBufferClass = (classPo) &ByteBufferClass;

static void initByteBufferClass(classPo class, classPo req) {
}

// IO initialization should already be done at this point
static void byteBufferInit(objectPo o, va_list *args) {
  byteBufferPo f = O_BYTEBUFFER(o);

  // Set up the buffer pointers
  f->buffer.in_pos = 0;
  f->buffer.out_pos = 0;
  setEncoding(O_IO(f), rawEncoding); /* set up the encoding */
  f->buffer.buffer = va_arg(*args, char *);
  f->buffer.bufferSize = va_arg(*args, integer); /* set up the buffer */
  f->io.mode = va_arg(*args, ioDirection); /* set up the access mode */
  f->buffer.resizeable = va_arg(*args, logical); /* is this buffer resizeable? */
  if (isReadingFile(O_IO(f)) && !isWritingFile(O_IO(f)))
    f->buffer.size = f->buffer.bufferSize;
  else
    f->buffer.size = 0;
}

static void byteBufferDestroy(objectPo o) {
  byteBufferPo bfr = O_BYTEBUFFER(o);
  if (bfr->buffer.resizeable && bfr->buffer.buffer != Null)
    free(bfr->buffer.buffer);
}

// Implement class buffer functions

static retCode byteBufferInBytes(ioPo io, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  byteBufferPo f = O_BYTEBUFFER(io);

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

static retCode ensureSpace(byteBufferPo f, integer count) {
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

static retCode byteBufferOutBytes(ioPo io, byte *b, integer count, integer *actual) {
  byteBufferPo f = O_BYTEBUFFER(io);

  retCode ret = ensureSpace(f, count);

  for (int ix = 0; ret == Ok && ix < count; ix++)
    f->buffer.buffer[f->buffer.out_pos++] = b[ix];
  f->buffer.size = f->buffer.out_pos;
  *actual = count;
  return ret;
}

static retCode byteBufferBackByte(ioPo io, byte b) {
  byteBufferPo f = O_BYTEBUFFER(io);

  if (f->buffer.in_pos > 0) {
    f->buffer.buffer[--f->buffer.in_pos] = b;
    return Ok;
  } else
    return Error;
}

static retCode byteBufferAtEof(ioPo io) {
  byteBufferPo f = O_BYTEBUFFER(io);

  if (f->buffer.out_pos < f->buffer.size)
    return Ok;
  else
    return Eof;
}

static retCode byteBufferFlusher(ioPo io, long count) {
  return Ok;
}

static retCode byteBufferClose(ioPo io) {
  destroyObject(O_OBJECT(io)); /* this will get rid of all the buffer objects attributes */
  return Ok;
}

retCode clearByteBuffer(byteBufferPo b) {
  b->buffer.in_pos = 0;
  b->buffer.out_pos = 0;
  b->io.inBpos = b->io.inCpos = 0;
  b->buffer.size = 0;
  return Ok;
}

static integer bufferNo = 0;

byteBufferPo newByteBuffer() {
  char name[MAX_SYMB_LEN];

  strMsg(name, NumberOf(name), "<buffer%d>", bufferNo++);

  byte *buffer = (byte *) malloc(sizeof(byte) * 128);

  return O_BYTEBUFFER(newObject(byteBufferClass, name, buffer, 128, ioWRITE, True));
}

byteBufferPo fixedByteBuffer(char *buffer, long len) {
  char *name = "buffer";
  return O_BYTEBUFFER(newObject(byteBufferClass, name, buffer, len, ioWRITE, False));
}

char *getBytesFromBuffer(byteBufferPo s, integer *len) {
  *len = s->buffer.size;
  return s->buffer.buffer;
}

integer byteBufferLength(byteBufferPo b) {
  return b->buffer.size;
}

integer byteBufferOutPos(byteBufferPo b) {
  return b->buffer.out_pos;
}

retCode rewindByteBuffer(byteBufferPo b) {
  b->buffer.in_pos = 0;
  b->io.inBpos = b->io.inCpos = 0;

  b->buffer.out_pos = 0;
  return Ok;
}

retCode appendIntoByteBuffer(byteBufferPo b, const char *text, integer txtLen) {
  ensureSpace(b, txtLen);
  for (integer ix = 0; ix < txtLen; ix++)
    b->buffer.buffer[b->buffer.out_pos++] = text[ix];
  b->buffer.size += txtLen;

  return Ok;
}

retCode appendToByteBuffer(byteBufferPo b, const char *text, integer txtLen) {
  ensureSpace(b, txtLen);
  for (integer ix = 0; ix < txtLen; ix++)
    b->buffer.buffer[b->buffer.size++] = text[ix];

  return Ok;
}

retCode appendByteToBuffer(byteBufferPo b, byte byte) {
  ensureSpace(b, 1);
  b->buffer.buffer[b->buffer.size++] = byte;
  return Ok;
}

retCode showByteBuffer(ioPo f, void *data, long depth, long precision, logical alt) {
  byteBufferPo b = O_BYTEBUFFER(data);
  integer bufLen;
  char *content = getBytesFromBuffer(b, &bufLen);

  return outMsg(f, "«%S»", content, bufLen);
}
