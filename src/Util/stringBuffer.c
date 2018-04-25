/*
 Buffer implementation
 Copyright (c) 2016, 2017. Francis G. McCabe

 Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the
 License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied. See the License for the specific language governing
 permissions and limitations under the License.
 */

#include "stringBufferP.h"

#include <stdlib.h>
#include <assert.h>

/* Set up the buffer file class */

static void initBufferClass(classPo class, classPo req);

static void BufferDestroy(objectPo o);

static void BufferInit(objectPo o, va_list *args);

static retCode bufferInBytes(ioPo io, byte *ch, integer count, integer *actual);

static retCode bufferOutBytes(ioPo f, byte *b, integer count, integer *actual);

static retCode bufferBackByte(ioPo f, byte b);

static retCode bufferAtEof(ioPo io);

static retCode bufferInReady(ioPo f);

static retCode bufferOutReady(ioPo f);

static retCode bufferFlusher(ioPo f, long count);

static retCode bufferSeek(ioPo f, integer count);

static retCode bufferClose(ioPo f);

static retCode bufferMark(ioPo f, integer *mark);

static retCode bufferReset(ioPo f, integer mark);

BufferClassRec BufferClass = {
  {(classPo) &IoClass, /* parent class is io object */
    "buffer", /* this is the buffer class */
    NULL,
    initBufferClass, /* Buffer class initializer */
    O_INHERIT_DEF, /* Buffer object element creation */
    BufferDestroy, /* Buffer objectdestruction */
    O_INHERIT_DEF, /* erasure */
    BufferInit, /* initialization of a buffer object */
    sizeof(BufferObject), /* size of a buffer object */
    NULL, /* pool of buffer values */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT, /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {},
  {bufferInBytes, /* inByte  */
    bufferOutBytes, /* outBytes  */
    bufferBackByte, /* backByte */
    bufferMark,       /* Mark the buffer */
    bufferReset,      /* Reset to a mark */
    bufferAtEof, /* at end of file? */
    bufferInReady, /* readyIn  */
    bufferOutReady, /* readyOut  */
    bufferFlusher, /* flush  */
    bufferSeek, /* seek  */
    bufferClose /* close  */
  }};

classPo bufferClass = (classPo) &BufferClass;

static void initBufferClass(classPo class, classPo req) {
}

// IO initialization should already be done at this point
static void BufferInit(objectPo o, va_list *args) {
  bufferPo f = O_BUFFER(o);

  // Set up the buffer pointers
  f->buffer.pos = 0;
  setEncoding(O_IO(f), va_arg(*args, ioEncoding)); /* set up the encoding */
  f->buffer.buffer = va_arg(*args, char *);
  f->buffer.bufferSize = va_arg(*args, integer); /* set up the buffer */
  f->io.mode = va_arg(*args, ioDirection); /* set up the access mode */
  f->buffer.resizeable = va_arg(*args, logical); /* is this buffer resizeable? */
  if (isReadingFile(O_IO(f)))
    f->buffer.size = f->buffer.bufferSize;
  else
    f->buffer.size = 0;
}

static void BufferDestroy(objectPo o) {
  bufferPo str = O_BUFFER(o);
  if (str->buffer.resizeable)
    free(str->buffer.buffer);
}

// Implement class buffer functions

static retCode bufferSeek(ioPo io, integer count) {
  bufferPo f = O_BUFFER(io);

  if (count < f->buffer.pos) {
    f->buffer.pos = count;
    return Ok;
  } else
    return Fail;
}

static retCode bufferInBytes(ioPo io, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;
  bufferPo f = O_BUFFER(io);

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

static retCode ensureSpace(bufferPo f, integer count) {
  assert(isFileOpen(O_IO(f))==Ok);
  if (f->buffer.pos + count >= f->buffer.bufferSize) {
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
  bufferPo f = O_BUFFER(io);

  retCode ret = ensureSpace(f, count);

  for (int ix = 0; ret == Ok && ix < count; ix++)
    f->buffer.buffer[f->buffer.pos++] = b[ix];
  f->buffer.size = f->buffer.pos;
  *actual = count;
  return ret;
}

static retCode bufferBackByte(ioPo io, byte b) {
  bufferPo f = O_BUFFER(io);

  if (f->buffer.pos > 0) {
    f->buffer.buffer[--f->buffer.pos] = b;
    return Ok;
  } else
    return Error;
}

static retCode bufferMark(ioPo io, integer *mark) {
  bufferPo f = O_BUFFER(io);

  *mark = f->buffer.pos;
  return Ok;
}

static retCode bufferReset(ioPo io, integer mark) {
  bufferPo f = O_BUFFER(io);

  if (f->buffer.pos > 0) {
    if (mark < f->buffer.size) {
      f->buffer.pos = mark;
      return Ok;
    } else
      return Fail;
  } else
    return Error;
}

static retCode bufferAtEof(ioPo io) {
  bufferPo f = O_BUFFER(io);

  if (f->buffer.pos < f->buffer.size)
    return Ok;
  else
    return Eof;
}

static retCode bufferInReady(ioPo io) {
  bufferPo f = O_BUFFER(io);

  if (f->buffer.pos < f->buffer.size)
    return Ok;
  else
    return Eof;
}

static retCode bufferOutReady(ioPo io) {
  bufferPo f = O_BUFFER(io);

  if (f->buffer.pos < f->buffer.bufferSize)
    return Ok;
  else {
    if (f->buffer.resizeable)
      return Ok;
    else
      return Fail;
  }
}

static retCode bufferFlusher(ioPo io, long count) {
  return Ok;
}

static retCode bufferClose(ioPo io) {
  destroyObject(O_OBJECT(io)); /* this will get rid of all the buffer objects attributes */
  return Ok;
}

retCode clearBuffer(bufferPo in) {
  in->buffer.pos = 0;
  in->io.inBpos = in->io.inCpos = 0;
  in->buffer.size = 0;
  return Ok;
}

retCode bufferStepForward(bufferPo in, long cnt) {
  if (in->buffer.pos + cnt < in->buffer.size) {
    in->buffer.pos += cnt;
    return Ok;
  } else
    return Error;
}

bufferPo newStringBuffer() {
  byte name[] = {'<', 'b', 'u', 'f', 'f', 'e', 'r', '>', 0};
  byte *buffer = (byte *) malloc(sizeof(byte) * 128);

  return O_BUFFER(newObject(bufferClass, name, utf8Encoding, buffer, 128, ioWRITE, True));
}

bufferPo fixedStringBuffer(char *buffer, long len) {
  byte name[] = {'<', 'b', 'u', 'f', 'f', 'e', 'r', '>', 0};
  return O_BUFFER(newObject(bufferClass, name, utf8Encoding, buffer, len, ioWRITE, False));
}

char *getTextFromBuffer(integer *actual, bufferPo s) {
  *actual = s->buffer.pos;

  ensureSpace(s, 1);
  s->buffer.buffer[s->buffer.pos] = '\0';

  return s->buffer.buffer;
}

long bufferSize(bufferPo s) {
  return s->buffer.size;
}

retCode rewindBuffer(bufferPo in) {
  in->buffer.pos = 0;
  in->io.inBpos = in->io.inCpos = 0;
  return Ok;
}
