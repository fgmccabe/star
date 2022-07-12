/*
 Word Buffer implementation
 Copyright (c) 2016, 2017, 2020. Francis G. McCabe
 */

#include "wordBufferP.h"

#include <stdlib.h>
#include <starOptions.h>
#include <ooio.h>
#include <string.h>
#include <assert.h>

/* Set up the chunked buffer file class */

static void initWordBufferClass(classPo class, classPo req);

static void wordBufferDestroy(objectPo o);

static void wordBufferInit(objectPo o, va_list *args);

static retCode wordBufferInWords(wordBufferPo buffer, byte *ch, integer count, integer *actual);

static retCode wordBufferBackWord(ioPo io, byte b);

static retCode wordBufferAtEof(ioPo io);

WordBufferClassRec WordBufferClass = {
  {(classPo) &ObjectClass,                    /* parent class is object */
    "words",                         /* this is the word buffer class */
    initWordBufferClass,                      /* Buffer class initializer */
    O_INHERIT_DEF,
    O_INHERIT_DEF,                        /* Buffer object element creation */
    wordBufferDestroy,                        /* Buffer objectdestruction */
    O_INHERIT_DEF,                        /* erasure */
    wordBufferInit,                           /* initialization of a buffer object */
    sizeof(WordBufferObject),                 /* size of a buffer object */
    NULL,                                 /* pool of buffer values */
    O_INHERIT_DEF,                        // No special hash function
    O_INHERIT_DEF,                        // No special equality
    PTHREAD_ONCE_INIT,                    /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo wordBufferClass = (classPo) &WordBufferClass;

static void initWordBufferClass(classPo class, classPo req) {
}

size_t grainSize(bufferGrain grain) {
  switch (grain) {
    case shortGrain:
      return sizeof(int16);
    case wordGrain:
      return sizeof(int32);
    case longGrain:
      return sizeof(int64);
  }
}

// IO initialization should already be done at this point
static void wordBufferInit(objectPo o, va_list *args) {
  wordBufferPo f = O_WORDBUFFER(o);

  // Set up the buffer pointers
  f->buffer.in_pos = 0;
  f->buffer.out_pos = 0;
  f->buffer.buffer = va_arg(*args, byte *);
  f->buffer.bufferSize = va_arg(*args, integer); /* set up the buffer */
  f->buffer.grain = va_arg(*args, bufferGrain);   // Grain of the buffer
  f->buffer.mode = va_arg(*args, ioDirection); /* set up the access mode */
  f->buffer.resizeable = va_arg(*args, logical); /* is this buffer resizeable? */
  if (f->buffer.mode == ioREAD)
    f->buffer.size = f->buffer.bufferSize;
  else
    f->buffer.size = 0;
}

static void wordBufferDestroy(objectPo o) {
  wordBufferPo bfr = O_WORDBUFFER(o);
  if (bfr->buffer.resizeable && bfr->buffer.buffer != Null)
    free(bfr->buffer.buffer);
}

// Implement class buffer functions

static retCode wordBufferInWords(wordBufferPo buffer, byte *ch, integer count, integer *actual) {
  retCode ret = Ok;
  integer remaining = count;

  while (remaining > 0) {
    if (buffer->buffer.in_pos >= buffer->buffer.size) {
      if (remaining == count)
        ret = Eof;
      break;
    } else {
      switch (buffer->buffer.grain) {
        case shortGrain: {
          int16 *inter = (int16 *) ch;
          *inter++ = ((int16 *) buffer->buffer.buffer)[buffer->buffer.in_pos++];
          ch = (byte *) inter;
          break;
        }
        case wordGrain: {
          int32 *inter = (int32 *) ch;
          *inter++ = ((int32 *) buffer->buffer.buffer)[buffer->buffer.in_pos++];
          ch = (byte *) inter;
        }
        case longGrain: {
          int64 *inter = (int64 *) ch;
          *inter++ = ((int64 *) buffer->buffer.buffer)[buffer->buffer.in_pos++];
          ch = (byte *) inter;
        }
        default:
          return Error;
      }
      remaining--;
    }
  }
  *actual = count - remaining;

  return ret;
}

static retCode ensureSpace(wordBufferPo f, integer count) {
  if (f->buffer.out_pos + count >= f->buffer.bufferSize) {
    if (f->buffer.resizeable) {
      integer nlen = f->buffer.bufferSize + (f->buffer.bufferSize >> 1u) + count; /* allow for some growth */
      byte *nbuff = realloc(f->buffer.buffer, grainSize(f->buffer.grain) * nlen);
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

static retCode wordBufferAtEof(ioPo io) {
  wordBufferPo f = O_WORDBUFFER(io);

  if (f->buffer.out_pos < f->buffer.size)
    return Ok;
  else
    return Eof;
}

retCode closeWordBuffer(wordBufferPo buffer) {
  decReference(O_OBJECT(buffer)); /* this will get rid of all the buffer objects attributes */
  return Ok;
}

static integer bufferNo = 0;

wordBufferPo newWordBuffer(bufferGrain grain) {
  byte *buffer = (byte *) malloc(grainSize(grain) * 128);

  return O_WORDBUFFER(newObject(wordBufferClass, buffer, 128, grain, ioWRITE, True));
}

wordBufferPo fixedWordBuffer(char *buffer, long len, bufferGrain grain) {
  return O_WORDBUFFER(newObject(wordBufferClass, buffer, len, grain, ioWRITE, False));
}

byte *getCurrentBufferData(wordBufferPo s, integer *len) {
  *len = s->buffer.out_pos;
  return s->buffer.buffer;
}

byte *getBufferData(wordBufferPo b, integer *len) {
  byte *current = getCurrentBufferData(b, len);
  byte *data = malloc(grainSize(b->buffer.grain) * b->buffer.out_pos);
  memmove(data, current, grainSize(b->buffer.grain) * (*len));
  return data;
}

retCode reserveBufferSpace(wordBufferPo b, integer len, integer *pos) {
  ensureSpace(b, len);
  *pos = b->buffer.out_pos;
  b->buffer.out_pos += len;
  return Ok;
}

retCode appendWordToBuffer(wordBufferPo b, integer word) {
  ensureSpace(b, 1);
  switch (b->buffer.grain) {
    case shortGrain:
      ((int16 *) b->buffer.buffer)[b->buffer.out_pos++] = (int16) word;
      return Ok;
    case wordGrain:
      ((int32 *) b->buffer.buffer)[b->buffer.out_pos++] = (int32) word;
      return Ok;
    case longGrain:
      ((int64 *) b->buffer.buffer)[b->buffer.out_pos++] = (int64) word;
      return Ok;
  }
  return Error;
}

retCode writeIntoBuffer(wordBufferPo b, integer pos, integer data) {
  assert(pos >= 0 && pos < b->buffer.out_pos);

  switch (b->buffer.grain) {
    case shortGrain:
      ((int16 *) b->buffer.buffer)[pos] = (int16) data;
      return Ok;
    case wordGrain:
      ((int32 *) b->buffer.buffer)[pos] = (int32) data;
      return Ok;
    case longGrain:
      ((int64 *) b->buffer.buffer)[pos] = (int64) data;
      return Ok;
  }
}
