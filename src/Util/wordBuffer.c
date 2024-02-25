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

WordBufferClassRec WordBufferClass = {
  .objectPart = {
    .parent = (classPo) &ObjectClass,                    /* parent class is object */
    .className = "words",                         /* this is the word buffer class */
    .classInit = initWordBufferClass,                      /* Buffer class initializer */
    .classInherit = O_INHERIT_DEF,
    .create = O_INHERIT_DEF,                        /* Buffer object element creation */
    .destroy = wordBufferDestroy,                        /* Buffer objectdestruction */
    .erase = O_INHERIT_DEF,                        /* erasure */
    .init = wordBufferInit,                           /* initialization of a buffer object */
    .size = sizeof(WordBufferObject),                 /* size of a buffer object */
    .pool = NULL,                                 /* pool of buffer values */
    .hashCode = O_INHERIT_DEF,                        // No special hash function
    .equality = O_INHERIT_DEF,                        // No special equality
    .inited = PTHREAD_ONCE_INIT,                    /* not yet initialized */
    .mutex = PTHREAD_MUTEX_INITIALIZER
  },
  .bufferPart = {}
};

classPo wordBufferClass = (classPo) &WordBufferClass;

static void initWordBufferClass(classPo class, classPo req) {
}

size_t grainSize(bufferGrain grain) {
  switch (grain) {
    case byteGrain:
      return sizeof(byte);
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
  f->buffer.pos = 0;
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

static retCode ensureSpace(wordBufferPo f, integer count) {
  if (f->buffer.pos + count >= f->buffer.bufferSize) {
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

retCode closeWordBuffer(wordBufferPo buffer) {
  decReference(O_OBJECT(buffer)); /* this will get rid of all the buffer objects attributes */
  return Ok;
}

static integer bufferNo = 0;

wordBufferPo newWordBuffer(bufferGrain grain) {
  byte *buffer = (byte *) malloc(grainSize(grain) * 128);

  return O_WORDBUFFER(newObject(wordBufferClass, buffer, 128, grain, ioWRITE, True));
}

byte *getCurrentBufferData(wordBufferPo s, integer *len) {
  *len = s->buffer.pos;
  return s->buffer.buffer;
}

byte *getBufferData(wordBufferPo b, integer *len) {
  byte *current = getCurrentBufferData(b, len);
  byte *data = malloc(grainSize(b->buffer.grain) * b->buffer.pos);
  memmove(data, current, grainSize(b->buffer.grain) * (*len));
  return data;
}

retCode appendWordToBuffer(wordBufferPo b, integer word) {
  ensureSpace(b, 1);
  switch (b->buffer.grain) {
    case byteGrain:
      b->buffer.buffer[b->buffer.pos++] = (byte) word;
      return Ok;
    case shortGrain:
      ((int16 *) b->buffer.buffer)[b->buffer.pos++] = (int16) word;
      return Ok;
    case wordGrain:
      ((int32 *) b->buffer.buffer)[b->buffer.pos++] = (int32) word;
      return Ok;
    case longGrain:
      ((int64 *) b->buffer.buffer)[b->buffer.pos++] = (int64) word;
      return Ok;
  }
  return Error;
}
