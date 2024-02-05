//
// Created by Francis McCabe on 2/3/24.
//

#ifndef STAR_BYTEBUFFERP_H
#define STAR_BYTEBUFFERP_H

#include "ioP.h"
#include "byteBuffer.h"

typedef struct {
} StrBufferClassPartRec;

typedef struct byteBuffer_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;
  StrBufferClassPartRec bufferPart;
} ByteBufferClassRec;

extern ByteBufferClassRec ByteBufferClass;  /* the standard pointer to an buffer class record */

typedef struct byteBuffer_part_{
  byte *buffer;                         /* The data buffer */
  byte line[MAXLINE];                   // Temporary buffer space
  integer bufferSize;
  integer in_pos;                       // Position of next read
  integer out_pos;                      // position of next write
  integer size;                         // in_pos <= size <= out_pos
  logical resizeable;                   /* Is this string object resizeable? */
} ByteBufferPart;

typedef struct byteBuffer_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            /* Io level of io object */
  ByteBufferPart buffer;
} BufferObject;

retCode showByteBuffer(ioPo f, void *data, long depth, long precision, logical alt);

#endif //STAR_BYTEBUFFERP_H
