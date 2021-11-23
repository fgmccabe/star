/*
 * byteBufferP.h
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_BYTEBUFFERP_H_
#define OOLIB_HEADERS_BYTEBUFFERP_H_

#include "ioP.h"
#include "byteBuffer.h"

typedef struct {
} ByteBufferClassPartRec;

typedef struct byteBuffer_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;
  ByteBufferClassPartRec bufferPart;
} ByteBufferClassRec;

extern ByteBufferClassRec ByteBufferClass;  /* the standard pointer to an buffer class record */

typedef struct byteBuffer_part_{
  char *buffer;                         /* The data buffer */
  integer bufferSize;
  integer in_pos;                       // Position of next read
  integer out_pos;                      // position of next write
  integer size;                         // in_pos <= size <= out_pos
  logical resizeable;                   /* Is this buffer resizeable? */
} ByteBufferPart;

typedef struct byteBuffer_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            /* Io level of io object */
  ByteBufferPart buffer;
} ByteBufferObject;

retCode showByteBuffer(ioPo f, void *data, long depth, long precision, logical alt);

#endif /* OOLIB_HEADERS_BYTEBUFFERP_H_ */
