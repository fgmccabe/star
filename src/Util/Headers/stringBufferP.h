/*
 * stringBufferP.h
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_STRINGBUFFERP_H_
#define OOLIB_HEADERS_STRINGBUFFERP_H_

#include "ioP.h"
#include "stringBuffer.h"

typedef struct {
} BufferClassPartRec;

typedef struct _buffer_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;
  BufferClassPartRec bufferPart;
} BufferClassRec;

extern BufferClassRec BufferClass;  /* the standard pointer to an buffer class record */

typedef struct _buffer_part_{
  char *buffer;                         /* The data buffer */
  integer bufferSize;
  integer in_pos;                       // Position of next read
  integer out_pos;                      // position of next write
  integer size;                         // in_pos <= size <= out_pos
  logical resizeable;                   /* Is this string object resizeable? */
} BufferPart;

typedef struct _buffer_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            /* Io level of io object */
  BufferPart buffer;
} BufferObject;



#endif /* OOLIB_HEADERS_STRINGBUFFERP_H_ */
