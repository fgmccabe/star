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
} StrBufferClassPartRec;

typedef struct strBuffer_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockPart;
  IoClassPartRec ioPart;
  StrBufferClassPartRec bufferPart;
} BufferClassRec;

extern BufferClassRec StrBufferClass;  /* the standard pointer to an buffer class record */

typedef struct strBuffer_part_{
  char *buffer;                         /* The data buffer */
  char line[MAXLINE];                   // Temporary buffer space
  integer bufferSize;
  integer pos;                          // Position of next operation
  integer size;                         // in_pos <= size <= out_pos
  logical resizeable;                   /* Is this string object resizeable? */
} BufferPart;

typedef struct stringBuffer_ {
  ObjectRec object;                     /* object level of the io structure */
  LockObjectRec lock;
  IoPart io;                            /* Io level of io object */
  BufferPart buffer;
} BufferObject;

retCode showStringBuffer(ioPo f, void *data, long depth, long precision, logical alt);

#endif /* OOLIB_HEADERS_STRINGBUFFERP_H_ */
