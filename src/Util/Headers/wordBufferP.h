/*
 * wordBufferP.h
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_WORDBUFFERP_H_
#define OOLIB_HEADERS_WORDBUFFERP_H_

#include "ioP.h"
#include "wordBuffer.h"

typedef struct {
} WordBufferClassPartRec;

typedef struct wordBuffer_class_ {
  ObjectClassRec objectPart;
  WordBufferClassPartRec bufferPart;
} WordBufferClassRec;

extern WordBufferClassRec WordBufferClass;  /* the standard pointer to an buffer class record */

typedef struct wordBuffer_part_{
  byte *buffer;                         /* The data buffer */
  integer bufferSize;
  integer size;                         // How many elements in the buffer
  integer in_pos;                       // Position of next read
  integer out_pos;                      // position of next write
  logical resizeable;                   /* Is this buffer object resizeable? */
  bufferGrain grain;                    // How big is each grain
  ioDirection mode;                     // reading or writing
} WordBufferPart;

typedef struct wordBuffer_ {
  ObjectRec object;                     /* object level of the io structure */
  WordBufferPart buffer;
} WordBufferObject;

#endif /* OOLIB_HEADERS_WORDBUFFERP_H_ */
