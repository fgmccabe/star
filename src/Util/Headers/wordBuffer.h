/*
 * wodBuffer.h
 *
 *  Created on: 10/29/20
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_WORDBUFFER_H_
#define OOLIB_HEADERS_WORDBUFFER_H_

/*
  Word Buffer (private header)
  Copyright (c) 2016, 2017, 2020. Francis G. McCabe
*/

#include "object.h"

typedef struct wordBuffer_ *wordBufferPo;
extern classPo wordBufferClass;

typedef enum{
  byteGrain,
  shortGrain,
  wordGrain,
  longGrain
} bufferGrain;

wordBufferPo newWordBuffer(bufferGrain grain);
retCode closeWordBuffer(wordBufferPo buffer);
size_t grainSize(bufferGrain grain);

byte * getCurrentBufferData(wordBufferPo s, integer *len);
retCode appendWordToBuffer(wordBufferPo b, integer data);
byte* getBufferData(wordBufferPo b,integer *len);

#ifdef VERIFY_OBJECT
#define O_WORDBUFFER(c) ((wordBufferPo)(checkCast((c),wordBufferClass)))
#else
#define O_WORDBUFFER(c) ((wordBufferPo)(c))
#endif

#endif /* O_WORDBUFFER */
