/*
 * byteBuffer.h
 *
 *  Created on: 10/29/20
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_BYTEBUFFER_H_
#define OOLIB_HEADERS_BYTEBUFFER_H_

/*
  Byte Buffer (private header)
  Copyright (c) 2016, 2017, 2020. Francis G. McCabe
*/

#include "object.h"

typedef struct byteBuffer_ *byteBufferPo;
extern classPo byteBufferClass;

byteBufferPo newByteBuffer(size_t size);
byteBufferPo fixedByteBuffer(char *buffer, long len);

char *getBytesFromBuffer(byteBufferPo s, integer *len);
integer byteBufferLength(byteBufferPo b);
integer byteBufferOutPos(byteBufferPo b);
retCode appendToByteBuffer(byteBufferPo b, const char *text, integer txtLen);
retCode appendIntoByteBuffer(byteBufferPo b, const char *text, integer txtLen);
retCode appendByteToBuffer(byteBufferPo b, byte byte);
retCode clearByteBuffer(byteBufferPo b);
retCode rewindByteBuffer(byteBufferPo b);

#ifdef VERIFY_OBJECT
#define O_BYTEBUFFER(c) ((byteBufferPo)(checkCast((c),byteBufferClass)))
#else
#define O_BYTEBUFFER(c) ((byteBufferPo)(c))
#endif

#endif /* O_BYTEBUFFER */
