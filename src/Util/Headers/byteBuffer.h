/*
 * byteBuffer.h
 *   Copyright (c) 2024 and beyond. Francis G. McCabe
 *  Created by Francis McCabe on 2/3/24.
 */

#ifndef OOLIB_HEADERS_BYTEBUFFER_H_
#define OOLIB_HEADERS_BYTEBUFFER_H_

#include "object.h"

typedef struct byteBuffer_ *byteBufferPo;
extern classPo byteBufferClass;

logical isAByteBuffer(objectPo o);

byteBufferPo newByteBuffer();
byteBufferPo newIoByteBuffer();
byteBufferPo newReadByteBuffer(byte *text, integer textLen);
byteBufferPo fixedByteBuffer(byte *buffer, long len);

byte *getBytesFromBuffer(byteBufferPo s, integer *len);
integer byteBufferLength(byteBufferPo b);
integer byteBufferOutPos(byteBufferPo b);
retCode appendByteToBuffer(byteBufferPo b, byte by);
retCode clearByteBuffer(byteBufferPo b);
retCode rewindByteBuffer(byteBufferPo b);

#ifdef VERIFY_OBJECT
#define O_BYTEBUFFER(c) ((byteBufferPo)(checkCast((c),byteBufferClass)))
#else
#define O_BYTEBUFFER(c) ((byteBufferPo)(c))
#endif

#endif /* OOLIB_HEADERS_BYTEBUFFER_H_ */
