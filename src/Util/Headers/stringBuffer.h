/*
 * stringBuffer.h
 *   Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_STRINGBUFFER_H_
#define OOLIB_HEADERS_STRINGBUFFER_H_

#include "object.h"
#include "unistr.h"
#include "strng.h"

typedef struct stringBuffer_ *strBufferPo;
extern classPo strBufferClass;

logical isAStringBuffer(objectPo o);

strBufferPo newStringBuffer();
strBufferPo newReadStringBuffer(char *text, integer textLen);
strBufferPo fixedStringBuffer(char *buffer, long len);

char *getTextFromBuffer(strBufferPo s, integer *len);
integer strBufferLength(strBufferPo b);
integer strBufferBumpOutPos(strBufferPo b, integer incr);
retCode insertIntoStringBuffer(strBufferPo b, codePoint ch);
retCode deleteFromStrBuffer(strBufferPo b, integer len);
retCode appendToStrBuffer(strBufferPo b, const char *text, integer txtLen);
retCode appendIntoStrBuffer(strBufferPo b, const char *text, integer txtLen);
retCode stringIntoStrBuffer(strBufferPo b, strgPo str);
retCode appendCodePointToStrBuffer(strBufferPo b, codePoint ch);
retCode clearStrBuffer(strBufferPo b);
retCode rewindStrBuffer(strBufferPo b);
retCode twizzleStrBuffer(strBufferPo b, integer pos);

strgPo stringFromBuffer(strBufferPo b);

logical isTrivialBuffer(strBufferPo b);

#ifdef VERIFY_OBJECT
#define O_BUFFER(c) ((strBufferPo)(checkCast((c),strBufferClass)))
#else
#define O_BUFFER(c) ((strBufferPo)(c))
#endif

#endif /* OOLIB_HEADERS_STRINGBUFFER_H_ */
