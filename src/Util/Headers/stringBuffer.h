/*
 * stringBuffer.h
 *
 *  Created on: May 19, 2016
 *      Author: fgm
 */

#ifndef OOLIB_HEADERS_STRINGBUFFER_H_
#define OOLIB_HEADERS_STRINGBUFFER_H_

/*
  String File library (private header)
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "object.h"
#include "unistr.h"
#include "strng.h"

typedef struct stringBuffer_ *strBufferPo;
extern classPo strBufferClass;

strBufferPo newStringBuffer();
strBufferPo newIoStringBuffer();
strBufferPo newReadStringBuffer(char *text, integer textLen);
strBufferPo fixedStringBuffer(char *buffer, long len);

char *getTextFromBuffer(strBufferPo s, integer *len);
retCode readTextFromBuffer(strBufferPo b, char *tgt, integer len);
integer strBufferLength(strBufferPo b);
integer strBufferOutPos(strBufferPo b);
integer strBufferBumpOutPos(strBufferPo b, integer incr);
retCode insertIntoStringBuffer(strBufferPo b, codePoint ch);
retCode deleteFromStrBuffer(strBufferPo b, integer len);
retCode appendToStrBuffer(strBufferPo b, const char *text, integer txtLen);
retCode appendIntoStrBuffer(strBufferPo b, const char *text, integer txtLen);
retCode stringIntoStrBuffer(strBufferPo b, strgPo str);
retCode appendCodePointToStrBuffer(strBufferPo b, codePoint ch);
retCode clearStrBuffer(strBufferPo b);
retCode rewindStrBuffer(strBufferPo b);
retCode seekStrBuffer(strBufferPo b, integer pos);
retCode twizzleStrBuffer(strBufferPo b, integer pos);

strgPo stringFromBuffer(strBufferPo b);

logical isTrivialBuffer(strBufferPo b);

#ifdef VERIFY_OBJECT
#define O_BUFFER(c) ((strBufferPo)(checkCast((c),strBufferClass)))
#else
#define O_BUFFER(c) ((strBufferPo)(c))
#endif

#endif /* OOLIB_HEADERS_STRINGBUFFER_H_ */
