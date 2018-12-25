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

typedef struct _buffer_ *bufferPo;
extern classPo bufferClass;

bufferPo newStringBuffer();
bufferPo newIoStringBuffer();
bufferPo newReadStringBuffer(char *text, integer textLen);
bufferPo fixedStringBuffer(char *buffer, long len);

char *getTextFromBuffer(bufferPo s, integer *len);
integer bufferLength(bufferPo b);
integer bufferOutPos(bufferPo b);
integer bufferBumpOutPos(bufferPo b,integer incr);
retCode insertIntoBuffer(bufferPo b, codePoint ch);
retCode deleteFromBuffer(bufferPo b, integer len);
retCode appendToBuffer(bufferPo b, char *text, integer txtLen);
retCode appendIntoBuffer(bufferPo b, char *text, integer txtLen);
retCode stringIntoBuffer(bufferPo b, strgPo str);
retCode clearBuffer(bufferPo b);
retCode rewindBuffer(bufferPo b);
retCode seekBuffer(bufferPo b,integer pos);
retCode twizzleBuffer(bufferPo b,integer pos);

strgPo stringFromBuffer(bufferPo b);

logical isTrivialBuffer(bufferPo b);

#ifdef VERIFY_OBJECT
#define O_BUFFER(c) ((bufferPo)(checkCast((c),bufferClass)))
#else
#define O_BUFFER(c) ((bufferPo)(c))
#endif

#endif /* OOLIB_HEADERS_STRINGBUFFER_H_ */
