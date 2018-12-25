/* 
  I/O handling top-level
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _IO_LIB_H_
#define _IO_LIB_H_

#include "integer.h"
#include "object.h"
#include "lockable.h"
#include "strng.h"

typedef enum {
  ioNULL = 000, ioREAD = 001, ioWRITE = 002
} ioDirection;

typedef struct _io_object_ *ioPo;
extern classPo ioClass;

#include "unistr.h"
#include "stringBuffer.h"

#ifndef MAXLINE
#define MAXLINE 1024                  /* Size of a standard line buffer */
#endif

#ifndef MAXFILELEN
#define MAXFILELEN 512                /* Maximum length of a file name */
#endif

void closeIo(void);                     /* Close down the I/O system */

retCode inByte(ioPo f, byte *ch);      /* read a single byte -- with status */
retCode inBytes(ioPo f, byte *buffer, integer len, integer *actual); /* read a block of bytes */
retCode putBackByte(ioPo f, byte b);

retCode isLookingAt(ioPo f, char *prefix);    /* Is prefix the first thing in the file? */

retCode inChar(ioPo f, codePoint *ch);     /* read a character */
retCode unGetChar(ioPo f, codePoint ch);   /* put a single character back */
retCode inLine(ioPo f, bufferPo buffer, char *term);

retCode pushBack(ioPo f, char *str, integer from, integer len);

retCode outByte(ioPo f, byte c);
retCode outChar(ioPo f, codePoint ch);
retCode outBytes(ioPo f, byte *data, integer len, integer *actual);

retCode outText(ioPo f, const char *text, integer len);
retCode outStr(ioPo f, char *str);
retCode outStrg(ioPo f,strgPo str);

retCode closeFile(ioPo f);            /* generic file closer */
retCode flushFile(ioPo f);            /* generic file flush */
/* file flush */
void flushOut(void);                    /* flush all files */

void setEncoding(ioPo f, ioEncoding encoding);
retCode isFileOpen(ioPo f);
logical isReadingFile(ioPo f);
logical isWritingFile(ioPo f);
retCode isFileAtEof(ioPo f);
retCode fileStatus(ioPo f);

char * fileName(ioPo f);
ioDirection fileMode(ioPo f);
integer inCPos(ioPo f);
integer outBPos(ioPo f);

retCode ioErrorMsg(ioPo io, char *fmt, ...);

#ifdef VERIFY_OBJECT
#define O_IO(c) ((ioPo)(checkCast((c),ioClass)))
#else
#define O_IO(c) ((ioPo)(c))
#endif

#endif
