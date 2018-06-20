/* 
  Unicode interface
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#ifndef _IO_UNICODE_H_
#define _IO_UNICODE_H_

#include "config.h"
#include "integer.h"
#include "retcode.h"
#include "logical.h"

typedef uint32 codePoint; /* underlying code point is actually up to 20 bits */

typedef enum {
  rawEncoding,
  utf8Encoding,
  unknownEncoding
} ioEncoding;

logical isChar(codePoint ch);  /* Is character a legal codePoint char? */

logical isCcChar(codePoint ch);
logical isCfChar(codePoint ch);
logical isCnChar(codePoint ch);
logical isCoChar(codePoint ch);
logical isCsChar(codePoint ch);
logical isLlChar(codePoint ch);
logical isLmChar(codePoint ch);
logical isLoChar(codePoint ch);
logical isLtChar(codePoint ch);
logical isLuChar(codePoint ch);
logical isMcChar(codePoint ch);
logical isMeChar(codePoint ch);
logical isMnChar(codePoint ch);
logical isNdChar(codePoint ch);
logical isNlChar(codePoint ch);
logical isNoChar(codePoint ch);
logical isPcChar(codePoint ch);
logical isPdChar(codePoint ch);
logical isPeChar(codePoint ch);
logical isPfChar(codePoint ch);
logical isPiChar(codePoint ch);
logical isPoChar(codePoint ch);
logical isPsChar(codePoint ch);
logical isScChar(codePoint ch);
logical isSkChar(codePoint ch);
logical isSmChar(codePoint ch);
logical isSoChar(codePoint ch);
logical isZlChar(codePoint ch);
logical isZpChar(codePoint ch);
logical isZsChar(codePoint ch);

logical isLetterChar(codePoint ch);
logical isSpaceChar(codePoint ch);
int digitValue(codePoint ch);

codePoint lowerOf(codePoint ch);
codePoint upperOf(codePoint ch);

integer countCodePoints(const char *src, integer start, integer end);
integer uniCodeCount(char *src);

int64 advanceCodePoint(char *src, integer start, integer end, int64 count);
codePoint codePointAt(const char *src, integer pt, integer end);
codePoint nextCodePoint(const char *src, integer *start, integer end);
retCode nxtPoint(const char *src, integer *start, integer end, codePoint *code);
retCode prevPoint(const char *src, integer *pos, codePoint *code);
integer backCodePoint(char *src, integer from, integer count);

int codePointSize(codePoint pt);

logical isUniIdentifier(char * id);

integer uniStrLen(const char *s);
retCode uniCpy(char *dest, integer len, const char *src);
retCode uniNCpy(char *dest, integer len, const char *src, integer sLen);
comparison uniCmp(const char *s1, const char *s2);
comparison uniNCmp(const char *s1, integer l1, const char *s2, integer l2);
retCode uniInsert(char *dest, integer len, const char *src);
retCode appendCodePoint(char *dest, integer *pos, integer len, codePoint ch);
retCode uniTack(char * dest, long len, const char *src);
retCode uniAppend(char *dest, integer *pos, integer len, char *src);
retCode uniNAppend(char *dest, integer *pos, integer len, char *src, integer sLen);
retCode uniReverse(char *dest, integer len);

int64 uniIndexOf(const char *s, integer len, integer from, codePoint c);
integer uniLastIndexOf(char *s, integer len, codePoint c);
char * uniSubStr(char * s, long len, long from, long cnt, char * buff, long bLen);

long uniSearch(const char *src, integer len, integer start, const char *tgt, integer tlen);
char * uniSearchAny(char *s, integer len, char *term);
codePoint uniSearchDelims(char *s, integer len, char *t);
char * uniLast(char *s, integer l, codePoint c);
char * uniDuplicate(const char *s);
void uniDestroy(char *s);
logical uniIsLit(const char *s1, const char *s2);
logical uniIsLitPrefix(const char *s1, const char *s2);
logical uniIsPrefix(const char *s1, integer len1, const char *s2, integer len2);
char * uniEndStr(char * s);
integer uniHash(const char * name);
integer uniNHash(const char * name, long len);
retCode uniLower(const char *s, integer sLen, char *d, integer dLen);

integer hash64(integer ix);

#ifndef uniEOF
#define uniEOF (0xffff)
#endif

#endif
