/* 
   Unicode interface
   (c) 1994-2010 F.G. McCabe

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   
   Contact: Francis McCabe <fmccabe@gmail.com>
*/ 

#ifndef _IO_UNICODE_H_
#define _IO_UNICODE_H_

#include "integer.h"
#include "retcode.h"
#include "logical.h"

typedef unsigned int16 uniChar; /* define the unicode character base type */
typedef unsigned int16 *string; /* A string is a pointer to a unichar seq */

typedef enum {rawEncoding,	        /* only read/write 8bit data */
	      utf16Encoding,
	      utf16EncodingSwap,
	      utf8Encoding,
              unknownEncoding} ioEncoding;

/* The various ranges in the UTF-8 encoding are as follows:
  
  0x00000000 - 0x0000007F:
  0xxxxxxx

  0x00000080 - 0x000007FF:
  110xxxxx 10xxxxxx
*/
#define U80 ((0x6)<<5)
#define M80 ((0x7)<<5)
#define UC80(x) (((x)&M80)==U80)
#define UX80(x) ((x)&(~M80))

#define UR ((0x2)<<6)
#define MR ((0x3)<<6)
#define UCR(x) (((x)&MR)==UR)
#define UXR(x) ((x)&(~MR))

/*

  0x00000800 - 0x0000FFFF:
  1110xxxx 10xxxxxx 10xxxxxx

*/
#define U800 ((0xe)<<4)
#define M800 ((0xf)<<4)
#define UC800(x) (((x)&M800)==U800)
#define UX800(x) ((x)&(~M800))

/*
  The  xxx  bit  positions  are  filled with the bits of the
  character code number in binary representation.  Only  the
  shortest  possible  multibyte sequence which can represent
  the code number of the character can be used.
*/

logical isChar(uniChar ch);	/* Is character a legal Unicode char? */

logical isCcChar(uniChar ch);
logical isCfChar(uniChar ch);
logical isCnChar(uniChar ch);
logical isCoChar(uniChar ch);
logical isCsChar(uniChar ch);
logical isLlChar(uniChar ch);
logical isLmChar(uniChar ch);
logical isLoChar(uniChar ch);
logical isLtChar(uniChar ch);
logical isLuChar(uniChar ch);
logical isMcChar(uniChar ch);
logical isMeChar(uniChar ch);
logical isMnChar(uniChar ch);
logical isNdChar(uniChar ch);
logical isNlChar(uniChar ch);
logical isNoChar(uniChar ch);
logical isPcChar(uniChar ch);
logical isPdChar(uniChar ch);
logical isPeChar(uniChar ch);
logical isPfChar(uniChar ch);
logical isPiChar(uniChar ch);
logical isPoChar(uniChar ch);
logical isPsChar(uniChar ch);
logical isScChar(uniChar ch);
logical isSkChar(uniChar ch);
logical isSmChar(uniChar ch);
logical isSoChar(uniChar ch);
logical isZlChar(uniChar ch);
logical isZpChar(uniChar ch);
logical isZsChar(uniChar ch);

logical isLetterChar(uniChar ch);
logical isSpaceChar(uniChar ch);
int digitValue(uniChar ch);

uniChar lowerOf(uniChar ch);
uniChar upperOf(uniChar ch);

logical isUniIdentifier(uniChar *id);

long uniStrLen(const uniChar *s);
uniChar *uniCat(uniChar *dest,long len,const uniChar *src);
uniChar *uniAppend(uniChar *dest,long len,const uniChar *src);
uniChar *uniTackOn(uniChar *dest,long len,uniChar ch);
uniChar *uniCpy(uniChar *dest,long len,const uniChar *src);
uniChar *uniNCpy(uniChar *dest,long len,const uniChar *src,long sLen);
int uniCmp(uniChar *s1,uniChar *s2);
int uniNCmp(uniChar *s1,uniChar *s2,long l);
logical uniIsTail(uniChar *s1,uniChar *s2);
uniChar *uniInsert(uniChar *dest,long len,const uniChar *src);
uniChar *uniTack(uniChar *dest,long len,const char *src);
uniChar *uniSearch(uniChar *s,long len,uniChar c);

long uniIndexOf(uniChar *s,long len,uniChar c);
long uniLastIndexOf(uniChar *s,long len,uniChar c);
uniChar *uniSubStr(uniChar *s,long len,int from,int cnt,uniChar *buff,int bLen);

uniChar *uniSearchAny(uniChar *s,long len,uniChar *term);
uniChar *uniLast(uniChar *s,long l,uniChar c);
uniChar *uniDuplicate(uniChar *s);
uniChar *uniDup(uniChar *s,long len);
uniChar *uniNewStr(unsigned char *s);
uniChar *uniSplit(uniChar *s,long from,long to,uniChar *buffer,long len);
void uniFree(uniChar *s);
uniChar *uniLit(uniChar *dest,long len,const char *src);
logical uniIsLit(uniChar *s1,char *s2);
logical uniIsLitPrefix(uniChar *s1,char *s2);
uniChar *uniEndStr(uniChar *s);
uinteger uniHash(const uniChar *name);
uniChar *uniLower(uniChar *src,uniChar *buff,long len);
uniChar *uniIntern(uniChar *s);
long utf8_uni(const unsigned char *str,long max,uniChar *buff,long len);
long uni_utf8(const uniChar *s,long len,unsigned char *buff,long tlen);
unsigned char *_utf(const uniChar *s,unsigned char *b,long len);
uniChar *_uni(const unsigned char *s,uniChar *b,long len);
long uniCharUtf8Size(uniChar c);

extern uniChar uniEmpty[];

#ifndef uniEOF
#define uniEOF (0xffff)
#endif

#ifndef uniBOM                          // Byte Order mark
#define uniBOM (0xfeff)
#define uniBOMhi (0xfe)
#define uniBOMlo (0xff)
#endif


#ifndef uniSentinel                     // This marks a stream as a UTF16
#define uniSentinel (0xfeff)
#endif

#ifndef uniRevSentinel                  // This marks a stream as a byte swapped UTF16
#define uniRevSentinel (0xfffe)
#endif

#endif
