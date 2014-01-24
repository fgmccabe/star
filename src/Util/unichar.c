/*
  Character classification of UNICODE characters
  (c) 1999-2000 F.G.McCabe

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

  Contact: Francis McCabe <fgm@fla.fujitsu.com>
 */

#include "config.h"
#include "ioP.h"
#include <assert.h>
#include <stdlib.h>
#include "unichar.h"

logical isChar(uniChar ch)
{
  return genCatTbl[ch]!=Other;
}

/* Other, Control */
logical isCcChar(uniChar ch){
  return genCatTbl[ch]==Cc;
}

/* Other, format */
logical isCfChar(uniChar ch){
  return genCatTbl[ch]==Cf;
}

/* Other, unassigned */
logical isCnChar(uniChar ch){
  return genCatTbl[ch]==Cn;
}

/* Other, private */
logical isCoChar(uniChar ch){
  return genCatTbl[ch]==Co;
}

/* Other, surrogate */
logical isCsChar(uniChar ch){
  return genCatTbl[ch]==Cs;
}

/* Letter, lowercase */
logical isLlChar(uniChar ch){
  return genCatTbl[ch]==Ll;
}

/* Letter, modifier */
logical isLmChar(uniChar ch){
  return genCatTbl[ch]==Lm;
}

/* Letter, other */
logical isLoChar(uniChar ch){
  return genCatTbl[ch]==Lo;
}

/* Letter, titlecase */
logical isLtChar(uniChar ch){
  return genCatTbl[ch]==Lt;
}

/* Letter, uppercase */
logical isLuChar(uniChar ch){
  return genCatTbl[ch]==Lu;
}

/* Mark, spacing combining */
logical isMcChar(uniChar ch){
  return genCatTbl[ch]==Mc;
}

/* Mark, enclosing */
logical isMeChar(uniChar ch){
  return genCatTbl[ch]==Me;
}

/* Mark, nonspacing */
logical isMnChar(uniChar ch){
  return genCatTbl[ch]==Mn;
}

/* Number, decimal digit */
logical isNdChar(uniChar ch){
  return genCatTbl[ch]==Nd;
}

/* Number, letter */
logical isNlChar(uniChar ch){
  return genCatTbl[ch]==Nl;
}

/* Number, other */
logical isNoChar(uniChar ch){
  return genCatTbl[ch]==No;
}

/* Punctuation, connector */
logical isPcChar(uniChar ch){
  return genCatTbl[ch]==Pc;
}

/* Punctuation, dash */
logical isPdChar(uniChar ch){
  return genCatTbl[ch]==Pd;
}

/* Punctuation, close */
logical isPeChar(uniChar ch){
  return genCatTbl[ch]==Pe;
}

/* Punctuation, final quote */
logical isPfChar(uniChar ch){
  return genCatTbl[ch]==Pf;
}

/* Punctuation, initial quote */
logical isPiChar(uniChar ch){
  return genCatTbl[ch]==Pi;
}

/* Punctuation, other */
logical isPoChar(uniChar ch){
  return genCatTbl[ch]==Po;
}

/* Punctuation, open */
logical isPsChar(uniChar ch){
  return genCatTbl[ch]==Ps;
}

/* Symbol, currency */
logical isScChar(uniChar ch){
  return genCatTbl[ch]==Sc;
}

/* Symbol, modifier */
logical isSkChar(uniChar ch){
  return genCatTbl[ch]==Sk;
}

/* Symbol, math */
logical isSmChar(uniChar ch){
  return genCatTbl[ch]==Sm;
}

/* Symbol, other */
logical isSoChar(uniChar ch){
  return genCatTbl[ch]==So;
}

/* Separator, line */
logical isZlChar(uniChar ch){
  return genCatTbl[ch]==Zl;
}

/* Separator, paragraph */
logical isZpChar(uniChar ch){
  return genCatTbl[ch]==Zp;
}

/* Separator, space */
logical isZsChar(uniChar ch){
  return genCatTbl[ch]==Zs;
}

logical isLetterChar(uniChar ch)
{
  return genCatTbl[ch]==Lu||genCatTbl[ch]==Ll||genCatTbl[ch]==Lt||genCatTbl[ch]==Lm||genCatTbl[ch]==Lo||genCatTbl[ch]==Nl;
}

logical isSpaceChar(uniChar ch)
{
  return ch==' '||ch=='\t'||ch=='\n'||ch=='\r';
}


uniChar lowerOf(uniChar ch)             /* <<<<< Fix me <<<<< */
{
  if((ch >= 'A' && ch <= 'Z') ||
     (ch >= 192 && ch <= 214) ||
     (ch >= 216 && ch <= 222))
    return ch + 32;
  else
    return ch;
}

uniChar upperOf(uniChar ch)             /* <<<<< Fix me <<<<< */
{
  if((ch >= 'a' && ch <= 'z') ||
     (ch >= 224 && ch <= 246) ||
     (ch >= 248 && ch <= 254))
    return ch - 32;
  else
    return ch;
}

/* Compute the digit value associated with a unicode digit character */
int digitValue(uniChar ch)
{
  if(isNdChar(ch)){
    if(0x30<=ch && ch<=0x39)	/* ASCII digits */
      return ch-0x30;
    else if(0x660<=ch && ch<=0x669) /* Arabic indic digits */
      return ch-0x660;
    else if(0x6F0<=ch && ch<=0x6F9) /* extended arabic */
      return ch-0x6f0;
    else if(0x966<=ch && ch<=0x96f) /* devanagari digits */
      return ch-0x966;
    else if(0x9e6<=ch && ch<=0x9ef) /* Bengali digits */
      return ch-0x9e6;
    else if(0xa66<=ch && ch<=0xa6f) /* Gurmukhi digits */
      return ch-0xa66;
    else if(0xae6<=ch && ch<=0xaef) /* Gujurati digits */
      return ch-0xae6;
    else if(0xb66<=ch && ch<=0xb6f) /* Oriya digits */
      return ch-0xb66;
    else if(0xbe6<=ch && ch<=0xbef) /* Tamil digits */
      return ch-0xbe6;
    else if(0xc66<=ch && ch<=0xc6f) /* Telegu digits */
      return ch-0xc66;
    else if(0xce6<=ch && ch<=0xcef) /* Kannada digits */
      return ch-0xce6;
    else if(0xd66<=ch && ch<=0xd6f) /* Malayam digits */
      return ch-0xd66;
    else if(0xe50<=ch && ch<=0xe59) /* Thai digits */
      return ch-0xe50;
    else if(0xed0<=ch && ch<=0xed9) /* Lao digits */
      return ch-0xed0;
    else if(0xf20<=ch && ch<=0xf29) /* Tibetan digits */
      return ch-0xf20;
    else if(0x1040<=ch && ch<=0x1049) /* Myanmar digits */
      return ch-0x1040;
    else if(0x1369<=ch && ch<=0x1371) /* Ethiopic digits */
      return ch-0x1369;
    else if(0x17e0<=ch && ch<=0x17e9) /* Khmer digits */
      return ch-0x17e0;
    else if(0x1810<=ch && ch<=0x1819) /* Mongolian digits */
      return ch-0x1810;
    else if(ch==0x3007)		/* CJK digit 0 */
      return 0;
    else if(ch==0x4e00)		/* CJK digit 1 */
      return 1;
    else if(ch==0x4e8c)		/* CJK digit 2 */
      return 2;
    else if(ch==0x4e09)		/* CJK digit 3 */
      return 3;
    else if(ch==0x56db)		/* CJK digit 4 */
      return 4;
    else if(ch==0x4e94)		/* CJK digit 5 */
      return 5;
    else if(ch==0x516d)		/* CJK digit 6 */
      return 6;
    else if(ch==0x4e03)		/* CJK digit 7 */
      return 7;
    else if(ch==0x516b)		/* CJK digit 8 */
      return 8;
    else if(ch==0x4e5d)		/* CJK digit 9 */
      return 9;
    else
      return -1;
  }
  else
    return -1;
}

logical isUniIdentifier(uniChar *str)
{
  if(str!=NULL && *str!='\0' && isLetterChar(*str)){
    while(*str!='\0'){
      if(!(isLetterChar(*str) || isNdChar(*str)))
	return False;
      str++;
    }
    return True;
  }
  else
    return False;
}

