/*
  Parse the Unicode file to extract requird info
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
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

typedef enum{
  OtherControl=1<<0,
  OtherFormat=1<<1,
  OtherUnassigned=1<<2,
  OtherPrivate=1<<3,
  OtherSurrogate=1<<4,
  LetterLower=1<<5,
  LetterModifier=1<<6,
  LetterOther=1<<7,
  LetterTitle=1<<8,
  LetterUpper=1<<9,
  MarkSpacingCombining=1<<10,
  MarkEnclosing=1<<11,
  MarkNonSpacing=1<<12,
  NumberDigit=1<<13,
  NumberLetter=1<<14,
  NumberOther=1<<15,
  PuncConn=1<<16,
  PuncDash=1<<17,
  PuncClose=1<<18,
  PuncFinal=1<<19,
  PuncInitial=1<<20,
  PuncOther=1<<21,
  PuncOpen=1<<22,
  SymbolCurrency=1<<23,
  SymbolModifier=1<<24,
  SymbolMath=1<<25,
  SymbolOther=1<<26,
  SeparatorLine=1<<27,
  SeparatorPara=1<<28,
  SeparatorSpace=1<<29
} CharCatagory;


long codes[1<<16];      /* The table of character attributes */
  

int main(int argc,char **argv)
{
  FILE *out=stdout;
  FILE *in=stdin;

  if(argc>=2){
    in = fopen(argv[1],"r");
    
    if(argc>=3)
      out=fopen(argv[2],"w");
  }
  
}

int parseUnicodes(FILE *in)
{
  while(!eof(in)){
    long code;          /* The unicode character */
    char    
    
    
  }
}
