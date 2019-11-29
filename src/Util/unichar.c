/*
  Character classification of UNICODE characters
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.

*/

#include "config.h"
#include "unistrP.h"

/* Other, Control */
logical isCcChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Cc);
}

/* Other, format */
logical isCfChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Cf);
}

/* Other, unassigned */
logical isCnChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Cn);
}

/* Other, private */
logical isCoChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Co);
}

/* Other, surrogate */
logical isCsChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Cs);
}

/* Letter, lowercase */
logical isLlChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Ll);
}

/* Letter, modifier */
logical isLmChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Lm);
}

/* Letter, other */
logical isLoChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Lo);
}

/* Letter, titlecase */
logical isLtChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Lt);
}

/* Letter, uppercase */
logical isLuChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Lu);
}

/* Mark, spacing combining */
logical isMcChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Mc);
}

/* Mark, enclosing */
logical isMeChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Me);
}

/* Mark, nonspacing */
logical isMnChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Mn);
}

/* Number, decimal digit */
logical isNdChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Nd);
}

/* Number, letter */
logical isNlChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Nl);
}

/* Number, other */
logical isNoChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == No);
}

/* Punctuation, connector */
logical isPcChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Pc);
}

/* Punctuation, dash */
logical isPdChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Pd);
}

/* Punctuation, close */
logical isPeChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Pe);
}

/* Punctuation, final quote */
logical isPfChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Pf);
}

/* Punctuation, initial quote */
logical isPiChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Pi);
}

/* Punctuation, other */
logical isPoChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Po);
}

/* Punctuation, open */
logical isPsChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Ps);
}

/* Symbol, currency */
logical isScChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Sc);
}

/* Symbol, modifier */
logical isSkChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Sk);
}

/* Symbol, math */
logical isSmChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Sm);
}

/* Symbol, other */
logical isSoChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == So);
}

/* Separator, line */
logical isZlChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Zl);
}

/* Separator, paragraph */
logical isZpChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Zp);
}

/* Separator, space */
logical isZsChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Zs);
}

logical isIdStart(codePoint ch) {
  return (logical) (
    genCatTbl[ch] == Lu ||
    genCatTbl[ch] == Ll ||
    genCatTbl[ch] == Lt ||
    genCatTbl[ch] == Lm ||
    genCatTbl[ch] == Lo ||
    genCatTbl[ch] == Nl);
}

logical isIdContinue(codePoint ch) {
  return (logical) (isIdStart(ch) ||
                    genCatTbl[ch] == Nd ||
                    genCatTbl[ch] == Mn
  );
}

logical isLetterChar(codePoint ch) {
  return (logical) (genCatTbl[ch] == Lu || genCatTbl[ch] == Ll || genCatTbl[ch] == Lt || genCatTbl[ch] == Lm ||
                    genCatTbl[ch] == Lo || genCatTbl[ch] == Nl);
}

logical isSpaceChar(codePoint ch) {
  return (logical) (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r');
}

codePoint lowerOf(codePoint ch)             /* <<<<< Fix me <<<<< */
{
  if ((ch >= 'A' && ch <= 'Z') ||
      (ch >= 192 && ch <= 214) ||
      (ch >= 216 && ch <= 222))
    return ch + 32;
  else
    return ch;
}

/* Compute the digit value associated with a unicode digit character */
int digitValue(codePoint ch) {
  if (isNdChar(ch)) {
    if (0x30 <= ch && ch <= 0x39)  /* ASCII digits */
      return (int) (ch - 0x30u);
    else if (0x660 <= ch && ch <= 0x669) /* Arabic indic digits */
      return (int) (ch - 0x660u);
    else if (0x6F0 <= ch && ch <= 0x6F9) /* extended arabic */
      return (int) (ch - 0x6f0u);
    else if (0x7c0 <= ch && ch <= 0x7c9) /* ??? */
      return (int) (ch - 0x7c0u);
    else if (0x966 <= ch && ch <= 0x96f) /* devanagari digits */
      return (int) (ch - 0x966u);
    else if (0x9e6 <= ch && ch <= 0x9ef) /* Bengali digits */
      return (int) (ch - 0x9e6u);
    else if (0xa66 <= ch && ch <= 0xa6f) /* Gurmukhi digits */
      return (int) (ch - 0xa66u);
    else if (0xae6 <= ch && ch <= 0xaef) /* Gujurati digits */
      return (int) (ch - 0xae6u);
    else if (0xb66 <= ch && ch <= 0xb6f) /* Oriya digits */
      return (int) (ch - 0xb66u);
    else if (0xbe6 <= ch && ch <= 0xbef) /* Tamil digits */
      return (int) (ch - 0xbe6u);
    else if (0xc66 <= ch && ch <= 0xc6f) /* Telegu digits */
      return (int) (ch - 0xc66u);
    else if (0xce6 <= ch && ch <= 0xcef) /* Kannada digits */
      return (int) (ch - 0xce6u);
    else if (0xd66 <= ch && ch <= 0xd6f) /* Malayam digits */
      return (int) (ch - 0xd66u);
    else if (0xe50 <= ch && ch <= 0xe59) /* Thai digits */
      return (int) (ch - 0xe50u);
    else if (0xed0 <= ch && ch <= 0xed9) /* Lao digits */
      return (int) (ch - 0xed0u);
    else if (0xf20 <= ch && ch <= 0xf29) /* Tibetan digits */
      return (int) (ch - 0xf20u);
    else if (0x1040 <= ch && ch <= 0x1049) /* Myanmar digits */
      return (int) (ch - 0x1040u);
    else if (0x1369 <= ch && ch <= 0x1371) /* Ethiopic digits */
      return (int) (ch - 0x1369u);
    else if (0x17e0 <= ch && ch <= 0x17e9) /* Khmer digits */
      return (int) (ch - 0x17e0u);
    else if (0x1810 <= ch && ch <= 0x1819) /* Mongolian digits */
      return (int) (ch - 0x1810u);
    else if (ch == 0x3007)    /* CJK digit 0 */
      return 0;
    else if (ch == 0x4e00)    /* CJK digit 1 */
      return 1;
    else if (ch == 0x4e8c)    /* CJK digit 2 */
      return 2;
    else if (ch == 0x4e09)    /* CJK digit 3 */
      return 3;
    else if (ch == 0x56db)    /* CJK digit 4 */
      return 4;
    else if (ch == 0x4e94)    /* CJK digit 5 */
      return 5;
    else if (ch == 0x516d)    /* CJK digit 6 */
      return 6;
    else if (ch == 0x4e03)    /* CJK digit 7 */
      return 7;
    else if (ch == 0x516b)    /* CJK digit 8 */
      return 8;
    else if (ch == 0x4e5d)    /* CJK digit 9 */
      return 9;
    else
      return -1;
  } else
    return -1;
}
