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

UniCharCategory uniCharCategory(codePoint ch) {
  return genCatTbl[ch];
}

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
  if (isNdChar(ch))
    return digitValTbl[ch];
  else
    return -1;
}
