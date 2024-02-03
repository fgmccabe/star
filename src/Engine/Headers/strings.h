//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_STRINGS_H
#define STAR_STRINGS_H

#include "term.h"
#include "heap.h"

// String structure
typedef struct string_term_ *stringPo;

extern clssPo strClass;

extern stringPo C_STR(termPo t);

static inline logical isString(termPo p) {
  return hasClass(p, strClass);
}

const char *strVal(termPo o, integer *size);

integer strLength(stringPo str);

integer stringHash(stringPo str);

termPo allocateString(heapPo H, const char *txt, long length);

termPo allocateFromStrBuffer(heapPo H, strBufferPo bffr);

termPo allocateCString(heapPo H, const char *txt);

retCode copyChars2Buff(stringPo str, char *buffer, integer buffLen);

logical sameString(stringPo s1, stringPo s2);

retCode quoteStrg(ioPo out, stringPo str);

retCode qtChar(ioPo f, codePoint ch);

retCode outChars(ioPo out, termPo t, integer precision, integer depth, logical alt);

#endif //STAR_STRINGS_H
