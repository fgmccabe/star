//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_CHARS_H
#define STAR_CHARS_H

#include "term.h"
#include "heap.h"

// String structure
typedef struct chars_term_ *charsPo;

extern clssPo charsClass;

extern charsPo C_CHARS(termPo t);

static inline logical isChars(termPo p) {
  return hasClass(p, charsClass);
}

const char *charsVal(termPo o, integer *size);

integer charsLength(charsPo str);

integer charsHash(charsPo str);

termPo allocateChars(heapPo H, const char *txt, long length);

termPo allocateFromStrBuffer(strBufferPo bffr, heapPo H);

termPo allocateCString(heapPo H, const char *txt);

retCode copyChars2Buff(charsPo str, char *buffer, integer buffLen);

logical sameCharSeqs(charsPo s1, charsPo s2);

retCode quoteChars(ioPo out, charsPo str);

retCode outChars(ioPo out, termPo t, integer precision, integer depth, logical alt);

#endif //STAR_CHARS_H
