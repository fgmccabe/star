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

extern const char *charsVal(termPo o, integer *size);

extern integer charsLength(charsPo str);

extern integer charsHash(charsPo str);

extern termPo allocateChars(heapPo H, const char *txt, long length);

extern termPo allocateCString(heapPo H, const char *txt);

extern retCode copyChars2Buff(charsPo str, char *buffer, integer buffLen);

extern logical sameCharSeqs(charsPo s1, charsPo s2);

retCode quoteChars(ioPo out, charsPo str);

#endif //STAR_CHARS_H
