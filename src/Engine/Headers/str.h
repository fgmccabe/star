//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_STR_H
#define STAR_STR_H

#include "term.h"
#include "heap.h"

// String structure
typedef struct string_term *stringPo;

extern clssPo stringClass;

extern stringPo C_STR(termPo t);

static inline logical isString(termPo p) {
  return hasClass(p, stringClass);
}

extern const char *stringVal(termPo o, integer *size);

extern integer stringLength(stringPo str);

extern integer stringHash(stringPo str);

extern termPo allocateString(heapPo H, const char *txt, long length);

extern termPo allocateCString(heapPo H, const char *txt);

extern retCode copyString2Buff(stringPo str, char *buffer, integer buffLen);

extern logical sameString(stringPo s1, stringPo s2);

#endif //STAR_STR_H
