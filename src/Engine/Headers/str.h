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

typedef retCode (*charProc)(codePoint ch, integer ix, void *cl);

static inline logical isString(termPo p) {
  return hasClass(p, stringClass);
}

extern const char *stringVal(termPo o, integer *size);

extern integer stringLength(stringPo str);

extern integer stringHash(stringPo str);

extern stringPo allocateString(heapPo H, const char *txt, long length);

extern stringPo allocateCString(heapPo H, const char *txt);

extern retCode processString(stringPo str, charProc p, void *cl);

extern retCode copyString2Buff(stringPo str, char *buffer, integer buffLen);

#endif //STAR_STR_H
