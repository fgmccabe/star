//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_STR_H
#define CAFE_STR_H

#include "engine.h"
#include "term.h"

// String structure
typedef struct string_struct *stringPo;

extern clssPo stringClass;

typedef retCode (*charProc)(codePoint ch, integer ix, void *cl);

extern termPo allocateString(heapPo H, char *txt, long length);

static inline logical isString(termPo p) {
  return hasClass(p, stringClass);
}

extern const char *stringVal(termPo o, integer *size);

extern integer stringHash(stringPo str);

extern retCode processString(stringPo str,charProc p,void *cl);

extern retCode copyString2Buff(stringPo str, char *buffer, integer buffLen);

#endif //CAFE_STR_H
