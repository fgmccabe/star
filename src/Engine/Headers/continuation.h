//
// Created by Francis McCabe on 11/21/22.
//

#ifndef STAR_CONTINUATION_H
#define STAR_CONTINUATION_H

#include "term.h"
#include "stack.h"

typedef struct ContStructure *continuationPo;

extern clssPo contClass;

static inline logical isContinuation(termPo p) {
  return hasClass(p, contClass);
}

continuationPo allocateContinuation(heapPo H, stackPo stack);

continuationPo C_CONTINUATION(termPo t);

stackPo contStack(continuationPo cont);
logical continIsValid(continuationPo cont);
void invalidateCont(continuationPo cont);

#endif //STAR_CONTINUATION_H
