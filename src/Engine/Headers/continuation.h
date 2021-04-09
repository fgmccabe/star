//
// Created by Francis McCabe on 2/21/21.
//

#ifndef STAR_CONTINUATION_H
#define STAR_CONTINUATION_H

#include "term.h"
#include "heap.h"
#include "stack.h"

typedef struct contination_record *continuationPo;

extern clssPo continuationClass;

extern continuationPo C_CONTINUATION(termPo t);
extern continuationPo allocateContinuation(heapPo H,stackPo stk);

typedef enum{
  continuationEmpty,
  continuationCaptured,
  continuationEntered
} ContinuationState;

extern ContinuationState markContinuation(continuationPo cnt,ContinuationState state);

#endif //STAR_CONTINUATION_H
