//
// Created by Francis McCabe on 2/21/21.
//

#ifndef STAR_CONTINUATIONP_H
#define STAR_CONTINUATIONP_H

#include "continuation.h"
#include "termP.h"

typedef struct contination_record {
  clssPo clss;                  // == continuationClass
  stackPo stk;
  ContinuationState state;
} ContinuationRecord;

extern void initContinuations();

#define CONTINUATION_CELLCOUNT CellCount(sizeof(ContinuationRecord))

extern logical traceContinuation;

#endif //STAR_CONTINUATIONP_H
