//
// Created by Francis McCabe on 11/21/22.
//

#ifndef STAR_CONTINUATIONP_H
#define STAR_CONTINUATIONP_H

#include "continuation.h"
#include "heap.h"

typedef struct ContStructure {
  clssPo clss;                  // == continClass
  stackPo stack;
  framePo fp;
  insPo pc;
  integer counter;
} ContinuationRecord;

#define ContinuationCellCount CellCount(sizeof(ContinuationRecord))

extern logical traceContinuations;      // continuation tracing

void initContinuations();

#endif //STAR_CONTINUATIONP_H
