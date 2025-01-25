//
// Created by Francis McCabe on 1/7/24.
//

#ifndef STAR_FUTUREP_H
#define STAR_FUTUREP_H

#include "future.h"
#include "termP.h"

void initFuture();

typedef enum{
  notResolved,
  isAccepted,
  isRejected
} futureState;

typedef struct future_record {
  ClassRecord clss;                  // == futureClass
  integer hash;
  termPo val;
  futureState state;
  futurePoll poller;
  void *cl;
  void *cl2;
} FutureRecord;

#define FutureCellCount CellCount(sizeof(FutureRecord))

futurePo makeResolvedFuture(heapPo h, termPo val, futureState state);

#endif //STAR_FUTUREP_H
