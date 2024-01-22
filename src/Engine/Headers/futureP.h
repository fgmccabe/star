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
  clssPo clss;                  // == promiseClass
  integer hash;
  termPo val;
  futureState state;
} FutureRecord;

#define FutureCellCount CellCount(sizeof(FutureRecord))

#endif //STAR_FUTUREP_H
