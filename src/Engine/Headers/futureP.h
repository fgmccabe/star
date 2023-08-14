//
// Created by Francis McCabe on 9/10/21.
//

#ifndef STAR_FUTUREP_H
#define STAR_FUTUREP_H

#include "future.h"
#include "termP.h"
#include "heap.h"

void initFuture();

typedef struct future_record_ {
  clssPo clss;                  // == futureClass
  integer hash;
  termPo val;
  futureSetProc set;
} FutureRecord;

#define FutureCellCount CellCount(sizeof(FutureRecord))

#endif //STAR_FUTUREP_H
