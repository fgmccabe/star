//
// Created by Francis McCabe on 6/7/18.
//

#ifndef STAR_FUTUREP_H
#define STAR_FUTUREP_H

#include "future.h"
#include "code.h"
#include "termP.h"
#include <stdatomic.h>


typedef struct _future_record_ {
  clssPo clss;                  // == cellClass
  _Atomic logical isSet;
  termPo generator;
  termPo content;               // Contents
} FutureRecord;

#define FutureCellCount CellCount(sizeof(FutureRecord))

void initFuture();

futurePo newFuture(heapPo H, termPo content);

#endif //STAR_FUTUREP_H
