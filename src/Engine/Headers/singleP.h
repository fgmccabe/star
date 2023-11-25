//
// Created by Francis McCabe on 9/10/21.
//

#ifndef STAR_SINGLEP_H
#define STAR_SINGLEP_H

#include "single.h"
#include "termP.h"
#include "heap.h"

void initSingle();

typedef struct single_record {
  clssPo clss;                  // == futureClass
  integer hash;
  termPo val;
} SingleRecord;

#define SingleCellCount CellCount(sizeof(SingleRecord))

#endif //STAR_SINGLEP_H
