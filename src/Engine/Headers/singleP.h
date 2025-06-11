//
// Created by Francis McCabe on 12/5/24.
//

#ifndef STAR_SINGLEP_H
#define STAR_SINGLEP_H

#include "single.h"
#include "termP.h"
#include "heapP.h"

typedef struct single_rec_ {
  ClassRecord clss;             // == singleClass
  termPo content;               // Contents
} SingleRecord;

clssPo singleClass;

#define SingleCellCount CellCount(sizeof(SingleRecord))

extern void initSingle();

#endif //STAR_SINGLEP_H
