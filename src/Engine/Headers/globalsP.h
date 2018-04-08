//
// Created by Francis McCabe on 4/5/18.
//

#ifndef CAFE_GLOBALSP_H
#define CAFE_GLOBALSP_H

#include "globals.h"
#include "termP.h"
#include "heapP.h"


typedef struct _global_rec_ {
  clssPo clss;                  // == globalClass
  integer varNo;
  integer hash;
  termPo content;               // Contents
  char *name;
} GlobalRecord;

#define GlobalCellCount CellCount(sizeof(GlobalRecord))

void initCell();

void markGlobals(gcSupportPo G);

#endif //CAFE_GLOBALSP_H
