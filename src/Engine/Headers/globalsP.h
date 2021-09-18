//
// Created by Francis McCabe on 4/5/18.
//

#ifndef STAR_GLOBALSP_H
#define STAR_GLOBALSP_H

#include "globals.h"
#include "termP.h"
#include "heapP.h"


typedef struct global_rec_ {
  clssPo clss;                  // == globalClass
  integer varNo;
  integer hash;
  termPo content;               // Contents
  char *name;
} GlobalRecord;

#define GlobalCellCount CellCount(sizeof(GlobalRecord))

void markGlobals(gcSupportPo G);

#endif //STAR_GLOBALSP_H
