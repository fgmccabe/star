//
// Created by Francis McCabe on 10/2/21.
//

#ifndef STAR_THUNKP_H
#define STAR_THUNKP_H

#include "thunk.h"
#include "termP.h"
#include "heapP.h"

typedef struct thunk_rec_ {
  clssPo clss;                  // == thunkClass
  termPo content;               // Contents
  normalPo lam;
  integer hash;
} ThunkRecord;

#define ThunkCellCount CellCount(sizeof(ThunkRecord))

extern void initThunk();

#endif //STAR_THUNKP_H
