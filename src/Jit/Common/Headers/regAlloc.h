//
// Created by Francis McCabe on 3/12/25.
//

#ifndef STAR_REGALLOC_H
#define STAR_REGALLOC_H

#include "config.h"
#include "ooio.h"

typedef struct {
  integer varNo;
  int32 firstIns;
  int32 lastIns;
} RangeRecord, *rangePo;

typedef struct {
  int32 varId;
  rangePo range;
} VarSpec, *varSpecPo;

void initRegAlloc();

#endif //STAR_REGALLOC_H
