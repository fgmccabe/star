//
// Created by Francis McCabe on 1/14/20.
//

#ifndef STAR_MEMOP_H
#define STAR_MEMOP_H

#include "memo.h"
#include "termP.h"
#include "heapP.h"

typedef struct memo_rec_ {
  clssPo clss;                  // == memoClass
  integer hash;
  termPo content;               // Contents
  normalPo provider;              // Function that implements the value
} MemoRecord;

extern void initMemo();

#define MemoCellCount CellCount(sizeof(MemoRecord))

#endif //STAR_MEMOP_H
