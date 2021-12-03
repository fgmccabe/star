//
// Created by Francis McCabe on 11/10/21.
//

#ifndef STAR_BIGNUMP_H
#define STAR_BIGNUMP_H

#include "bignum.h"
#include "multiP.h"
#include "heap.h"
#include "termP.h"

typedef struct bignum_term_ {
  clssPo clss;                  // == integerClass
  integer hash;
  integer count;
  uint32 data[ZEROARRAYSIZE];
} BignumRecord;

void initBignum();

#define BignumCellCount(len) CellCount(sizeof(BignumRecord)+(len)*sizeof(uint32))

extern termPo allocateBignum(heapPo H, integer count, uint32 data[]);

#endif //STAR_BIGNUMP_H
