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
  ClassRecord clss;             // == integerClass
  uint32 count;
  uint32 data[ZEROARRAYSIZE];
} BignumRecord;

void initBignum();

#define BignumCellCount(len) CellCount(sizeof(BignumRecord)+(len)*sizeof(uint32))

termPo allocateBignum(heapPo H, uint32 count, uint32 data[]);
termPo bignumFromString(heapPo h, char *text, integer tLen);
#endif //STAR_BIGNUMP_H
