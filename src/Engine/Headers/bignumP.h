//
// Created by Francis McCabe on 11/10/21.
//

#ifndef STAR_BIGNUMP_H
#define STAR_BIGNUMP_H

#include "bignum.h"
#include "multi.h"
#include "heap.h"
#include "termP.h"
#include "bcdP.h"

extern bignumPo C_BIGNUM(termPo t);

typedef struct bignum_term_ {
  clssPo clss;                  // == integerClass
  integer hash;
  integer count;
  byte data[ZEROARRAYSIZE];
} BignumRecord;

void initBignum();

#define BignumCellCount(len) CellCount(sizeof(BignumRecord)+(len)*sizeof(byte))

extern termPo allocateBignum(heapPo H, integer count, byte data[]);

#endif //STAR_BIGNUMP_H
