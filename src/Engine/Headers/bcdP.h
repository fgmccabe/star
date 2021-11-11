//
// Created by Francis McCabe on 11/10/21.
//

#ifndef STAR_BCDP_H
#define STAR_BCDP_H

#include "bcd.h"
#include "heap.h"
#include "termP.h"

extern bcdPo C_BCD(termPo t);

typedef struct bcd_term_ {
  clssPo clss;                  // == integerClass
  integer length;               // Number of digits
  integer hash;
  byte data[ZEROARRAYSIZE];
} BcdRecord;

void initBCD();

#define BCDCellCount(len) CellCount(sizeof(BcdRecord)+(len)*sizeof(char))

extern termPo allocateBCD(heapPo H, integer count, byte data[]);

#endif //STAR_BCDP_H
