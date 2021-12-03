//
// Created by Francis McCabe on 11/13/21.
//

#ifndef STAR_BCDP_H
#define STAR_BCDP_H

#include "bcd.h"

typedef struct bcd_number_ {
  integer count;
  uint32 data[ZEROARRAYSIZE];
} BcdNumberRecord;

bcdPo allocBCD(integer count, uint32 *data);
uint64 bcd_add(uint32 a, uint32 b);

#endif //STAR_BCDP_H
