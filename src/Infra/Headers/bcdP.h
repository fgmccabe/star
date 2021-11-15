//
// Created by Francis McCabe on 11/13/21.
//

#ifndef STAR_BCDP_H
#define STAR_BCDP_H

#include "bcd.h"

typedef struct bcd_number_ {
  sign sign;
  integer count;
  byte data[ZEROARRAYSIZE];
} BcdNumberRecord;

retCode fillinBCD(sign sign, integer count, integer size, byte *data, bcdPo bcd);
void trimBCD(bcdPo bcd);
bcdPo allocBCD(sign sign, integer count, byte *data);

#endif //STAR_BCDP_H
