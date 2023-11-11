//
// Created by Francis McCabe on 10/30/23.
//

#ifndef STAR_SPECIAL_H
#define STAR_SPECIAL_H

#include "logical.h"
#include "config.h"

typedef struct shapeStruct *shapePo;

typedef struct special_class *specialClassPo;

typedef enum{
  integerLbl,
  bigNumLbl,
  floatLbl,
  capabilityLbl,
  cellLbl,
  charLbl,
  closureLbl,
  codeLbl,
  continuationLbl,
  futureLbl,
  ioChannelLbl,
  labelLbl,
  lockLbl,
  stackLbl,
  stringLbl,
  specialLbl,
  specialCount
} specialIndex;

void initSpecial(specialIndex ix,specialClassPo special);
logical isSpecialLbl(int32 ix);
specialClassPo specialClassLbl(int32 ix);

#endif //STAR_SPECIAL_H
