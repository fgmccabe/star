//
// Created by Francis McCabe on 1/6/26.
//

#ifndef STAR_DISASSP_H
#define STAR_DISASSP_H

#include "disass.h"

typedef enum {
  smeIns,
  sveIns,
  dpIns,
  brnchIns,
  dpReg,
  dpScalar,
  ldStrs
} instructionClass;

#endif //STAR_DISASSP_H
