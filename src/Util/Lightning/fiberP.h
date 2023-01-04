//
// Created by Francis McCabe on 12/28/22.
//

#ifndef STAR_FIBERP_H
#define STAR_FIBERP_H

#include "fiber.h"

typedef struct fiber_record_ {
  FiberState state;
  void *fiberMem;
  integer memSize;
  integer regs[64];
} FiberStruct;

#endif //STAR_FIBERP_H
