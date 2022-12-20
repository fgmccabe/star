//
// Created by Francis McCabe on 12/19/22.
//

#ifndef STAR_FIBER_H
#define STAR_FIBER_H

#include "integer.h"

typedef struct fiber_record_ *fiberPo;


typedef enum {
  suspended,
  active,
  moribund
} FiberState;

fiberPo createFiber(integer size);
void dropFiber(fiberPo fiber);

#endif //STAR_FIBER_H
