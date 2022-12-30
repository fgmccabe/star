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

typedef void (*fiberFun)(fiberPo f,void *cl);

fiberPo createFiber(fiberFun fn, void *cl,integer size);
void dropFiber(fiberPo fiber);

void switchFiber(fiberPo next,void *val);

#endif //STAR_FIBER_H
