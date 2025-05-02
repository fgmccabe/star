//
// Created by Francis McCabe on 8/19/24.
//

#ifndef STAR_FIBER_H
#define STAR_FIBER_H

#include "retcode.h"

// Fiber structure. Allow code to switch between coroutines

typedef enum {
  active,
  suspended,
  moribund
} fiberStatus;

typedef struct fiber_record_ *fiberPo;
typedef retCode (*fiberFun)(fiberPo thisFiber, void *cl);

retCode suspendFiber(fiberPo thisFiber, fiberStatus msg, void *data);
retCode resumeFiber(fiberPo fiber, fiberStatus msg, void *data);
retCode retireFiber(fiberPo thisFbier, fiberStatus ret, void *data);

#endif //STAR_FIBER_H
