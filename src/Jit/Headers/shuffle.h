//
// Created by Francis McCabe on 9/25/25.
//

#ifndef STAR_SHUFFLE_H
#define STAR_SHUFFLE_H

#include "jitP.h"

typedef struct argSpec_ {
  FlexOp src;
  FlexOp dst;
  logical mark;
  int32 group;
} ArgSpec,*argSpecPo;

void shuffleVars(assemCtxPo ctx, argSpecPo args, int32 arity, registerMap freeRegs);

#endif //STAR_SHUFFLE_H
