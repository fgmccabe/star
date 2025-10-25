//
// Created by Francis McCabe on 10/24/25.
//

#ifndef STAR_SHUFFLE_H
#define STAR_SHUFFLE_H

#include "sort.h"

typedef void (*moveFunc)(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap *freeRegs);
void shuffleVars(assemCtxPo ctx,
                 argSpecPo args,
                 int32 arity,
                 registerMap* freeRegs,
                 moveFunc mover);

#endif //STAR_SHUFFLE_H
