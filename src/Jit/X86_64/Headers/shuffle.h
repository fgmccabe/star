//
// Created by Francis McCabe on 10/24/25.
//

#ifndef STAR_X86_64_SHUFFLE_H
#define STAR_X86_64_SHUFFLE_H

#include "sort.h"
#include "lower.h"

void shuffleVars(jitCompPo jit,
                 argSpecPo args,
                 int32 arity, registerMap* freeRegs);

#endif //STAR_X86_64_SHUFFLE_H
