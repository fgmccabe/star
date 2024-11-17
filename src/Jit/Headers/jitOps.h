//
// Created by Francis McCabe on 7/1/20.
//

#ifndef STAR_JITOPS_H
#define STAR_JITOPS_H

#include "jit.h"
#include "code.h"

#undef instruction
#define instruction(Op,A1,A2,Dl,_,Cmt)    \
retCode jit_##Op(insPo code, integer pc, jitCompPo jitCtx);
#include "instructions.h"
#undef instruction

#endif //STAR_JITOPS_H
