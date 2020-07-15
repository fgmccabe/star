//
// Created by Francis McCabe on 7/1/20.
//

#ifndef STAR_JITOPS_H
#define STAR_JITOPS_H

#include "jit.h"
#include "code.h"

#undef instruction

#define instruction(Op, A1, Dl, Cmt)    \
retCode jit_##Op(insPo code,integer *pc,opAndSpec OpAnd,jitCompPo context);

#include "instructions.h"

#endif //STAR_JITOPS_H
