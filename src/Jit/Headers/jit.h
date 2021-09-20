//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JIT_H
#define STAR_JIT_H

#include <code.h>
#include "config.h"
#include "ooio.h"

typedef termPo (*jitCode)();

typedef termPo (*jitCode1)(termPo arg);
typedef termPo (*jitCode2)(termPo a1, termPo a2);
typedef termPo (*jitCode3)(termPo a1, termPo a2, termPo a3);
typedef termPo (*jitCode4)(termPo a1, termPo a2, termPo a3, termPo a4);
typedef termPo (*jitCode5)(termPo a1, termPo a2, termPo a3, termPo a4, termPo a5);
typedef termPo (*jitCode6)(termPo a1, termPo a2, termPo a3, termPo a4, termPo a5, termPo a6);
typedef termPo (*jitCode7)(termPo a1, termPo a2, termPo a3, termPo a4, termPo a5, termPo a6, termPo a7);
typedef termPo (*jitCode8)(termPo a1, termPo a2, termPo a3, termPo a4, termPo a5, termPo a6, termPo a7, termPo a8);

typedef struct jit_compiler_ *jitCompPo;

void initJit();
retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen);

#endif //STAR_JIT_H
