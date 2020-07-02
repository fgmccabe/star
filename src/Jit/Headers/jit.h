//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JIT_H
#define STAR_JIT_H

#include <code.h>
#include "config.h"
#include "ooio.h"

typedef retCode (*jitCode)();
typedef struct jit_compiler_ *jitCompPo;

void initJit();

#endif //STAR_JIT_H
