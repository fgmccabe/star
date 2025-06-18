//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JIT_H
#define STAR_JIT_H

#include <code.h>
#include "config.h"
#include "term.h"
#include "engine.h"
#include "escape.h"

typedef termPo (*jittedCode)();

typedef integer (*Cfunc1)(integer arg1);
typedef integer (*Cfunc2)(integer a1, integer a2);
typedef integer (*Cfunc3)(integer a1, integer a2, integer a3);

typedef struct jit_compiler_ *jitCompPo;

void initJit();
retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen);
ReturnStatus invokeJitMethod(processPo P, methodPo mtd);

#ifdef TRACEJIT
extern tracingLevel traceJit;
#endif
extern integer jitThreshold;
extern logical jitOnLoad;
#endif //STAR_JIT_H
