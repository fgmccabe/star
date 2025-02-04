//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JIT_H
#define STAR_JIT_H

#include <code.h>
#include "config.h"
#include "ooio.h"
#include "term.h"
#include "stack.h"

typedef termPo (*jitCode)();

typedef termPo (*jitCode1)(stackPo stk, termPo arg);
typedef termPo (*jitCode2)(stackPo stk, termPo a1, termPo a2);
typedef termPo (*jitCode3)(stackPo stk, termPo a1, termPo a2, termPo a3);
typedef termPo (*jitCode4)(stackPo stk, termPo a1, termPo a2, termPo a3, termPo a4);
typedef termPo (*jitCodeStar)(stackPo stk, termPo a[]);

typedef integer (*Cfunc1)(integer arg1);
typedef integer (*Cfunc2)(integer a1, integer a2);
typedef integer (*Cfunc3)(integer a1, integer a2, integer a3);

typedef struct jit_compiler_ *jitCompPo;

void initJit();
retCode installJitCode(methodPo mtd, jitCode code);
retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen);
termPo invokeJitMethod(methodPo mtd, heapPo H, stackPo stk);

#ifdef TRACEJIT
extern logical traceJit;
#endif
extern integer jitThreshold;
extern logical jitOnLoad;
#endif //STAR_JIT_H
