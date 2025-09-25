//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JIT_H
#define STAR_JIT_H

#include <code.h>
#include "config.h"
#include "engine.h"
#include "escape.h"

typedef ReturnStatus (*jittedCode)();

typedef struct {
  jittedCode code;
  uint32 codeSize;
} CodeBlock;

typedef struct jit_compiler_ *jitCompPo;
void initJit();

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen);
retCode jitSpecial(methodPo mtd, char *errMsg, integer msgLen, int32 depth);

ReturnStatus invokeJitMethod(enginePo P, methodPo mtd);

#ifdef TRACEJIT
extern tracingLevel traceAssem; // Set if tracing assembler
extern tracingLevel traceJit;
#endif

extern logical jitOnLoad;
#endif //STAR_JIT_H
