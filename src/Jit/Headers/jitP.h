//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JITP_H
#define STAR_JITP_H

#include <config.h>
#include "jit.h"
#include "array.h"
#include "macros.h"
#include "code.h"
#include "infra.h"

typedef struct jit_compiler_ {
  methodPo mtd;
  int32 arity;
  int32 lclCnt;
  registerMap freeRegs;
  assemCtxPo assemCtx;
  int32 minOffset;
  int32 maxOffset;
  char errMsg[MAXLINE];
} JitCompilerContext;

assemCtxPo assemCtx(jitCompPo jitCtx);

jitCompPo jitContext(methodPo mtd);
void clearJitContext(jitCompPo ctx);
void clearCodeCtxMaps(assemCtxPo ctx);;

jittedCode createCode(assemCtxPo ctx);

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space);

retCode reserveReg(jitCompPo jit, mcRegister rg);
mcRegister findFreeReg(jitCompPo jit);
void releaseReg(jitCompPo jit, mcRegister rg);

logical isByte(int64 x);
logical isI32(int64 x);

retCode jitInstructions(jitCompPo jitCtx, methodPo mtd, char *errMsg, integer msgLen);
retCode jitSpecialInstructions(jitCompPo jit, methodPo mtd, int32 depth) ;

#endif //STAR_JITP_H
