//
// Created by Francis McCabe on 4/1/20.
//

#include "jit.h"
#include "lower.h"
#include "jitP.h"

static poolPo contextPool = Null;

void initJit() {
  if (contextPool == Null) {
    contextPool = newPool(sizeof(JitCompilerContext), 8);
  }
}

jitCompPo jitContext(methodPo mtd) {
  jitCompPo jitComp = (jitCompPo) allocPool(contextPool);

  jitComp->mtd = mtd;
  jitComp->assemCtx = createCtx();
  jitComp->freeRegs = defltAvailRegSet();
  jitComp->minOffset = -stackDelta(mtd);
  jitComp->maxOffset = mtdArity(mtd);
  jitComp->lclCnt = lclCount(mtd);
  jitComp->arity = mtdArity(mtd);

  return jitComp;
}

void clearJitContext(jitCompPo jit) {
  discardCtx(jit->assemCtx);
  freePool(contextPool, jit);
}

mcRegister findFreeReg(jitCompPo jit) {
  mcRegister rg = nxtAvailReg(jit->freeRegs);
  check(rg!=XZR, "no available registers");
  jit->freeRegs = dropReg(jit->freeRegs, rg);
  return rg;
}

retCode reserveReg(jitCompPo jit, mcRegister rg) {
  if (isRegInMap(jit->freeRegs, rg)) {
    jit->freeRegs = dropReg(jit->freeRegs, rg);
    return Ok;
  } else
    return Error;
}

void releaseReg(jitCompPo jit, mcRegister rg) {
  jit->freeRegs = freeReg(jit->freeRegs, rg);
}

logical haveFreeReg(jitCompPo jit) {
  return jit->freeRegs!=emptyRegSet();
}
