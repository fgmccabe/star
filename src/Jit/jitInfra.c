//
// Created by Francis McCabe on 4/1/20.
//

#include "jit.h"
#include <lower.h>
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
  jitComp->minOffset = - stackDelta(mtd);
  jitComp->maxOffset = mtdArity(mtd);
  return jitComp;
}

void clearJitContext(jitCompPo jit) {
  discardCtx(jit->assemCtx);
  freePool(contextPool, jit);
}

armReg findFreeReg(jitCompPo jit) {
  armReg rg = nxtAvailReg(jit->freeRegs);
  check(rg!=XZR, "no available registers");
  jit->freeRegs = dropReg(jit->freeRegs, rg);
  return rg;
}

retCode reserveReg(jitCompPo jit, armReg rg) {
  if (isRegInMap(jit->freeRegs, rg)) {
    jit->freeRegs = dropReg(jit->freeRegs, rg);
    return Ok;
  } else
    return Error;
}

void releaseReg(jitCompPo jit, armReg rg) {
  jit->freeRegs = freeReg(jit->freeRegs, rg);
}
