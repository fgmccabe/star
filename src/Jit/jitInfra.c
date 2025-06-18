//
// Created by Francis McCabe on 4/1/20.
//

#include "jit.h"
#include <lower.h>
#include "jitP.h"

static poolPo contextPool = Null;
static poolPo labelPool = Null;

void initJit() {
  if (contextPool == Null) {
    contextPool = newPool(sizeof(JitCompilerContext), 8);
    labelPool = newPool(sizeof(LabelMarkerRecord), 256);
  }
}

jitCompPo jitContext(methodPo mtd, char *errMsg, integer msgLen) {
  jitCompPo jitComp = (jitCompPo) allocPool(contextPool);

  jitComp->mtd = mtd;
  jitComp->assemCtx = createCtx();
  jitComp->freeRegs = defltAvailRegSet();
  jitComp->errMsg = errMsg;
  jitComp->msgLen = msgLen;
  jitComp->pcLocs = allocArray(sizeof(PcMapEntry), codeSize(mtd), True);

  return jitComp;
}

void clearJitContext(jitCompPo jit) {
  discardCtx(jit->assemCtx);
  freePool(contextPool, jit);
  if (jit->pcLocs != Null)
    jit->pcLocs = eraseArray(jit->pcLocs,Null,Null);
}

armReg findFreeReg(jitCompPo jit) {
  armReg rg = nxtAvailReg(jit->freeRegs);
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
