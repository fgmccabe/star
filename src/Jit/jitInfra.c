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
  jitComp->vTop = 0;
  jitComp->assemCtx = createCtx();
  jitComp->usedRegs = 0;
  jitComp->freeRegs = defltAvailRegSet();
  return jitComp;
}

void clearJitContext(jitCompPo jit){
  discardCtx(jit->assemCtx);
  freePool(contextPool,jit);
}
