//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include "codeP.h"
#include "jitP.h"

integer jitThreshold = 1000;
logical jitOnLoad = False;

#ifdef TRACEJIT
tracingLevel traceJit = noTracing;
#endif

assemCtxPo assemCtx(jitCompPo jitCtx) {
  return jitCtx->assemCtx;
}

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space) {
  //  check(jitCtx->vTop >= amnt && jitCtx->vTop < NumberOf(jitCtx->vStack) - space, "stack out of bounds");
}

void markEntry(jitCompPo jit, codeLblPo entry) {
  assert(jit->entry == Null);
  jit->entry = entry;
}

codeLblPo jitEntry(jitCompPo jit) {
  return jit->entry;
}

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  jitCompPo jit = jitContext(mtd, errMsg, msgLen);

  retCode ret = jitInstructions(jit, mtd, errMsg, msgLen);

  if (ret == Ok) {
    assemCtxPo ctx = jit->assemCtx;
    ret = setJitCode(mtd, createCode(ctx), currentPc(ctx));
  }
  clearJitContext(jit);

  strMsg(errMsg, msgLen, "error in generating jit code");

  return ret;
}

