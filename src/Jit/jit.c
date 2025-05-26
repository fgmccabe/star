//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include "codeP.h"
#include "jitP.h"

integer jitThreshold = 1000;
logical jitOnLoad = False;

#ifdef TRACEJIT
tracingLevel traceJit =  noTracing;
#endif

assemCtxPo assemCtx(jitCompPo jitCtx) {
  return jitCtx->assemCtx;
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

  if (ret == Ok)
    return setJitCode(mtd, createCode(jit->assemCtx));
  clearJitContext(jit);

  strMsg(errMsg, msgLen, "error in generating jit code");

  return ret;
}


