//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include "codeP.h"
#include "jitP.h"

logical jitOnLoad = True;

#ifdef TRACEJIT
tracingLevel traceJit = noTracing;
#endif

assemCtxPo assemCtx(jitCompPo jitCtx) {
  return jitCtx->assemCtx;
}

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space) {
  //  check(jitCtx->vTop >= amnt && jitCtx->vTop < NumberOf(jitCtx->vStack) - space, "stack out of bounds");
}

logical jitStackHasRoom(jitCompPo jit, int32 amnt) {
  return jit->stackDepth - amnt >= 0;
}

int32 jitStackDepth(jitCompPo jit) {
  return jit->stackDepth;
}

int32 jitTrueStackDepth(jitCompPo jit) {
  return lclCount(jit->mtd) + jit->stackDepth;
}

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  if (!hasJit(mtd)) {
    jitCompPo jit = jitContext(mtd);

#ifdef TRACEJIT
    if (traceJit)
      dRegisterMap(jit->freeRegs);
#endif

    retCode ret = jitInstructions(jit, mtd, errMsg, msgLen);

    if (ret == Ok) {
      assemCtxPo ctx = jit->assemCtx;
      ret = setJitCode(mtd, createCode(ctx), currentPc(ctx));
    } else
      strMsg(errMsg, msgLen, "error: %S in generating jit code for %M", jit->errMsg, uniStrLen(jit->errMsg),
             mtdLabel(mtd));

    clearJitContext(jit);

    return ret;
  }
  return Ok;
}

retCode jitSpecial(methodPo mtd, char *errMsg, integer msgLen, int32 depth) {
  if (!hasJit(mtd)) {
    jitCompPo jit = jitContext(mtd);
    jit->stackDepth = depth;

#ifdef TRACEJIT
    if (traceJit)
      dRegisterMap(jit->freeRegs);
#endif

    retCode ret = jitInstructions(jit, mtd, errMsg, msgLen);

    if (ret == Ok) {
      assemCtxPo ctx = jit->assemCtx;
      ret = setJitCode(mtd, createCode(ctx), currentPc(ctx));
    } else
      strMsg(errMsg, msgLen, "error: %S in generating jit code", jit->errMsg, uniStrLen(jit->errMsg));

    clearJitContext(jit);

    return ret;
  }
  return Ok;
}
