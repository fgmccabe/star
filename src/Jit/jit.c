//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include <turm.h>
#include "codeP.h"
#include "lower.h"
#include "jitP.h"
#include "jitOps.h"

integer jitThreshold = 1000;
logical jitOnLoad = False;

#ifdef TRACEJIT
logical traceJit = False;
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
  jitCompPo jit = jitContext(mtd);

  retCode ret = jit_preamble(mtd, jit);

  if (ret == Ok)
    ret = jitInstructions(jit, entryPoint(mtd), codeSize(mtd), errMsg, msgLen);

  if (ret == Ok)
    ret = jit_postamble(mtd, jit);

  if (ret == Ok)
    return setJitCode(mtd, createCode(jit->assemCtx));
  clearJitContext(jit);

  strMsg(errMsg, msgLen, "error in generating jit code");

  return ret;
}

retCode jitInstructions(jitCompPo jitCtx, insPo code, integer insCount, char *errMsg, integer msgLen) {
  retCode ret = Ok;

  for (integer pc = 0; ret == Ok && pc < insCount; pc++) {
    switch (ins[pc].op) {

#define instruction(Op, A1, A2, Dl, Tp, Cmt)    \
    case Op:{                                   \
      ret = jit_##Op(ins,pc,jitCtx);            \
      break;                                    \
    }

#include "instructions.h"

#undef instruction

      default:
        return Error;
    }
  }

  return ret;
}

termPo invokeJitMethod(methodPo mtd, heapPo H, stackPo stk) {
  switch (codeArity(mtd)) {
    case 0:
      return codeJit(mtd)();
    case 1:
      return ((jitCode1) codeJit(mtd))(stk, topStack(stk));
    case 2:
      return ((jitCode2) codeJit(mtd))(stk, topStack(stk), peekStack(stk, 1));
    case 3:
      return ((jitCode3) codeJit(mtd))(stk, topStack(stk), peekStack(stk, 1), peekStack(stk, 2));
    case 4:
      return ((jitCode4) codeJit(mtd))(stk, topStack(stk), peekStack(stk, 1), peekStack(stk, 2), peekStack(stk, 3));
    default: {
      integer arity = codeArity(mtd);
      termPo args[arity];
      for (integer ix = 0; ix < arity; ix++) {
        args[ix] = peekStack(stk, ix);
      }
      return ((jitCodeStar) codeJit(mtd))(stk, args);
    }
  }
}
