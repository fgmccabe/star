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

int32 collectOperand(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

insPo collectTgt(insPo base, integer *pc){
  int32 off = collectOperand(base,pc);
  return base+off;
}

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  insPo ins = entryPoint(mtd);
  integer len = insCount(mtd);
  jitCompPo jitCtx = jitContext(mtd);

  retCode ret = jit_preamble(mtd, jitCtx);

  for (integer pc = 0; ret == Ok && pc < len;) {
    switch (ins[pc++]) {
#undef instruction
#define instruction(Op, A1, A2, Dl, Cmt)                 \
    case Op:{                                            \
      tryRet(nextOperand(ins, &pc, A1, jitCtx,errMsg,msgLen)); \
      ret = nextOperand(ins, &pc, A2, jitCtx,errMsg,msgLen);   \
      break;                                             \
    }
#include "instructions.h"
#undef instruction
      default:
        return Error;
    }
  }

  // Sort the labels into order of occurrance
  if(ret==Ok)
    ret = sortLabels(jitCtx);

  for (integer pc = 0; ret == Ok && pc < len;) {
    switch (ins[pc++]) {

#define instruction(Op, A1, A2, Dl, Cmt)        \
    case Op:{                                   \
      tryRet(resolvePcLbl(ins, pc-1,jitCtx,errMsg,msgLen));  \
      ret = jit_##Op(ins,&pc,jitCtx);           \
      break;                                    \
    }

#include "instructions.h"

#undef instruction

      default:
        return Error;
    }
  }

  if (ret == Ok)
    ret = jit_postamble(mtd, jitCtx);

  if (ret == Ok)
    return setJitCode(mtd, createCode(jitCtx->assemCtx));
  clearJitContext(jitCtx);

  strMsg(errMsg, msgLen, "error in generating jit code");

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
