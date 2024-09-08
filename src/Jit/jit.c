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

static int32 collectOperand(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

vOperand argnOp(insPo base, integer *pc, jitCompPo jitCtx) {
  return (vOperand) {.loc=noWhere};
}

vOperand argtOs(insPo base, integer *pc, jitCompPo jitCtx) {
  return (vOperand) {.loc=stkOff, .ix=0};
}

vOperand argtO1(insPo base, integer *pc, jitCompPo jitCtx) {
  return (vOperand) {.loc=stkOff, .ix=1};
}

vOperand argi32(insPo base, integer *pc, jitCompPo jitCtx) {
  integer lit = collectOperand(base, pc);
  return (vOperand) {.loc=literal, .ix=lit};
}

vOperand argart(insPo base, integer *pc, jitCompPo jitCtx) {
  integer lit = collectOperand(base, pc);
  return (vOperand) {.loc=literal, .ix=lit};
}

vOperand argarg(insPo base, integer *pc, jitCompPo jitCtx) {
  integer arg = collectOperand(base, pc);
  return (vOperand) {.loc=argument, .ix=arg};
}

vOperand arglcl(insPo base, integer *pc, jitCompPo jitCtx) {
  integer off = collectOperand(base, pc);
  return (vOperand) {.loc=local, .ix=off};
}

vOperand arglcs(insPo base, integer *pc, jitCompPo jitCtx) {
  integer off = collectOperand(base, pc);
  return (vOperand) {.loc=local, .ix=off};
}

vOperand argoff(insPo base, integer *pc, jitCompPo jitCtx) {
  integer off = collectOperand(base, pc);
  return (vOperand) {.loc=codeOff, .ix=*pc + off};
}

vOperand argEs(insPo base, integer *pc, jitCompPo jitCtx) {
  integer esc = collectOperand(base, pc);
  return (vOperand) {.loc=escapeNo, .ix=esc};
}

vOperand arglit(insPo base, integer *pc, jitCompPo jitCtx) {
  integer lit = collectOperand(base, pc);
  return (vOperand) {.loc=literal, .ix=lit};
}

vOperand argsym(insPo base, integer *pc, jitCompPo jitCtx) {
  integer lit = collectOperand(base, pc);
  return (vOperand) {.loc=literal, .ix=lit};
}

vOperand argglb(insPo base, integer *pc, jitCompPo jitCtx) {
  integer lit = collectOperand(base, pc);
  return (vOperand) {.loc=global, .ix=lit};
}

vOperand argtPe(insPo base, integer *pc, jitCompPo jitCtx) {
  integer lit = collectOperand(base, pc);
  return (vOperand) {.loc=literal, .ix=lit};
}

#undef instruction
#define instruction(Op, A1, A2, Dl, Cmt)        \
    case Op:{                                   \
      vOperand Arg1 = arg##A1(ins,&pc,jitCtx);  \
      vOperand Arg2 = arg##A2(ins,&pc,jitCtx);  \
      ret = jit_##Op(ins,Arg1,Arg2,&pc,jitCtx); \
      break;                                    \
    }

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  insPo ins = entryPoint(mtd);
  integer len = insCount(mtd);
  integer pc = 0;
  jitCompPo jitCtx = jitContext(mtd);

  retCode ret = jit_preamble(mtd, jitCtx);

  while (ret == Ok && pc < len) {
    switch (ins[pc++]) {
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
