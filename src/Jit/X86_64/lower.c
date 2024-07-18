//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include "x86_64.h"

/* Lower Star VM code to X64 code */
/*
 *  First six arguments passed in registers:
 *  A0 = RDI
 *  A0 = RSI
 *  A0 = RDX
 *  A1 = RCX
 *  A2 = R8
 *  A3 = R9
 *
 *  RBP is the frame pointer
 *  R10 points to the literals tuple
 *
 *  RAX is the return value
 */

registerMap defltAvailRegSet() {
  return (1<<RAX)|(1<<RCX)|(1<RBX)|(1<<RSI)|(1<<RDI)|
    (1<<R8)|(1<<R9)|(1<<R10)|(1<<R11)|(1<<R12)|(1<<R13)|(1<<R14)|(1<<R15);
}


retCode jit_preamble(methodPo mtd, jitCompPo jitCtx) {
  return Error;
}

retCode jit_postamble(methodPo mtd, jitCompPo jitCtx) {
  return Error;
}

static int32 collectOperand(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

x64Op formX64Operand(vOperand v) {
  switch (v.loc) {
    case argument: {
      switch (v.ix) {
        case 0: {
          x64Op op = {.mode=Reg, .op.reg=RCX};
          return op;
        }
        case 1: {
          x64Op op = {.mode=Reg, .op.reg=RDX};
          return op;
        }
        case 2: {
          x64Op op = {.mode=Reg, .op.reg=R8};
          return op;
        }
        case 3: {
          x64Op op = {.mode=Reg, .op.reg=R9};
          return op;
        }
        default: {
          x64Op op = {.mode=Based, .op.based.base=RBP, .op.based.disp=(int) (v.ix * LONG_COUNT + FRAME_SIZE)};
          return op;
        }
      }
    }
    case literal: {
      x64Op op = {.mode=Based, .op.based.base=R10, .op.based.disp=(int) (v.ix * LONG_COUNT)};
      return op;
    }
    case local: {
      x64Op op = {.mode=Based, .op.based.base=RBP, .op.based.disp=(int) (-v.ix * LONG_COUNT)};
      return op;
    }
    case mcReg: {
      return v.mcLoc;
    }
  }
}

static x64Op popStkOp(jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand v = jitCtx->vStack[--jitCtx->vTop];
  return formX64Operand(v);
}

static void pushStkOp(jitCompPo jitCtx, x64Op operand) {
  verifyJitCtx(jitCtx, 0, 1);
  vOperand v = {.loc=mcReg, .mcLoc=operand};
  jitCtx->vStack[jitCtx->vTop++] = v;
}

retCode jit_Halt(insPo code,  vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Nop(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Abort(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Closure(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Alloc(insPo code, vOperand a1, vOperand a2,  integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Assign(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_LdA(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vStack[jitCtx->vTop++] = a1;
  return Ok;
}

retCode jit_LdL(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  int32 lxlNo = collectOperand(code, pc);
  vOperand entry = {.loc=local, .ix=lxlNo, .type=ptrTp};
  jitCtx->vStack[jitCtx->vTop++] = entry;
  return Ok;
}

retCode jit_LdC(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  int32 litNo = collectOperand(code, pc);
  vOperand entry = {.loc = literal, .ix=litNo};
  jitCtx->vStack[jitCtx->vTop++] = entry;
  return Ok;
}

retCode jit_LdG(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_LdV(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StV(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Nth(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StNth(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StA(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StG(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StL(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TL(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TG(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Thunk(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_LdTh(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StTh(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TTh(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Dup(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 1);
  jitCtx->vStack[jitCtx->vTop] = jitCtx->vStack[jitCtx->vTop - 1];
  jitCtx->vTop++;
  return Ok;
}

retCode jit_Drop(insPo code, vOperand a1, vOperand a2, integer *c, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vTop--;
  return Ok;
}

retCode jit_Swap(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand entry = jitCtx->vStack[jitCtx->vTop];
  jitCtx->vStack[jitCtx->vTop] = jitCtx->vStack[jitCtx->vTop - 1];
  jitCtx->vStack[jitCtx->vTop - 1] = entry;
  return Ok;
}

retCode jit_Rot(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Rst(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  int32 height = collectOperand(code, pc);
  check(height >= 0 && height <= jitCtx->vTop, "reset alignment");
  jitCtx->vTop = height;
  return Ok;
}

retCode jit_Call(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_OCall(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TCall(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TOCall(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Invoke(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Escape(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Ret(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_RtG(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_RetX(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Locals(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Frame(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Case(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IndxJmp(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Jmp(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cell(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Get(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FAdd(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FAbs(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FSub(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FMul(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FDiv(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FMod(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FEq(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FGe(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FCmp(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FLt(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IAdd(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  /* x64Op a1 = popStkOp(jitCtx); */
  /* x64Op a2 = popStkOp(jitCtx); */

  /* add(a1, a2, jitCtx->assemCtx); */
  /* pushStkOp(jitCtx, a1); */

  return Error;
}

retCode jit_IAbs(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ISub(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IMul(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IDiv(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IMod(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ICmp(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IEq(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IGe(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ILt(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CCmp(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CEq(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CGe(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CLt(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BAnd(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BOr(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BNot(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BXor(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BAsr(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BLsl(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BLsr(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CLbl(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Unpack(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cmp(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_If(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IfNot(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Fiber(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Spawn(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Suspend(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Resume(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Retire(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Release(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Underflow(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TEq(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Try(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Throw(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dBug(insPo code, vOperand a1, vOperand a2, integer *pc, jitCompPo jitCtx) {
  return Error;
}
