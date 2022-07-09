//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include "arm64.h"

/* Lower Star VM code to Arm64 code */
/*

 */

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

armOp formOperand(vOperand v) {
  switch (v.loc) {
    case argument: {
      switch (v.ix) {
        case 0: {
          armOp op = {.mode=Reg, .op.reg=X0};
          return op;
        }
        case 1: {
          armOp op = {.mode=Reg, .op.reg=X1};
          return op;
        }
        case 2: {
          armOp op = {.mode=Reg, .op.reg=X2};
          return op;
        }
        case 3: {
          armOp op = {.mode=Reg, .op.reg=X3};
          return op;
        }
        default: {
          armOp op = {.mode=Based, .op.based.base=X12, .op.based.disp=(int) (v.ix * LONG_COUNT + FRAME_SIZE)};
          return op;
        }
      }
    }
    case literal: {
      armOp op = {.mode=Based, .op.based.base=X11, .op.based.disp=(int) (v.ix * LONG_COUNT)};
      return op;
    }
    case local: {
      armOp op = {.mode=Based, .op.based.base=X12, .op.based.disp=(int) (-v.ix * LONG_COUNT)};
      return op;
    }
    case immediate: {
      armOp op = {.mode=Immediate, .op.imm=(int) (v.ix)};
      return op;
    }
    case mcReg: {
      return v.mcLoc;
    }
  }
}

static armOp popStkOp(jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand v = jitCtx->vStack[--jitCtx->vTop];
  return formOperand(v);
}

static void pushStkOp(jitCompPo jitCtx, armOp operand) {
  verifyJitCtx(jitCtx, 0, 1);
  vOperand v = {.loc=mcReg, .mcLoc=operand};
  jitCtx->vStack[jitCtx->vTop++] = v;
}

retCode jit_Nop(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Halt(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Abort(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Alloc(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Assign(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_LdA(insPo code, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  int32 argNo = collectOperand(code, pc);
  vOperand entry = {.loc=argument, .ix=argNo, .type=ptrTp};
  jitCtx->vStack[jitCtx->vTop++] = entry;
  return Ok;
}

retCode jit_LdL(insPo code, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  int32 lxlNo = collectOperand(code, pc);
  vOperand entry = {.loc=local, .ix=lxlNo, .type=ptrTp};
  jitCtx->vStack[jitCtx->vTop++] = entry;
  return Ok;
}

retCode jit_LdC(insPo code, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  int32 litNo = collectOperand(code, pc);
  vOperand entry = {.loc = literal, .ix=litNo};
  jitCtx->vStack[jitCtx->vTop++] = entry;
  return Ok;
}

retCode jit_LdG(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_LdV(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StV(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Nth(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StNth(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StA(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StG(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StL(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TL(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TG(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Dup(insPo code, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 1);
  jitCtx->vStack[jitCtx->vTop] = jitCtx->vStack[jitCtx->vTop - 1];
  jitCtx->vTop++;
  return Ok;
}

retCode jit_Drop(insPo code, integer *c, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vTop--;
  return Ok;
}

retCode jit_Swap(insPo code, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand entry = jitCtx->vStack[jitCtx->vTop];
  jitCtx->vStack[jitCtx->vTop] = jitCtx->vStack[jitCtx->vTop - 1];
  jitCtx->vStack[jitCtx->vTop - 1] = entry;
  return Ok;
}

retCode jit_Rst(insPo code, integer *pc, jitCompPo jitCtx) {
  int32 height = collectOperand(code, pc);
  check(height >= 0 && height <= jitCtx->vTop, "reset alignment");
  jitCtx->vTop = height;
  return Ok;
}

retCode jit_Call(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_OCall(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TCall(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TOCall(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Escape(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Ret(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_RetX(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_RtG(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Frame(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Case(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IndxJmp(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Jmp(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cell(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Get(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FAdd(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FAbs(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FSub(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FMul(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FDiv(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FMod(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FEq(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FGe(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FCmp(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FLt(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IAdd(insPo code, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  armOp a1 = popStkOp(jitCtx);
  armOp a2 = popStkOp(jitCtx);

//  add(a1, a2, jitCtx->assemCtx);
  pushStkOp(jitCtx, a1);

  return Error;
}

retCode jit_IAbs(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ISub(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IMul(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IDiv(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IMod(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ICmp(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IEq(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IGe(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ILt(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BAnd(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BOr(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BNot(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BXor(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BAsr(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BLsl(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BLsr(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CLbl(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Unpack(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cmp(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_If(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IfNot(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Task(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Suspend(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Resume(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Retire(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Release(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Underflow(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TEq(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dBug(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

