//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include "x86_64.h"

/* Lower Star VM code to X64 code */
/*
 *  First four arguments passed in registers:
 *  A0 = RCX
 *  A1 = RDX
 *  A2 = R8
 *  A3 = R9
 *
 *  RBP is the frame pointer
 *  R10 points to the literals tuple
 *
 *  RAX is the return value
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
          x64Op op = {.mode=Based, .op.based.base=RBP, .op.based.disp=(int)(v.ix * LONG_COUNT + FRAME_SIZE)};
          return op;
        }
      }
    }
    case literal:{
      x64Op op = {.mode=Based, .op.based.base=R10, .op.based.disp=(int)(v.ix * LONG_COUNT)};
      return op;
    }
    case local:{
      x64Op op = {.mode=Based, .op.based.base=RBP, .op.based.disp=(int)(-v.ix * LONG_COUNT)};
      return op;
    }
    case immediate:{
      x64Op op = {.mode=Immediate,  .op.imm=(int)(v.ix)};
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

retCode jit_Thnk(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ThGet(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ThSet(insPo code, integer *pc, jitCompPo jitCtx) {
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
  x64Op a1 = popStkOp(jitCtx);
  x64Op a2 = popStkOp(jitCtx);

  add(a1, a2, jitCtx->assemCtx);
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

retCode jit_CmpVd(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_If(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IfNot(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Tag(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cut(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Prompt(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Handle(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Throw(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Resume(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TResume(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Underflow(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dBreak(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dBug(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dLine(insPo code, integer *pc, jitCompPo jitCtx) {
  return Error;
}