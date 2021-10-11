//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lower.h"
#include "jitP.h"
#include "jitOps.h"
#include "x86_64.h"

retCode jit_preamble(methodPo mtd, jitCompPo context) {
  return Error;
}

retCode jit_postamble(methodPo mtd, jitCompPo context) {
  return Error;
}

static int32 collectOperand(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

x64Reg popStkIReg(jitCompPo context) {
  check(context->vTop > 0, "stack out of bounds");
  vOperand v = context->vStack[context->vTop--];
  switch (v.loc) {
    case argument:
    case local:
    case literal:
    case immediate:
      return 0;
    case mcReg:
      return v.regNo;
  }
}

retCode jit_Halt(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Abort(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Alloc(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Assign(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_LdA(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  int32 lxlNo = collectOperand(code, pc);
  vOperand entry = {.loc=argument, .ix=lxlNo, .type=ptrTp, .litrl=Null};
  context->vStack[context->vTop++] = entry;
  return Ok;
}

retCode jit_LdL(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  int32 lxlNo = collectOperand(code, pc);
  vOperand entry = {.loc=local, .ix=lxlNo, .type=ptrTp, .litrl=Null};
  context->vStack[context->vTop++] = entry;
  return Ok;
}

retCode jit_LdC(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  int32 litNo = collectOperand(code, pc);
  termPo lit = getMtdLit(context->mtd, litNo);
  vOperand entry = {.loc = literal, .ix=litNo, .litrl=lit};
  context->vStack[context->vTop++] = entry;
  return Ok;
}

retCode jit_LdG(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_LdV(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_StV(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Nth(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_StNth(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_StA(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_StG(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_StL(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_TL(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_TG(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Dup(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop > 0 && context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  context->vStack[context->vTop] = context->vStack[context->vTop - 1];
  context->vTop++;
  return Ok;
}

retCode jit_Drop(insPo code, integer *c, jitCompPo context) {
  check(context->vTop > 0, "stack empty");
  context->vTop--;
  return Ok;
}

retCode jit_Swap(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop > 0, "stack empty");
  vOperand entry = context->vStack[context->vTop];
  context->vStack[context->vTop] = context->vStack[context->vTop - 1];
  context->vStack[context->vTop - 1] = entry;
  return Ok;
}

retCode jit_Rst(insPo code, integer *pc, jitCompPo context) {
  int32 height = collectOperand(code, pc);
  check(height >= 0 && height <= context->vTop, "reset alignment");
  context->vTop = height;
  return Ok;
}

retCode jit_Call(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_OCall(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_TCall(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_TOCall(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Escape(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Ret(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Thnk(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_ThGet(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_ThSet(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Frame(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Case(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IndxJmp(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Jmp(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Cell(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Get(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FAdd(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FAbs(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FSub(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FMul(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FDiv(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FMod(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FEq(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FGe(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FCmp(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_FLt(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IAdd(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop > 1, "stack empty");
  x64Reg a1 = popStkIReg(context);
  x64Reg a2 = popStkIReg(context);

  return Error;
}

retCode jit_IAbs(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_ISub(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IMul(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IDiv(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IMod(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_ICmp(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IEq(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IGe(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_ILt(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BAnd(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BOr(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BNot(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BXor(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BAsr(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BLsl(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_BLsr(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_CLbl(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Unpack(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Cmp(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_CmpVd(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_If(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_IfNot(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Tag(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Cut(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Prompt(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Handle(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Throw(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Resume(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_TResume(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_Underflow(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_dBreak(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_dBug(insPo code, integer *pc, jitCompPo context) {
  return Error;
}

retCode jit_dLine(insPo code, integer *pc, jitCompPo context) {
  return Error;
}
