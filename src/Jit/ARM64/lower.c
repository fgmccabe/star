//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include "arm64.h"
#include <assert.h>

/* Lower Star VM code to Arm64 code */


retCode jit_preamble(methodPo mtd, jitCompPo jitCtx) {
  return Error;
}

retCode jit_postamble(methodPo mtd, jitCompPo jitCtx) {
  return Error;
}

static vOperand popStkOp(jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  return jitCtx->vStack[--jitCtx->vTop];
}

static void pushStkOp(jitCompPo jitCtx, vOperand operand) {
  verifyJitCtx(jitCtx, 0, 1);
  jitCtx->vStack[jitCtx->vTop++] = operand;
}

retCode jit_Nop(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Ok;
}

retCode jit_Halt(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Abort(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Alloc(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Assign(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_LdA(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vStack[jitCtx->vTop++] = arg1;
  return Ok;
}

retCode jit_LdL(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vStack[jitCtx->vTop++] = arg1;
  return Ok;
}

retCode jit_LdC(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vStack[jitCtx->vTop++] = arg1;
  return Ok;
}

retCode jit_LdG(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
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

retCode jit_LdV(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StV(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Nth(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StNth(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StA(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StG(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_StL(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TL(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TG(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Dup(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 1);
  jitCtx->vStack[jitCtx->vTop] = jitCtx->vStack[jitCtx->vTop - 1];
  jitCtx->vTop++;
  return Ok;
}

retCode jit_Drop(insPo code, vOperand arg1, vOperand arg2, integer *c, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  jitCtx->vTop--;
  return Ok;
}

retCode jit_Swap(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand entry = jitCtx->vStack[jitCtx->vTop];
  jitCtx->vStack[jitCtx->vTop] = jitCtx->vStack[jitCtx->vTop - 1];
  jitCtx->vStack[jitCtx->vTop - 1] = entry;
  return Ok;
}

retCode jit_Rst(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  assert(arg1.loc == literal);
  int32 height = (int32) (arg1.ix);
  check(height >= 0 && height <= jitCtx->vTop, "reset alignment");
  jitCtx->vTop = height;
  return Ok;
}

retCode jit_Rot(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  assert(arg1.loc == literal);
  int32 height = (int32) (arg1.ix);
  check(height >= 0 && height <= jitCtx->vTop, "rotation amount");
  vOperand top = jitCtx->vStack[jitCtx->vTop];
  for (int32 ix = 0; ix < height - 1; ix++) {
    jitCtx->vStack[jitCtx->vTop - ix] = jitCtx->vStack[jitCtx->vTop - height - ix];
  }
  jitCtx->vStack[jitCtx->vTop - height] = top;
  return Ok;
}

retCode jit_Call(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_OCall(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TCall(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TOCall(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Invoke(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Locals(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Escape(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Ret(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_RetX(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_RtG(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Frame(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Case(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IndxJmp(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Jmp(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cell(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Get(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FAdd(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FAbs(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FSub(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FMul(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FDiv(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FMod(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FEq(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FGe(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FCmp(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_FLt(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IAdd(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand a1 = popStkOp(jitCtx);
  vOperand a2 = popStkOp(jitCtx);

  assemCtxPo cxt = assemCtx(jitCtx);

//  add(a1, a2, jitCtx->assemCtx);
  pushStkOp(jitCtx, a1);

  return Error;
}

retCode jit_IAbs(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ISub(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IMul(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IDiv(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IMod(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ICmp(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IEq(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IGe(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_ILt(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BAnd(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BOr(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BNot(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BXor(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BAsr(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BLsl(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_BLsr(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_CLbl(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Unpack(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Cmp(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_If(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_IfNot(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Fiber(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Spawn(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Suspend(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Resume(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Retire(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Release(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Underflow(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_TEq(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Try(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Throw(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dBug(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

registerMap defltAvailRegSet() {
  return callerSaved() | calleeSaved() | stackRegs();
}

registerMap allocReg(registerMap from, armReg Rg) {
  check((from & (1u << Rg)) != 0, "register not free");
  return (from & (~(1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  check((from & (1u << Rg)) != 0, "register already free");
  return (from | (1u << Rg));
}

armReg nxtAvailReg(registerMap from) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = 1u << ix;
    if ((from & mask) != 0)
      return ix;
  }
  return XZR;
}
