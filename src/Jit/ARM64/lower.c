//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include <assert.h>
#include "stackP.h"
#include "macros.h"
#include "code.h"

/* Lower Star VM code to Arm64 code */

static retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta);
static const integer integerByteCount = (integer) sizeof(integer);

static retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun);
static retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun);
static retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun);

static retCode spillUpto(jitCompPo jit, integer depth);

retCode jit_preamble(methodPo mtd, jitCompPo jit) {
  integer frameSize = lclCount(mtd) * integerByteCount + (integer) sizeof(StackFrame);
  if (!isInt32(frameSize))
    return Error;
  integer stkDelta = stackDelta(mtd) * integerByteCount;
  if (!isInt32(stkDelta))
    return Error;

  assemCtxPo ctx = assemCtx(jit);
  codeLblPo entry = defineLabel(ctx, "entry", ctx->pc);
  markEntry(jit, entry);
  int32 stkAdjustment = ALIGNVALUE(frameSize, 16);

  stp(FP, X30, PRX(SP, -sizeof(StackFrame)));
  mov(FP, RG(SP));
  str(PL, OF(FP, OffsetOf(StackFrame, pool)));
  if (stkAdjustment != 0)
    sub(SP, SP, IM(stkAdjustment));

  stackCheck(jit, mtd, (int32) stkDelta);
  return Ok;
}

retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta) {
  int32 stkMemOffset = OffsetOf(StackRecord, stkMem);
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = defineLabel(ctx, "stackOk", undefinedPc);

  sub(X16, SP, IM(delta));
  ldr(X17, OF(X27, stkMemOffset));
  cmp(X16, RG(X17));
  bhi(okLbl);

  saveRegisters(ctx, nonSpillSet(codeArity(mtd)));

  mov(X0, RG(SB));
  mov(X1, IM(delta));
  mov(X2, IM((integer) mtd));
  tryRet(invokeCFunc3(jit, (Cfunc3) handleStackOverflow));
  mov(SB, RG(X0));

  restoreRegisters(ctx, nonSpillSet(codeArity(mtd)));

  setLabel(ctx, okLbl);
  return Ok;
}

retCode jit_postamble(methodPo mtd, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);

  add(SP, FP, IM(sizeof(StackFrame)));
  mov(PL, OF(FP, OffsetOf(StackFrame, pool)));
  ldp(FP, LR, PSX(SP, 16));
  ret(LR);
  return Ok;
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

retCode jit_Closure(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Alloc(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
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

retCode jit_Call(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jit) {
  assert(arg1.loc == literal);

  labelPo lbl = C_LBL(getMtdLit(jit->mtd, arg1.ix));
  integer arity = labelArity(lbl);

  spillUpto(jit, arity);    // Spill all stack arguments up until arity

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

retCode jit_Assign(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
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

retCode jit_EndTry(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Throw(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Reset(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Shift(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_Invoke(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode jit_dBug(insPo code, vOperand arg1, vOperand arg2, integer *pc, jitCompPo jitCtx) {
  return Error;
}

retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun) {
  return Error;
}

retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun) {
  return Error;
}

retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun) {
  return Error;
}

static retCode spillUpto(jitCompPo jit, integer depth) {
  assemCtxPo ctx = assemCtx(jit);

  for (integer ix = 0; ix < jit->vTop - depth; ix++) {
    operandPo entry = &jit->vStack[ix];

    switch (entry->loc) {
      case argument:
      case local:
      case constant:
      case global:
        continue;
      case stkOff: {

        continue;
      }
      case mcReg: {
        integer off = allocateLocal(jit, -1, -1, spilledVar);
        armReg Rg = entry->mcLoc.reg;
        str(Rg, OF(FP, off));
        entry->loc = local;
        entry->ix = off;
        continue;
      }
      default:
        check(False, "illegal source loc on stack");
        return Error;
    }
  }
  return Ok;
}
