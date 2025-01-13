//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include "stackP.h"
#include "globals.h"
#include "signals.h"
#include "jitP.h"
#include "codeP.h"

/* Lower Star VM code to Arm64 code */

static retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta);
static const integer integerByteCount = (integer) sizeof(integer);

static retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun);
static retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun);
static retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun);

static retCode spillUpto(jitCompPo jit, integer depth);
static retCode loadStackIntoArgRegisters(jitCompPo jit, integer arity);

retCode jit_preamble(methodPo mtd, jitCompPo jit) {
  integer frameSize = lclCount(mtd) * integerByteCount + (integer) sizeof(StackFrame);
  if (!isInt32(frameSize))
    return Error;
  integer stkDelta = stackDelta(mtd) * integerByteCount;
  if (!isInt32(stkDelta))
    return Error;

  assemCtxPo ctx = assemCtx(jit);
  codeLblPo entry = defineLabel(ctx, ctx->pc);
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
  codeLblPo okLbl = defineLabel(ctx, undefinedPc);

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

static vOperand popStkOp(jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  return jit->vStack[--jit->vTop];
}

static void pushStkOp(jitCompPo jit, vOperand operand) {
  verifyJitCtx(jit, 0, 1);
  jit->vStack[jit->vTop++] = operand;
}

retCode jit_Nop(insPo code, integer pc, jitCompPo jit) {
  return Ok;
}

retCode jit_Halt(insPo code, integer pc, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  integer errCode = code[pc].fst;
  return callIntrinsic(ctx, (libFun) star_exit, 1, IM(errCode));
}

retCode jit_Abort(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Closure(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Alloc(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Entry(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_LdA(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  int32 argNo = code[pc].fst;
  vOperand argOp = {.loc=argument, .ix=argNo};

  jit->vStack[jit->vTop++] = argOp;

  return Ok;
}

retCode jit_LdL(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  int32 lclNo = code[pc].fst;
  vOperand lclOp = {.loc=local, .ix=lclNo};
  jit->vStack[jit->vTop++] = lclOp;
  return Ok;
}

retCode jit_LdS(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_LdC(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  int32 litNo = code[pc].fst;
  vOperand lclOp = {.loc=constant, .ix=litNo};
  jit->vStack[jit->vTop++] = lclOp;
  return Ok;
}

retCode jit_LdG(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_LdV(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);

  vOperand vdOp = {.loc=engineSymbol, .address=voidEnum};
  jit->vStack[jit->vTop++] = vdOp;
  return Ok;
}

retCode jit_StV(insPo code, integer pc, jitCompPo jit) {
  int32 lclNo = code[pc].fst;
  return Error;
}

retCode jit_Nth(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_StNth(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_StG(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_StL(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TL(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TG(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Sav(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TstSav(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_LdSav(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_StSav(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TSav(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Dup(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 1);
  jit->vStack[jit->vTop] = jit->vStack[jit->vTop - 1];
  jit->vTop++;
  return Ok;
}

retCode jit_Drop(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  jit->vTop--;
  return Ok;
}

retCode jit_Swap(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  vOperand entry = jit->vStack[jit->vTop];
  jit->vStack[jit->vTop] = jit->vStack[jit->vTop - 1];
  jit->vStack[jit->vTop - 1] = entry;
  return Ok;
}

retCode jit_Rst(insPo code, integer pc, jitCompPo jit) {
  int32 height = code[pc].fst;
  check(height >= 0 && height <= jit->vTop, "reset alignment");
  jit->vTop = height;
  return Ok;
}

retCode jit_Rot(insPo code, integer pc, jitCompPo jit) {
  int32 height = code[pc].fst;
  check(height >= 0 && height <= jit->vTop, "rotation amount");
  vOperand top = jit->vStack[jit->vTop];
  for (int32 ix = 0; ix < height - 1; ix++) {
    jit->vStack[jit->vTop - ix] = jit->vStack[jit->vTop - height - ix];
  }
  jit->vStack[jit->vTop - height] = top;
  return Ok;
}

retCode jit_Pick(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Call(insPo code, integer pc, jitCompPo jit) {
  int32 litNo = code[pc].fst;

  labelPo lbl = C_LBL(getMtdLit(jit->mtd, litNo));
  integer arity = labelArity(lbl);

  spillUpto(jit, arity);    // Spill all stack arguments up until arity

  return Error;
}

retCode jit_OCall(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TCall(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TOCall(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Locals(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Escape(insPo code, integer pc, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  int32 escNo = code[pc].fst;
  escapePo esc = getEscape(escNo);

  spillUpto(jit, escapeArity(esc));    // Spill all stack arguments up until arity
  loadStackIntoArgRegisters(jit, escapeArity(esc));
  codeLblPo escLbl = defineLabel(ctx, (integer) escapeFun(esc));
  bl(escLbl);

  return Error;
}

retCode jit_Ret(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Frame(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Block(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Break(insPo code, integer pc, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo tgt = getLblByPc(&code[pc], code[pc].alt, jit);

  assert(tgt != Null);

  b(tgt);
  return Ok;
}

retCode jit_Result(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Loop(insPo code, integer pc, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo tgt = getLblByPc(&code[pc], code[pc].alt, jit);

  assert(tgt != Null);

  b(tgt);
  return Ok;
}

retCode jit_Case(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IndxJmp(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Cell(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Get(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Assign(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FAdd(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FAbs(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FSub(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FMul(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FDiv(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FMod(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FEq(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FGe(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FCmp(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_FLt(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IAdd(insPo code, integer pc, jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  vOperand a1 = popStkOp(jit);
  vOperand a2 = popStkOp(jit);

  assemCtxPo cxt = assemCtx(jit);

//  add(a1, a2, jit->assemCtx);
  pushStkOp(jit, a1);

  return Error;
}

retCode jit_IAbs(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_ISub(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IMul(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IDiv(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IMod(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_ICmp(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IEq(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IGe(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_ILt(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_CCmp(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_CEq(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_CGe(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_CLt(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BAnd(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BOr(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BNot(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BXor(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BAsr(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BLsl(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_BLsr(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_CLit(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_CLbl(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Unpack(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Cmp(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_If(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_IfNot(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Fiber(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Spawn(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Suspend(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Resume(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Retire(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Release(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Underflow(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Try(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_EndTry(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_TryRslt(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Throw(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Reset(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Shift(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Invoke(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_dBug(insPo code, integer pc, jitCompPo jit) {
  return Error;
}

retCode jit_Line(insPo code, integer pc, jitCompPo jit) {
  return Ok;
}

retCode jit_Local(insPo code, integer pc, jitCompPo jit) {
  return Ok;
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

retCode spillUpto(jitCompPo jit, integer depth) {
  assemCtxPo ctx = assemCtx(jit);

  for (integer ix = 0; ix < jit->vTop - depth; ix++) {
    operandPo entry = &jit->vStack[ix];

    switch (entry->loc) {
      case argument:
      case local:
      case constant:
      case global:
        continue;
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

retCode loadStackIntoArgRegisters(jitCompPo jit, integer arity) {
  return Error;
}

retCode allocateStructure(clssPo clss, FlexOp amnt, armReg dst, jitCompPo jit) {
  uint32 currOff = OffsetOf(HeapRecord, curr);
  uint32 limitOff = OffsetOf(HeapRecord, limit);
  assemCtxPo ctx = assemCtx(jit);
  armReg glbHeap = findFreeReg(jit);
  armReg scratch = findFreeReg(jit);
  armReg limit = findFreeReg(jit);

  codeLblPo okLbl = newLabel(ctx);
  codeLblPo endLbl = newLabel(ctx);

  ldr(glbHeap, IM((uinteger) globalHeap));
  ldr(dst, OF(glbHeap, currOff));           // pick up current heap top
  add(scratch, dst, amnt);
  ldr(limit, OF(glbHeap, limitOff));        // check against limit
  cmp(limit, RG(scratch));
  bgt(okLbl);

  // Invoke out of line allocator


  b(endLbl);

  setLabel(ctx, okLbl);
  str(scratch, OF(glbHeap, currOff));         // record new heap top
  mov(scratch, IM((uinteger) clss));
  str(scratch, OF(dst, OffsetOf(TermRecord, clss))); // record class of new structure

  releaseReg(jit, glbHeap);
  releaseReg(jit, scratch);
  releaseReg(jit, limit);
  setLabel(ctx, endLbl);
  return Ok;
}
