//
// Created by Francis McCabe on 8/20/25.
//
#include <config.h>

#include "abort.h"
#include "cellP.h"
#include "lowerP.h"
#include "stackP.h"
#include "jitP.h"
#include "formioP.h"
#include "debug.h"
#include "engineP.h"
#include "shuffle.h"

retCode testResult(jitBlockPo block, jitBlockPo tgtBlock) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo skip = newLabel(ctx);
  cmp(X0, IM(Normal));
  beq(skip);
  retCode ret = breakOut(block, tgtBlock);
  bind(skip);
  return ret;
}

retCode getIntVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  asr(rg, rg, IM(2));
  return Ok;
}

retCode mkIntVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  lsl(rg, rg, IM(2));
  orr(rg, rg, IM(intTg));
  return Ok;
}

retCode getFltVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  bfc(rg, 0, 2);
  return Ok;
}

retCode mkFltVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);

  bfc(rg, 0, 2);
  orr(rg, rg, IM(fltTg));
  return Ok;
}

retCode jitError(jitCompPo jit, char *msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args; /* access the generic arguments */
  va_start(args, msg); /* start the variable argument sequence */

  __voutMsg(O_IO(f), msg, args); /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0); /* Terminate the string */

  closeIo(O_IO(f));

  strMsg(jit->errMsg, NumberOf(jit->errMsg), RED_ESC_ON "%s"RED_ESC_OFF, buff);
  return Error;
}

retCode bailOut(jitCompPo jit, ExitCode code) {
  return callIntrinsic(assemCtx(jit), criticalRegs(), (runtimeFn) star_exit, 1, IM(code));

  /* char buff[MAXLINE]; */
  /* strBufferPo f = fixedStringBuffer(buff, NumberOf(buff)); */

  /* va_list args;      /\* access the generic arguments *\/ */
  /* va_start(args, msg);    /\* start the variable argument sequence *\/ */

  /* __voutMsg(O_IO(f), msg, args);  /\* Display into the string buffer *\/ */

  /* va_end(args); */
  /* outByte(O_IO(f), 0);                /\* Terminate the string *\/ */

  /* closeIo(O_IO(f)); */

  /* assemCtxPo ctx = assemCtx(jit);   */
  /* integer conString = stringConstant(ctx,buff,uniStrlen(buff)); */
  /* ldr(X0,IM(conString)); */

  /* return callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(conString)); */
}

static armReg argRegs[] = {X0, X1, X2, X3, X4, X5, X6, X7};

static void moveArg(assemCtxPo ctx, FlexOp dst, FlexOp src, void *cl) {
 registerMap freeRegs = (registerMap) cl;

  move(ctx, dst, src, freeRegs);
}

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, int32 arity, ...) {
  va_list args;
  va_start(args, arity); /* start the variable argument sequence */

  ArgSpec operands[arity];

  for (int32 ix = 0; ix < arity; ix++) {
    operands[ix] = (ArgSpec){
      .src = (FlexOp) va_arg(args, FlexOp), .dst = RG(argRegs[ix]), .mark = True, .group = -1
    };
  }
  va_end(args);

  saveRegisters(ctx, saveMap);

  shuffleVars(ctx, operands, arity, moveArg, (void*)fixedRegSet(X16));

  mov(X16, IM((integer) fn));
  blr(X16);
  restoreRegisters(ctx, saveMap);
  return Ok;
}

// When we call a C intrinsic, we need to preserve important registers, especially in case of a GC
void stash(jitBlockPo block) {
  stashRegisters(block->jit, trueStackDepth(&block->stack));
}

void stashRegisters(jitCompPo jit, int32 stackLevel) {
  assert(stackLevel>=0);
  assemCtxPo ctx = assemCtx(jit);
  str(AG, OF(STK, OffsetOf(StackRecord, args)));
  armReg currSP = findFreeReg(jit);
  sub(currSP, AG, IM(stackLevel*pointerSize));
  str(currSP, OF(STK, OffsetOf(StackRecord, sp)));
  releaseReg(jit, currSP);
  str(FP, OF(STK, OffsetOf(StackRecord, fp)));
}

void unstash(jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  ldr(STK, OF(PR, OffsetOf(EngineRecord, stk)));
  ldr(AG, OF(STK, OffsetOf(StackRecord, args)));
  ldr(FP, OF(STK, OffsetOf(StackRecord, fp)));
}
