//
// Created by Francis McCabe on 8/20/25.
//
#include <config.h>

#include "abort.h"
#include "arithP.h"
#include "constantsP.h"
#include "lowerP.h"
#include "stackP.h"
#include "jitP.h"
#include "formioP.h"
#include "engineP.h"
#include "shuffle.h"

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

void getFltVal(jitCompPo jit, armReg rg, fpReg tgt) {
  assemCtxPo ctx = assemCtx(jit);

  fldr(tgt, OF(rg, OffsetOf(FloatRecord, dx)));
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

static void moveArg(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap *freeRegs) {
  move(ctx, dst, src, *freeRegs);
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

  registerMap tmpMap = fixedRegSet(X16);

  shuffleVars(ctx, operands, arity, &tmpMap, moveArg);

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

ReturnStatus invokeJitMethod(enginePo P, methodPo mtd) {
  jittedCode code = jitCode(mtd);
  stackPo stk = P->stk;
  int32 arity = lblArity(mtdLabel(mtd));
  ptrPo exitSP = stk->sp + arity - 1;

  ReturnStatus ret = Normal;

  asm("stp x29,x30, [sp, #-16]!\n"
    "stp x8,x9, [sp, #-16]!\n"
    "stp x10,x11, [sp, #-16]!\n"
    "stp x12,x13, [sp, #-16]!\n"
    "mov x14, %[stk]\n"
    "ldr x13, %[ag]\n"
    "mov x12, %[constants]\n"
    "mov x15, %[process]\n"
    "mov x16, %[code]\n"
    "ldr x29, %[fp]\n"
    "blr x16\n"
    "str X13, [x14,#40]\n" // we will need to change these if stack structure changes
    "str x29, [x14,#64]\n"
    "ldp x12,x13, [sp], #16\n"
    "ldp x10,x11, [sp], #16\n"
    "ldp x8,x9, [sp], #16\n"
    "ldp x29,x30, [sp], #16\n"
    "str w0, %[ret]\n"
    : [ret] "=&m"(ret)
    : [process]"r"(P), [stk] "r"(stk), [code] "r"(code), [ag] "m"(stk->args),
    [constants] "r"(constAnts),[fp] "m"(stk->fp)
    : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x0", "x11", "x12", "x13", "x14", "x15", "x16",
    "memory");

  P->stk->sp = exitSP;

  return ret;
}
