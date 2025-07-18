//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>

#include "cellP.h"
#include "lowerP.h"
#include "stackP.h"
#include "singleP.h"
#include "globalsP.h"
#include "constantsP.h"
#include "jitP.h"
#include "formioP.h"
#include "closureP.h"
#include "debug.h"
#include "engineP.h"
#include "errorCodes.h"
#include "threds.h"

/* Lower Star VM code to Arm64 code */

/* Register allocation for arm64:
 *
 * X0 = return register
 * X0-X10 = argument registers & scratch registers
 * X12 = Constants vector
 * AG = X13 = args pointer
 * STK = X14 = current stack structure pointer
 * X15 = current process structure
 * X16-X17 = intra procedure call scratch registers
 * X18 = platform register
 * X19-X28 = callee saved registers
 * FP = X29 = frame pointer
 * LR = X30 = link register
 * SP = X31 = system stack pointer
 */

#define CO (X12)
#define AG  (X13)
#define STK (X14)
#define PR (X15)

// We need these registers preserved at all costs
static registerMap criticalRegs() {
  return 1u << CO | 1u << PR;
}

static retCode stackCheck(jitCompPo jit, methodPo mtd);

static const int32 pointerSize = sizeof(integer);

static retCode bailOut(jitCompPo jit, ExitCode code);

static retCode getIntVal(jitCompPo jit, armReg rg);

static retCode mkIntVal(jitCompPo jit, armReg rg);

static retCode getFltVal(jitCompPo jit, armReg rg);

static retCode mkFltVal(jitCompPo jit, armReg rg);

static retCode jitError(jitCompPo jit, char *msg, ...);

static jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt);

static codeLblPo breakLabel(jitBlockPo block);

static codeLblPo loopLabel(jitBlockPo block);

retCode breakOutEq(jitBlockPo block, insPo code, int32 tgt);

retCode breakOutNe(jitBlockPo block, insPo code, int32 tgt);

retCode breakOut(jitBlockPo block, insPo code, int32 tgt, logical keepTop);

void stash(jitCompPo jit);
void stashRegisters(jitCompPo jit, int32 stackLevel);
void unstash(jitCompPo jit);

void loadOffset(jitCompPo jit, armReg tgt, armReg base, int32 ix);
void storeOffset(jitCompPo jit, armReg src, armReg base, int32 lclNo);

static void loadLocal(jitCompPo jit, armReg tgt, int32 lclNo);
static void storeLocal(jitCompPo jit, armReg src, int32 lclNo);
static void loadConstant(jitCompPo jit, int32 key, armReg tgt);

static armReg allocSmallStruct(jitCompPo jit, clssPo class, integer amnt);

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

  stk->sp = exitSP;

  return ret;
}

static void setStackDepth(jitCompPo jit, int32 depth) {
  jit->stackDepth = depth;
  check(jitStackHasRoom(jit,0), "Invalid jit stack depth");
}

static armReg popStkOp(jitCompPo jit, armReg tgt) {
  check(jitStackHasRoom(jit,1), "Insufficient stack depth for pop stack");
  loadLocal(jit, tgt, -jitTrueStackDepth(jit));
  jit->stackDepth--;
  return tgt;
}

static armReg topStkOp(jitCompPo jit) {
  armReg tgt = findFreeReg(jit);
  check(jitStackHasRoom(jit,1), "Invalid jit stack depth");
  loadLocal(jit, tgt, -jitTrueStackDepth(jit));
  return tgt;
}

static void pushStkOp(jitCompPo jit, armReg src) {
  jit->stack[jit->stackDepth].state = inRegister;
  jit->stack[jit->stackDepth].Rg = RG(src);
  jit->stackDepth++;
  storeLocal(jit, src, -jitTrueStackDepth(jit));
}

static void storeStack(jitCompPo jit, armReg src, int32 depth) {
  storeLocal(jit, src, -(lclCount(jit->mtd) + depth));
}

static void pshFrame(jitCompPo jit, assemCtxPo ctx, armReg mtdRg) {
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, OffsetOf(StackFrame, args)));
  sub(AG, AG, IM(jitTrueStackDepth(jit)*pointerSize));
  armReg tmp = findFreeReg(jit);
  ldr(tmp, OF(STK, OffsetOf(StackRecord, prog)));
  str(tmp, OF(FP, OffsetOf(StackFrame, prog))); // We know what program we are executing
  str(mtdRg, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  releaseReg(jit, tmp);
}

static void overrideFrame(jitCompPo jit, assemCtxPo ctx, int arity) {
  armReg src = findFreeReg(jit);
  armReg tgt = findFreeReg(jit);
  armReg tmp = findFreeReg(jit); // Overwrite existing arguments and locals
  sub(src, AG, IM((jitTrueStackDepth(jit)-arity) * pointerSize));
  add(tgt, AG, IM(argCount(jit->mtd) * pointerSize));
  if (arity < 8) {
    for (int ix = 0; ix < arity; ix++) {
      ldr(tmp, PRX(src, -pointerSize));
      str(tmp, PRX(tgt, -pointerSize));
    }
  } else {
    // Build a loop
    armReg cx = findFreeReg(jit);
    mov(cx, IM(arity));
    codeLblPo start = here();

    ldr(tmp, PRX(src, -pointerSize));
    str(tmp, PRX(tgt, -pointerSize));

    subs(cx, cx, IM(1));
    bne(start);
    releaseReg(jit, cx);
  }
  // Update current frame
  str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  mov(AG, RG(tgt));

  releaseReg(jit, tmp);
  releaseReg(jit, src);
  releaseReg(jit, tgt);
}

static retCode tesResult(jitBlockPo block, insPo code, int32 tgt) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo skip = newLabel(ctx);
  cmp(X0, IM(Normal));
  beq(skip);
  retCode ret = breakOut(block, code, tgt, True);

  bind(skip);
  return ret;
}

static retCode jitBlock(jitBlockPo block, insPo code, int32 from, int32 endPc) {
  retCode ret = Ok;
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);

  for (int32 pc = from; ret == Ok && pc < endPc;) {
    switch (code[pc].op) {
      case Halt: {
        // Stop execution
        ExitCode errCode = (ExitCode) code[pc].fst;
        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) star_exit, 1, IM(errCode));
        pc++;
        continue;
      }
      case Abort: {
        // abort with message
        reserveReg(jit, X1);
        reserveReg(jit, X2);
        loadConstant(jit, code[pc].fst, X1);
        popStkOp(jit, X2);
        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) abort_star, 3, RG(PR), RG(X1), RG(X2));
        pc++;
        releaseReg(jit, X1);
        releaseReg(jit, X2);
        continue;
      }
      case Call: {
        // Call <prog>
        int32 key = code[pc].fst;
        labelPo callee = C_LBL(getConstant(key));
        loadConstant(jit, key, X16);
        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo noMtd = newLabel(ctx);
        cbz(X17, noMtd);
        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        codeLblPo runMtd = newLabel(ctx);
        cbnz(X16, runMtd);

        bind(noMtd);
        bailOut(jit, undefinedCode);

        bind(runMtd);
        pshFrame(jit, ctx, X17);
        blr(X16);
        jit->stackDepth -= lblArity(callee) - 1;
        pc++;
        continue;
      }
      case XCall: {
        // XCall <prog>
        int32 key = code[pc].fst;
        labelPo callee = C_LBL(getConstant(key));

        // pick up the pointer to the method
        loadConstant(jit, key, X16);
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo noMtd = newLabel(ctx);
        cbz(X17, noMtd);
        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        codeLblPo runMtd = newLabel(ctx);
        cbnz(X16, runMtd);

        bind(noMtd);
        bailOut(jit, undefinedCode);

        bind(runMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        jit->stackDepth -= lblArity(callee) - 1;
        ret = tesResult(block, code, pc + code[pc].alt + 1);
        pc++;
        continue;
      }

      case OCall: {
        // Call closure
        int32 arity = code[pc].fst;
        popStkOp(jit, X16); // Pick up the closure
        ldr(X17, OF(X16, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(X16, OF(X16, OffsetOf(ClosureRecord, free))); // Pick up the free term
        pushStkOp(jit, X16); // The free term isthe first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        jit->stackDepth -= arity - 1;
        pc++;
        continue;
      }
      case XOCall: {
        int32 arity = code[pc].fst;
        popStkOp(jit, X16); // Pick up the closure
        ldr(X17, OF(X16, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(X16, OF(X16, OffsetOf(ClosureRecord, free))); // Pick up the free term
        pushStkOp(jit, X16); // The free term isthe first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        jit->stackDepth -= arity - 1;

        ret = tesResult(block, code, pc + code[pc].alt + 1);

        pc++;
        continue;
      }
      case TCall: {
        // TCall <prog>
        int32 key = code[pc].fst;
        int arity = lblArity(C_LBL(getConstant(key)));

        loadConstant(jit, key, X16);
        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        overrideFrame(jit, ctx, arity);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        ldr(X30, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);

        pc++;
        continue;
      }
      case TOCall: {
        // TOCall
        int32 arity = code[pc].fst;

        // Tail Call closure
        popStkOp(jit, X16); // Pick up the closure
        ldr(X17, OF(X16, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(X16, OF(X16, OffsetOf(ClosureRecord, free))); // Pick up the free term
        pushStkOp(jit, X16); // The free term isthe first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        overrideFrame(jit, ctx, arity);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        ldr(X30, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);

        pc++;
        continue;
      }
      case Escape: {
        // call C escape
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) escapeFun(esc), 1, RG(PR));
        unstash(jit);
        jit->stackDepth -= arity - 1;
        // X0 is the return code - which we ignore for normal escapes
        pc++;
        continue;
      }
      case XEscape: {
        // call C escape, with possible exception return
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) escapeFun(esc), 1, RG(PR));
        unstash(jit);
        jit->stackDepth -= arity - 1;
        ret = tesResult(block, code, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case Entry: {
        // locals definition
        int32 locals = code[pc].fst;
        jit->stackDepth = 0;
        assert(locals >= 0);

        str(LR, OF(FP, OffsetOf(StackFrame, link)));

        tryRet(stackCheck(jit, jit->mtd));

        if (locals > 0) {
          armReg vd = findFreeReg(jit);
          loadConstant(jit, voidIndex, vd);
          for (int32 ix = 1; ix <= locals; ix++) {
            storeLocal(jit, vd, -ix);
          }
          releaseReg(jit, vd);
        }
        pc++;
        continue;
      }
      case Ret: {
        // return
        armReg vl = popStkOp(jit, findFreeReg(jit));
        // Put return value at top of args on stack
        storeLocal(jit, vl, mtdArity(jit->mtd) - 1);
        releaseReg(jit, vl);

        // Pick up the caller program
        ldr(X16, OF(FP, OffsetOf(StackFrame, prog)));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust args register
        ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
        // Pick up return address
        ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        mov(X0, IM(Normal));
        br(X16);
        pc++;
        continue;
      }
      case XRet: {
        // exception return
        armReg vl = popStkOp(jit, findFreeReg(jit));
        // Put exception value at top of args on stack
        storeLocal(jit, vl, mtdArity(jit->mtd) - 1);
        releaseReg(jit, vl);

        // Pick up the caller program
        ldr(X16, OF(FP, OffsetOf(StackFrame, prog)));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust args register
        ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
        // Pick up return address
        ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        mov(X0, IM(Abnormal));
        br(X16);
        pc++;
        continue;
      }
      case Block: {
        // block of instructions
        int32 blockLen = code[pc].alt;
        int32 blockHeight = code[pc].fst;
        codeLblPo brkLbl = newLabel(ctx);
        pc++;

        JitBlock subBlock = {
          .jit = block->jit,
          .startPc = pc - 1,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
        };

        ret = jitBlock(&subBlock, code, pc, pc + blockLen);

        pc += blockLen;
        bind(brkLbl);
        jit->stackDepth = blockHeight;
        continue;
      }
      case Break: {
        // leave block
        ret = breakOut(block, code, pc + code[pc].alt + 1, False);
        pc++;
        continue;
      }
      case Result: {
        // return value out of block
        ret = breakOut(block, code, pc + code[pc].alt + 1, True);
        pc++;
        continue;
      }
      case Loop: {
        // jump back to start of block
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo tgt = loopLabel(tgtBlock);
        assert(tgt != Null);
        jit->stackDepth = code[tgtBlock->startPc].fst;
        b(tgt);
        pc++;
        continue;
      }
      case Drop: {
        // drop top of stack
        jit->stackDepth--;
        pc++;
        continue;
      }
      case Dup: {
        // duplicate top of stack
        armReg tgt = topStkOp(jit);
        pushStkOp(jit, tgt);
        releaseReg(jit, tgt);
        pc++;
        continue;
      }
      case Rot: {
        // Pull up nth element of stack
        int32 rotationHeight = code[pc].fst;
        armReg swp = findFreeReg(jit);
        armReg tmp = findFreeReg(jit);

        loadLocal(jit, swp, -jit->stackDepth);

        for (int32 ix = 0; ix < rotationHeight; ix++) {
          loadLocal(jit, tmp, ix - jit->stackDepth + 1);
          storeLocal(jit, tmp, ix - jit->stackDepth);
        }
        storeLocal(jit, swp, rotationHeight - jit->stackDepth);
        releaseReg(jit, swp);
        releaseReg(jit, tmp);
        pc++;
        continue;
      }
      case Rst: {
        // reset stack height to a fixed height
        jit->stackDepth = code[pc].fst;
        pc++;
        continue;
      }
      case Fiber: {
        // Create new fiber
        ret = reserveReg(jit, X1);
        if (ret == Ok) {
          armReg lam = popStkOp(jit, X1);
          ret = reserveReg(jit, X0);
          if (ret == Ok) {
            ldr(X0, OF(PR, OffsetOf(EngineRecord, heap)));
            stash(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) newStack, 3, RG(X0), IM(True), RG(X1));
            if (ret == Ok) {
              unstash(jit);
              pushStkOp(jit, X0); // returned from newStack
              releaseReg(jit, X0);
              releaseReg(jit, X1);
              pc++;
              continue;
            }
          }
        }
        return jitError(jit, "could not reserve registers for FIber");
      }
      case Suspend: {
        check(jitStackHasRoom(jit,2), "insufficent stack space for suspend");

        armReg stk = popStkOp(jit, findFreeReg(jit));
        armReg evt = popStkOp(jit, findFreeReg(jit));
        armReg tmp = findFreeReg(jit);
        codeLblPo rtn = newLabel(ctx);
        adr(tmp, rtn);
        str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) detachStack, 3, RG(PR), RG(stk), RG(evt));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        pc++;
        releaseReg(jit, tmp);
        releaseReg(jit, evt);
        releaseReg(jit, stk);
        setStackDepth(jit, 1);
        continue;
      }
      case Resume: {
        check(jitStackHasRoom(jit,2), "insufficent stack space for resume");

        armReg stk = popStkOp(jit, findFreeReg(jit));
        armReg evt = popStkOp(jit, findFreeReg(jit));
        codeLblPo rtn = newLabel(ctx);
        adr(X16, rtn);
        str(X16, OF(STK, OffsetOf(StackRecord, pc)));
        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) attachStack, 3, RG(PR), RG(stk), RG(evt));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        pc++;
        releaseReg(jit, stk);
        releaseReg(jit, evt);
        setStackDepth(jit, 1);
        continue;
      }
      case Retire: {
        // Similar to suspend, except that we trash the suspending stack
        check(jitStackHasRoom(jit,2), "insufficent stack space for retire");
        armReg stk = popStkOp(jit, findFreeReg(jit));
        armReg evt = popStkOp(jit, findFreeReg(jit));
        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) detachDropStack, 3, RG(PR), RG(stk), RG(evt));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        pc++;
        releaseReg(jit, evt);
        releaseReg(jit, stk);
        continue;
      }
      case Underflow: {
        // underflow from current stack
        armReg val = popStkOp(jit, findFreeReg(jit));
        stash(jit);
        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) detachDropStack, 3, RG(PR), RG(STK), RG(val));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        pc++;
        releaseReg(jit, val);
        continue;
      }
      case LdV: {
        // Place a void value on stack
        armReg vd = findFreeReg(jit);
        mov(vd, IM((integer) voidEnum));
        pushStkOp(jit, vd);
        releaseReg(jit, vd);
        pc++;
        continue;
      }
      case LdC: {
        // load literal from constant pool
        int32 key = code[pc].fst;
        armReg cn = findFreeReg(jit);

        loadConstant(jit, key, cn);

        pushStkOp(jit, cn);
        releaseReg(jit, cn);

        pc++;
        continue;
      }
      case LdA: {
        // load stack from args[xx]
        int32 argNo = code[pc].fst;
        armReg rg = findFreeReg(jit);
        loadLocal(jit, rg, argNo);
        pushStkOp(jit, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case LdL: {
        // load stack from local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg rg = findFreeReg(jit);
        loadLocal(jit, rg, -lclNo);
        pushStkOp(jit, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case StL: {
        // store tos to local[xx]
        int32 lclNo = code[pc].fst;
        armReg vl = popStkOp(jit, findFreeReg(jit));
        storeLocal(jit, vl, -lclNo);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case StV: {
        // clear a local to void
        int32 lclNo = code[pc].fst;
        armReg vd = findFreeReg(jit);
        loadConstant(jit, voidIndex, vd);
        storeLocal(jit, vd, -lclNo);
        releaseReg(jit, vd);
        pc++;
        continue;
      }
      case TL: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        armReg vl = topStkOp(jit);
        storeLocal(jit, vl, -lclNo);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case LdG: {
        // load a global variable
        armReg glb = findFreeReg(jit);
        armReg content = findFreeReg(jit);
        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Check if global is set
        ldr(content, OF(glb, OffsetOf(GlobalRecord, content)));
        codeLblPo haveContent = newLabel(ctx);
        cbnz(content, haveContent);

        labelPo glbLbl = declareLbl(globalVarName(glbVr), 0, 0);
        if (glbLbl == Null)
          return jitError(jit, "no label definition for global %s", globalVarName(glbVr));

        int32 lblKey = defineConstantLiteral((termPo) glbLbl);
        loadConstant(jit, lblKey, X16);

        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));

        codeLblPo returnPc = newLabel(ctx);
        adr(LR, returnPc);
        br(X16);

        bind(haveContent);
        pushStkOp(jit, content);
        bind(returnPc);
        releaseReg(jit, glb);
        releaseReg(jit, content);
        pc++;
        continue;
      }
      case StG: {
        // store into a global variable
        armReg tmp = findFreeReg(jit);
        armReg glb = findFreeReg(jit);
        popStkOp(jit, tmp);

        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Assign to the global var's content field
        str(tmp, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, tmp);
        releaseReg(jit, glb);
        pc++;
        continue;
      }
      case TG: {
        // copy into a global variable
        armReg glb = findFreeReg(jit);
        armReg vl = topStkOp(jit);

        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Assign to the global var's content field
        str(vl, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, vl);
        releaseReg(jit, glb);
        pc++;
        continue;
      }
      case Sav: {
        // create a single assignment variable
        armReg sng = allocSmallStruct(jit, singleClass, SingleCellCount);
        armReg tmp = findFreeReg(jit);
        mov(tmp, IM((integer) Null));
        str(tmp, OF(sng, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        pushStkOp(jit, sng);
        releaseReg(jit, sng);
        pc++;
        continue;
      }
      case LdSav: {
        // derefence a sav, break if not set
        armReg sng = popStkOp(jit, findFreeReg(jit));

        ldr(sng, OF(sng, OffsetOf(SingleRecord, content)));
        codeLblPo skip = newLabel(ctx);

        cbnz(sng, skip);
        ret = breakOut(block, code, pc + code[pc].alt + 1, False);
        bind(skip);
        pushStkOp(jit, sng);
        releaseReg(jit, sng);
        pc++;
        continue;
      }
      case TstSav: {
        // test a sav, return a logical
        armReg sng = popStkOp(jit, findFreeReg(jit));
        armReg tr = findFreeReg(jit);
        armReg fl = findFreeReg(jit);

        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);
        ldr(sng, OF(sng, OffsetOf(SingleRecord, content)));
        tst(sng, IM((integer) Null));
        csel(sng, tr, fl, EQ);
        pushStkOp(jit, sng);
        releaseReg(jit, sng);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        pc++;
        continue;
      }
      case StSav: {
        // store a value into a single assignment
        armReg sng = popStkOp(jit, findFreeReg(jit));
        armReg val = popStkOp(jit, findFreeReg(jit));

        codeLblPo ok = newLabel(ctx);
        armReg cont = findFreeReg(jit);
        ldr(cont, OF(sng, OffsetOf(SingleRecord, content)));
        cbnz(cont, ok);

        bailOut(jit, singleCode);
        bind(ok);
        str(val, OF(sng, OffsetOf(SingleRecord, content)));
        releaseReg(jit, cont);
        releaseReg(jit, val);
        releaseReg(jit, sng);
        pc++;
        continue;
      }
      case TSav: {
        armReg sng = popStkOp(jit, findFreeReg(jit));
        armReg val = topStkOp(jit);

        codeLblPo ok = newLabel(ctx);
        armReg cont = findFreeReg(jit);
        ldr(cont, OF(sng, OffsetOf(SingleRecord, content)));
        cbz(cont, ok);

        bailOut(jit, singleCode);
        bind(ok);
        str(val, OF(sng, OffsetOf(SingleRecord, content)));
        releaseReg(jit, cont);
        releaseReg(jit, val);
        releaseReg(jit, sng);
        pc++;
        continue;
      }
      case Cell: {
        // create R/W cell
        armReg cel = allocSmallStruct(jit, cellClass, CellCellCount);
        armReg tmp = findFreeReg(jit);
        popStkOp(jit, tmp);
        str(tmp, OF(cel, OffsetOf(CellRecord, content)));
        releaseReg(jit, tmp);
        pushStkOp(jit, cel);
        releaseReg(jit, cel);
        pc++;
        continue;
      }
      case Get: {
        // access a R/W cell
        armReg cel = popStkOp(jit, findFreeReg(jit));
        ldr(cel, OF(cel, OffsetOf(CellRecord, content)));
        pushStkOp(jit, cel);
        releaseReg(jit, cel);
        pc++;
        continue;
      }
      case Assign: {
        // assign to a R/W cell
        armReg cel = popStkOp(jit, findFreeReg(jit));
        armReg vl = popStkOp(jit, findFreeReg(jit));
        str(vl, OF(cel, OffsetOf(CellRecord, content)));
        releaseReg(jit, cel);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case CLbl: {
        // T,Lbl --> test for a data term, break if not lbl
        int32 key = code[pc].fst;
        armReg vl = popStkOp(jit, findFreeReg(jit));
        armReg tmp = findFreeReg(jit);

        codeLblPo fail = newLabel(ctx);
        codeLblPo ok = newLabel(ctx);

        and(tmp, vl, IM(0b11));
        cmp(tmp, IM(0b00));
        bne(fail);

        ldr(tmp, OF(vl, OffsetOf(TermRecord,clss))); // pick up the class
        loadConstant(jit, key, vl);
        cmp(tmp, RG(vl));
        beq(ok);

        bind(fail);
        ret = breakOut(block, code, pc + code[pc].alt + 1, False);

        bind(ok);
        pc++;
        releaseReg(jit, tmp);
        releaseReg(jit, vl);
        continue;
      }

      case CInt:
      case CChar:
      case CFlt: {
        armReg st = popStkOp(jit, findFreeReg(jit));
        integer lit = (integer) getConstant(code[pc].fst);
        if (is12bit(lit))
          cmp(st, IM(lit));
        else {
          armReg lt = findFreeReg(jit);
          loadConstant(jit, code[pc].fst, lt);
          cmp(st, RG(lt));
          releaseReg(jit, lt);
        }
        releaseReg(jit, st);
        ret = breakOutNe(block, code, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case CLit: {
        // T,lit --> test for a literal value, break if not
        int32 key = code[pc].fst;

        tryRet(reserveReg(jit, X0));
        tryRet(reserveReg(jit, X1));
        loadConstant(jit, key, X0);
        popStkOp(jit, X1);
        stash(jit);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) sameTerm, 2, RG(X0), RG(X1));
        unstash(jit);
        tst(X0, RG(X0));
        ret = breakOutEq(block, code, pc + code[pc].alt + 1);
        pc++;
        releaseReg(jit, X0);
        releaseReg(jit, X1);
        continue;
      }

      case Nth: {
        // T --> el, pick up the nth element
        armReg vl = popStkOp(jit, findFreeReg(jit));
        loadOffset(jit, vl, vl, code[pc].fst + 1);
        pushStkOp(jit, vl);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case StNth: {
        // T el --> store in nth element
        armReg trm = popStkOp(jit, findFreeReg(jit));
        armReg vl = popStkOp(jit, findFreeReg(jit));
        str(vl, OF(trm, (code[pc].fst + 1) * pointerSize));

        releaseReg(jit, vl);
        releaseReg(jit, trm);
        pc++;
        continue;
      }
      case If: {
        // break if true
        armReg vl = popStkOp(jit, findFreeReg(jit));
        armReg tr = findFreeReg(jit);
        loadConstant(jit, trueIndex, tr);
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        ret = breakOutEq(block, code, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case IfNot: {
        // break if false
        armReg vl = popStkOp(jit, findFreeReg(jit));
        armReg tr = findFreeReg(jit);
        loadConstant(jit, trueIndex, tr);
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        ret = breakOutNe(block, code, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case ICase: {
        armReg gr = popStkOp(jit, findFreeReg(jit));
        getIntVal(jit, gr);
        and(gr, gr, IM(LARGE_INT61));
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(code[pc].fst));
        armReg quotient = findFreeReg(jit);
        udiv(quotient, gr, divisor);
        msub(gr, divisor, quotient, gr);
        releaseReg(jit, divisor);
        armReg tgt = findFreeReg(jit);
        codeLblPo jmpTbl = newLabel(ctx);
        adr(tgt, jmpTbl);
        add(tgt, tgt, LS(gr, 2));
        br(tgt);
        releaseReg(jit, tgt);
        releaseReg(jit, quotient);
        releaseReg(jit, gr);
        bind(jmpTbl);
        pc++;
        continue;
      }
      case Case: {
        // T --> T, case <Max>
        if (reserveReg(jit, X0) == Ok) {
          popStkOp(jit, X0);
          stash(jit);
          callIntrinsic(ctx, criticalRegs(), (runtimeFn) hashTerm, 1, RG(X0));
          unstash(jit);
          armReg divisor = findFreeReg(jit);
          mov(divisor, IM(code[pc].fst));
          armReg quotient = findFreeReg(jit);
          udiv(quotient, X0, divisor);
          msub(X0, divisor, quotient, X0);
          releaseReg(jit, divisor);
          armReg tgt = findFreeReg(jit);
          codeLblPo jmpTbl = newLabel(ctx);
          adr(tgt, jmpTbl);
          add(tgt, tgt, LS(X0, 2));
          br(tgt);
          releaseReg(jit, tgt);
          releaseReg(jit, quotient);
          releaseReg(jit, X0);
          bind(jmpTbl);
          pc++;
          continue;
        } else {
          return jitError(jit, "cannot reserve R0");
        }
      }
      case IxCase: {
        // check and jump on index
        armReg tgt = popStkOp(jit, findFreeReg(jit));
        armReg ix = findFreeReg(jit);
        ldr(ix, OF(tgt, 0)); // Pick up the label
        ldr(ix, OF(ix, OffsetOf(LblRecord, index)));
        // Make sure that it is less than max
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(code[pc].fst));
        armReg quotient = findFreeReg(jit);
        udiv(quotient, ix, divisor);
        msub(ix, divisor, quotient, ix);

        codeLblPo jmpTbl = newLabel(ctx);
        adr(tgt, jmpTbl);
        add(tgt, tgt, LS(ix, 2));
        br(tgt);
        releaseReg(jit, tgt);
        releaseReg(jit, quotient);
        releaseReg(jit, ix);
        releaseReg(jit, divisor);
        bind(jmpTbl);
        pc++;
        continue;
      }
      case IAdd: {
        // L R --> L+R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        add(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case ISub: {
        // L R --> L-R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        sub(a1, a1, RG(a2));

        mkIntVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case IMul: {
        // L R --> L*R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        mul(a1, a2, a1);

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case IDiv: {
        // L R --> L/R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        cbnz(a2, skip);

        loadConstant(jit, divZeroIndex, a2);
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          storeStack(jit, a2, code[tgtBlock->startPc].fst);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        sdiv(a1, a1, a2);
        mkIntVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case IMod: {
        // L R --> L%R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg divisor = popStkOp(jit, findFreeReg(jit));
        getIntVal(jit, a1);
        getIntVal(jit, divisor);

        codeLblPo skip = newLabel(ctx);
        cbnz(divisor, skip);

        loadConstant(jit, divZeroIndex, divisor);
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          storeStack(jit, divisor, code[tgtBlock->startPc].fst);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);

        armReg quotient = findFreeReg(jit);
        sdiv(quotient, a1, divisor);
        msub(a1, divisor, quotient, a1);

        mkIntVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, divisor);
        releaseReg(jit, quotient);
        pc++;
        continue;
      }
      case IAbs: {
        // L --> abs(L)
        armReg a1 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);

        cmp(a1, IM(0));
        csneg(a1, a1, a1, GE);

        mkIntVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);

        pc++;
        continue;
      }
      case CEq:
      case IEq: {
        // L R --> L==R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findFreeReg(jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, NE);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        releaseReg(jit, tr);
        releaseReg(jit, fl);

        pc++;
        continue;
      }
      case CLt:
      case ILt: {
        // L R --> L<R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findFreeReg(jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);

        cmp(a1, RG(a2));
        csel(a1, tr, fl, LT);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        releaseReg(jit, fl);
        releaseReg(jit, tr);

        pc++;
        continue;
      }
      case CGe:
      case IGe: {
        // L R --> L>=R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findFreeReg(jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);

        cmp(a1, RG(a2));
        csel(a1, tr, fl, GE);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        releaseReg(jit, fl);
        releaseReg(jit, tr);

        pc++;
        continue;
      }

      case BAnd: {
        // L R --> L&R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        and(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case BOr: {
        // L R --> L|R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        orr(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case BXor: {
        // L R --> L^R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        eor(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case BLsl: {
        // L R --> L<<R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsl(a1, a1, RG(a2));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case BLsr: {
        // L R --> L>>R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsr(a1, a1, RG(a2));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case BAsr: {
        // L R --> L>>>R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        asr(a1, a1, RG(a2));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case BNot: {
        // L --> ~L
        armReg a1 = popStkOp(jit, findFreeReg(jit));

        getIntVal(jit, a1);
        mvn(a1, a1, LSL, 0);
        mkIntVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        pc++;
        continue;
      }
      case FAdd: {
        // L R --> L+R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fadd(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FSub: {
        // L R --> L-R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fsub(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FMul: {
        // L R --> L*R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fmul(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FDiv: {
        // L R --> L/R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        cbnz(a2, skip);
        int32 divZeroKey = defineConstantLiteral(divZero);

        loadConstant(jit, divZeroIndex, a2);
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          storeStack(jit, a2, code[tgtBlock->startPc].fst);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fdiv(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FMod: {
        // L R --> L%R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        cbnz(a2, skip);
        int32 divZeroKey = defineConstantLiteral(divZero);

        loadConstant(jit, divZeroIndex, a2);
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          storeStack(jit, a2, code[tgtBlock->startPc].fst);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fdiv(F2, F0, F1);
        fmsub(F2, F2, F1, F0);
        fmov(RG(a1), FP(F2));
        mkFltVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FAbs: {
        // L --> abs(L)
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        getFltVal(jit, a1);

        fmov(FP(F0), RG(a1));
        fabs(F0, F0);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        pc++;
        continue;
      }
      case FEq: {
        // L R --> L==
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));

        fcmp(F0, F1);
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, NE);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FLt: {
        // L R --> L<R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));

        fcmp(F0, F1);

        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, GE);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FGe: {
        // L R --> L>=R
        armReg a1 = popStkOp(jit, findFreeReg(jit));
        armReg a2 = popStkOp(jit, findFreeReg(jit));

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));

        fcmp(F0, F1);

        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, LT);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case Alloc: {
        // new structure, elements from stack
        int32 key = code[pc].fst;
        labelPo label = C_LBL(getConstant(key));
        int32 arity = lblArity(label);

        armReg term = allocSmallStruct(jit, (clssPo) label, NormalCellCount(arity));

        armReg tmp = findFreeReg(jit);
        for (int32 ix = 0; ix < arity; ix++) {
          popStkOp(jit, tmp);
          str(tmp, OF(term, (ix + 1) * pointerSize));
        }
        releaseReg(jit, tmp);

        pushStkOp(jit, term);
        releaseReg(jit, term);

        pc++;
        continue;
      }
      case Closure: {
        int32 key = code[pc].fst;

        armReg term = allocSmallStruct(jit, closureClass, ClosureCellCount);

        armReg tmp = findFreeReg(jit);
        loadConstant(jit, key, tmp);
        str(tmp, OF(term, OffsetOf(ClosureRecord, lbl)));

        popStkOp(jit, tmp); // pick up the free value
        str(tmp, OF(term, OffsetOf(ClosureRecord, free)));

        releaseReg(jit, tmp);

        pushStkOp(jit, term);
        releaseReg(jit, term);

        pc++;
        continue;
      }
      case Frame: {
        // frame instruction
        pc++;
        continue;
      }
      case dBug: {
        // enter the line debugger
        int32 locKey = code[pc].fst;
        armReg loc = findFreeReg(jit);
        loadConstant(jit, locKey, loc);
        pc++;
        stash(jit);
        switch (code[pc].op) {
          case Abort: {
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) abortDebug, 2, RG(PR), RG(loc));
            break;
          }
          case Entry: {
            armReg lbl = findFreeReg(jit);
            int32 lblKey = defineConstantLiteral((termPo) mtdLabel(jit->mtd));
            loadConstant(jit, lblKey, lbl);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) entryDebug, 3, RG(PR), RG(loc), RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case Call:
          case XCall: {
            armReg lbl = findFreeReg(jit);
            loadConstant(jit, code[pc].fst, lbl);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) callDebug, 4, RG(PR), IM(code[pc].op), RG(loc),
                                RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case TCall: {
            armReg lbl = findFreeReg(jit);
            loadConstant(jit, code[pc].fst, lbl);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) tcallDebug, 3, RG(PR), RG(loc), RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case OCall:
          case XOCall: {
            armReg lbl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) ocallDebug, 4, RG(PR), IM(code[pc].op), RG(loc),
                                RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case Ret: {
            armReg vl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) retDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case XRet: {
            armReg vl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) xretDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Assign: {
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) assignDebug, 2, RG(PR), RG(loc));
            break;
          }
          case Fiber: {
            armReg vl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) fiberDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Suspend: {
            armReg vl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) suspendDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Resume: {
            armReg vl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) resumeDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Retire: {
            armReg vl = topStkOp(jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) retireDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          default:
            return jitError(jit, "invalid instruction following DBug");
        }
        unstash(jit);
        releaseReg(jit, loc);
        continue;
      }
      case Line: {
        int32 locKey = code[pc].fst;
        armReg loc = findFreeReg(jit);
        loadConstant(jit, locKey, loc);
        stash(jit);
        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) lineDebug, 2, RG(PR), RG(loc));
        unstash(jit);
        releaseReg(jit, loc);
        pc++;
        continue;
      }
      default:
        return jitError(jit, "unknown instruction: %s", opNames[code[pc].op]);
    }
  }

  return ret;
}

retCode jitInstructions(jitCompPo jit, methodPo mtd, char *errMsg, integer msgLen) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method %L\n", mtd);
  }
#endif
  JitBlock block = {
    .jit = jit,
    .startPc = 0, .breakLbl = Null, .loopLbl = Null, .parent = Null
  };

  return jitBlock(&block, entryPoint(mtd), 0, codeSize(mtd));
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

retCode breakOutEq(jitBlockPo block, insPo code, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, code, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    beq(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOutNe(jitBlockPo block, insPo code, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, code, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;

  if (lbl != Null) {
    assemCtxPo ctx = assemCtx(jit);
    bne(lbl);
    return Ok;
  }
  return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOut(jitBlockPo block, insPo code, int32 tgt, logical keepTop) {
  jitBlockPo tgtBlock = breakBlock(block, code, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    if (keepTop) {
      int32 tgtOff = code[tgtBlock->startPc].fst;
      if (tgtOff != jit->stackDepth) {
        armReg tp = topStkOp(jit);
        storeStack(jit, tp, tgtOff);
        releaseReg(jit, tp);
      }
    }

    b(lbl);
    return Ok;
  }
  return jitError(jit, "cannot find target label for %d", tgt);
}

jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(code[block->startPc].op == Block);
      return block;
    } else
      block = block->parent;
  }
  return Null;
}

codeLblPo breakLabel(jitBlockPo block) {
  return block->breakLbl;
}

codeLblPo loopLabel(jitBlockPo block) {
  return block->loopLbl;
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

retCode stackCheck(jitCompPo jit, methodPo mtd) {
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = newLabel(ctx);
  int32 delta = (stackDelta(mtd) + (int32)FrameCellCount) * pointerSize;

  if (is16bit(delta))
    sub(X16, AG, IM(delta));
  else {
    mov(X16, IM(delta));
    sub(X16, AG, RG(X16));
  }
  cmp(X16, RG(FP));
  bhi(okLbl);

  mov(X0, IM((integer) mtd));
  str(X0, OF(STK, OffsetOf(StackRecord, prog)));
  adr(X0, okLbl);
  str(X0, OF(STK,OffsetOf(StackRecord,pc)));

  stashRegisters(jit, 0);
  retCode ret =
    callIntrinsic(ctx, criticalRegs(), (runtimeFn) handleStackOverflow, 4, RG(PR), IM(True), IM(delta),
                  IM(mtdArity(mtd)));
  unstash(jit);

  bind(okLbl);
  return ret;
}

// When we call a C intrinsic, we need to preserve important registers, especially in case of a GC
void stash(jitCompPo jit) {
  stashRegisters(jit, jitTrueStackDepth(jit));
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

void loadLocal(jitCompPo jit, armReg tgt, int32 lclNo) {
  check(lclNo>=jit->minOffset && lclNo<jit->maxOffset, "Accessing out of bounds locals");
  loadOffset(jit, tgt,AG, lclNo);
}

void storeLocal(jitCompPo jit, armReg src, int32 lclNo) {
  check(lclNo>=jit->minOffset && lclNo<jit->maxOffset, "Accessing out of bounds locals");
  storeOffset(jit, src,AG, lclNo);
}

static void loadConstant(jitCompPo jit, int32 key, armReg tgt) {
  assemCtxPo ctx = assemCtx(jit);
  termPo lit = getConstant(key);

  if (isSmall(lit))
    mov(tgt, IM((integer) lit));
  else
    loadOffset(jit, tgt,CO, key);
}

void loadOffset(jitCompPo jit, armReg tgt, armReg base, int32 ix) {
  assemCtxPo ctx = assemCtx(jit);
  int32 offset = ix * pointerSize;
  if (is9bit(offset))
    ldur(tgt, base, offset);
  else {
    mov(tgt, IM(ix));
    ldr(tgt, EX2(base,tgt,U_XTX,3));
  }
}

void storeOffset(jitCompPo jit, armReg src, armReg base, int32 lclNo) {
  assemCtxPo ctx = assemCtx(jit);
  int32 offset = lclNo * pointerSize;
  if (is9bit(offset))
    stur(src, base, offset);
  else {
    armReg tmp = findFreeReg(jit);
    mov(tmp, IM(lclNo));
    str(src, EX2(base,tmp,U_XTX,3));
    releaseReg(jit, tmp);
  }
}

armReg allocSmallStruct(jitCompPo jit, clssPo class, integer amnt) {
  assemCtxPo ctx = assemCtx(jit);

  codeLblPo ok = newLabel(ctx);

  reserveReg(jit, X0);
  armReg h = findFreeReg(jit);
  armReg c = findFreeReg(jit);
  armReg l = findFreeReg(jit);
  armReg reslt = findFreeReg(jit);
  codeLblPo again = here();
  ldr(h, OF(PR, OffsetOf(EngineRecord, heap)));
  ldr(c, OF(h, OffsetOf(HeapRecord, curr)));
  ldr(l, OF(h, OffsetOf(HeapRecord, limit)));
  mov(reslt, RG(c));
  add(c, c, IM(amnt * pointerSize));
  str(c, OF(h, OffsetOf(HeapRecord, curr)));
  cmp(c, RG(l));
  blt(ok);
  // Restore h->curr
  str(reslt, OF(h, OffsetOf(HeapRecord, curr)));
  stash(jit); // Slow path
  ldr(X0, OF(PR, OffsetOf(EngineRecord, heap)));
  callIntrinsic(ctx, criticalRegs(), (runtimeFn) allocateObject, 3, RG(X0), IM((integer) class), IM(amnt));
  unstash(jit);
  mov(reslt, RG(X0));
  bind(ok);
  mov(c, IM((integer) class));
  str(c, OF(reslt, OffsetOf(TermRecord, clss)));
  releaseReg(jit, h);
  releaseReg(jit, c);
  releaseReg(jit, l);
  releaseReg(jit, X0);
  return reslt;
}
