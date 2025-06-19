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
 * X0-X7 = integer parameters
 * X0 = return register
 * X8-X15 = caller saved scratch registers
 * X16-X17 = intra procedure call scratch registers
 * X18 = platform register
 * X24 = current process structure
 * X25 = Constants vector
 * AG = X26 = args pointer
 * STK = X27 = current stack structure pointer
 * SSP = X28 = star stack pointer
 * FP = X29 = frame pointer
 * LR = X30 = link register
 * SP = X31 = system stack pointer
 */

static retCode stackCheck(jitCompPo jit, methodPo mtd);

static int32 pointerSize = sizeof(integer);

static int32 argOffset(int32 argNo) {
  return argNo * pointerSize;
}

static retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun);

static retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun);

static retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun);

static retCode bailOut(jitCompPo jit, int32 code);

static retCode loadStackIntoArgRegisters(jitCompPo jit, armReg startRg, uint32 arity);

static retCode getIntVal(jitCompPo jit, armReg rg);

static retCode mkIntVal(jitCompPo jit, armReg rg);

static retCode getFltVal(jitCompPo jit, armReg rg);

static retCode mkFltVal(jitCompPo jit, armReg rg);

#define SSP (X28)
#define AG  (X26)
#define STK (X27)
#define CO (X25)
#define PR (X24)

static int32 fpArgs = OffsetOf(StackFrame, args);
static int32 fpProg = OffsetOf(StackFrame, prog);
static int32 fpLink = OffsetOf(StackFrame, link);

static retCode jitError(jitCompPo jit, char *msg, ...);

static jitBlockPo breakBlock(jitBlockPo block, int32 tgt);

static codeLblPo breakLabel(jitBlockPo block);

static codeLblPo loopLabel(jitBlockPo block);

retCode breakOutEq(jitBlockPo block, int32 tgt);

retCode breakOutNe(jitBlockPo block, int32 tgt);

retCode breakOut(jitBlockPo block, int32 tgt, logical keepTop);

void stashRegisters(jitCompPo jit);

void unstashRegisters(jitCompPo jit);

static void reserveHeapSpace(jitCompPo jit, integer amnt, codeLblPo ok);

static armReg allocSmallStruct(jitCompPo jit, clssPo class, integer amnt);

ReturnStatus invokeJitMethod(processPo P, methodPo mtd) {
  jittedCode code = jitCode(mtd);
  stackPo stk = processStack(P);
  heapPo h = processHeap(P);
  processPo p = P;

  ReturnStatus ret = Normal;

  asm( "mov x27, %[stk]\n"
       "ldr x28, %[ssp]\n"
       "ldr x26, %[ag]\n"
       "mov x25, %[constants]\n"
       "mov x24, %[process]\n"
       "mov x16, %[code]\n"
       "ldr x29, %[fp]\n"
       "stp x8,x9, [sp, #-16]!\n"
       "stp x10,x11, [sp, #-16]!\n"
       "stp x12,x13, [sp, #-16]!\n"
       "blr x16\n"
       "ldp x12,x13, [sp], #16\n"
       "ldp x10,x11, [sp], #16\n"
       "ldp x8,x9, [sp], #16\n"
       "str w0, %[ret]\n"
       "str X26, %[ag]\n"
       "str x28, %[ssp]\n"
       "str x29, %[fp]\n"
    : [ret] "=&m"(ret), [ag] "+m"(stk->args),[fp] "+m"(stk->fp), [ssp] "+m"(stk->sp)
  : [process]"r"(p), [stk] "r"(stk), [code] "r"(code),
  [constants] "r"(constAnts)
  : "x0", "x1", "x2", "x3", "x24", "x25", "x26", "x27", "x28", "memory");

  return ret;
}

static armReg popStkOp(jitCompPo jit, armReg tgt) {
  assemCtxPo ctx = assemCtx(jit);

  ldr(tgt, PSX(SSP, pointerSize));
  return tgt;
}

static armReg topStkOp(jitCompPo jit) {
  armReg tgt = findFreeReg(jit);
  assemCtxPo ctx = assemCtx(jit);
  ldr(tgt, RG(SSP));

  return tgt;
}

static void pushStkOp(jitCompPo jit, armReg src) {
  assemCtxPo ctx = assemCtx(jit);
  str(src, PRX(SSP, -pointerSize));
}

static armReg loadConstant(jitCompPo jit, int32 key, armReg tgt) {
  assemCtxPo ctx = assemCtx(jit);
  termPo lit = getConstant(key);

  if (isSmall(lit))
    mov(tgt, IM((integer) lit));
  else {
    ldr(tgt, OF(CO, key * pointerSize));
  }

  return tgt;
}

static void pshFrame(jitCompPo jit, assemCtxPo ctx, armReg mtdRg) {
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, fpArgs));
  mov(AG, RG(SSP));
  armReg tmp = findFreeReg(jit);
  ldr(tmp, OF(STK, OffsetOf(StackRecord, prog)));
  str(tmp, OF(FP, fpProg)); // We know what program we are executing
  str(mtdRg, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  releaseReg(jit, tmp);
}

static void overrideFrame(jitCompPo jit, assemCtxPo ctx, int arity) {
  armReg src = findFreeReg(jit);
  armReg tgt = findFreeReg(jit);
  armReg tmp = findFreeReg(jit); // Overwrite existing arguments and locals
  add(src, SSP, IM(arity * pointerSize));
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

    sub(cx, cx, IM(1));
    bne(start);
    releaseReg(jit, cx);
  }
  // Update current frame
  str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  mov(SSP, RG(tgt));
  mov(AG, RG(tgt));

  mov(tmp, IM((integer) jit->mtd));
  str(tmp, OF(FP, fpProg)); // We know what program we are executing
  releaseReg(jit, tmp);
  releaseReg(jit, src);
  releaseReg(jit, tgt);
}

static retCode tesResult(jitBlockPo block, int32 tgt) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo next = newLabel(ctx);
  tstw(X0, IM(Normal));
  beq(next);
  retCode ret = breakOut(block, tgt, True);
  bind(next);
  return ret;
}

static retCode jitBlock(jitBlockPo block, int32 from, int32 endPc) {
  retCode ret = Ok;
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  insPo code = block->code;

  for (int32 pc = from; ret == Ok && pc < endPc;) {
    recordPC(jit,pc,currentPc(ctx));
    switch (code[pc].op) {
      case Halt: {
        // Stop execution
        integer errCode = code[pc].fst;
        ret = callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(errCode));
        pc++;
        continue;
      }
      case Nop: // No operation
        pc++;
        continue;
      case Abort: {
        // abort with message
        reserveReg(jit, X1);
        reserveReg(jit, X2);
        popStkOp(jit, X1);
        popStkOp(jit, X2);
        stashRegisters(jit);
        callIntrinsic(ctx, (runtimeFn) abort_star, 3, RG(PR), RG(X1), RG(X2));
        pc++;
        releaseReg(jit, X1);
        releaseReg(jit, X2);
        continue;
      }
      case Call: {
        // Call <prog>
        int32 key = code[pc].fst;

        loadConstant(jit, key, X16);
        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, 20);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        pc++;
        continue;
      }
      case XCall: {
        // Call <prog>, with
        int32 key = code[pc].fst;

        // pick up the pointer to the method
        loadConstant(jit, key, X16);
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, 21);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        ret = tesResult(block, pc + block->code[pc].alt + 1);
        pc++;
        continue;
      }

      case OCall: {
        // Call closure
        popStkOp(jit, X16); // Pick up the closure
        ldr(X17, OF(X16, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(X16, OF(X16, OffsetOf(ClosureRecord, free))); // Pick up the free term
        pushStkOp(jit, X16); // The free term isthe first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, 22);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        pc++;
        continue;
      }
      case XOCall: {
        armReg tmp = findFreeReg(jit);

        popStkOp(jit, X16); // Pick up the closure
        ldr(X17, OF(X16, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(X16, OF(X16, OffsetOf(ClosureRecord, free))); // Pick up the free term
        pushStkOp(jit, X16); // The free term isthe first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(tmp, haveMtd);

        bailOut(jit, 23);

        bind(haveMtd);
        pshFrame(jit, ctx, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);

        ret = tesResult(block, pc + block->code[pc].alt + 1);

        pc++;
        continue;
      }
      case TCall: {
        // TCall <prog>
        int32 key = code[pc].fst;
        int arity = codeArity(labelCode(C_LBL(getConstant(key))));

        loadConstant(jit, key, X16);
        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, 24);

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

        bailOut(jit, 25);

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

        stashRegisters(jit);
        callIntrinsic(ctx, (runtimeFn) escapeFun(esc), 1, RG(PR));
        unstashRegisters(jit);
        // X0 is the return code - which we ignore for normal escapes
        pc++;
        continue;
      }
      case XEscape: {
        // call C escape, with possible exception return
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        stashRegisters(jit);
        callIntrinsic(ctx, (runtimeFn) escapeFun(esc), arity + 1, RG(X0), RG(X1), RG(X2), RG(X3), RG(X4), RG(X5),
                      RG(X6), RG(X7), RG(X8));
        unstashRegisters(jit);
        ret = tesResult(block, pc + block->code[pc].alt + 1);
        pc++;
        continue;
      }
      case Entry: {
        // locals definition
        int32 locals = code[pc].fst;
        assert(locals >= 0);

        str(LR, OF(FP, fpLink));

        tryRet(stackCheck(jit, jit->mtd));

        if (locals > 0) {
          armReg vd = findFreeReg(jit);
          loadConstant(jit, voidIndex, vd);
          if (locals < 8) {
            for (int32 ix = 0; ix < locals; ix++) {
              pushStkOp(jit, vd);
            }
          } else {
            // Build a loop
            armReg cx = findFreeReg(jit);
            mov(cx, IM(locals));
            codeLblPo start = here();
            pushStkOp(jit, vd);
            sub(cx, cx, IM(1));
            bne(start);
            releaseReg(jit, cx);
          }
          releaseReg(jit, vd);
        }
        pc++;
        continue;
      }
      case Ret: {
        // return
        armReg vl = popStkOp(jit, findFreeReg(jit));

        // Pick up the caller program
        ldr(X16, OF(FP, fpProg));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust Star stack and args register
        add(SSP, AG, IM(codeArity(jit->mtd) * pointerSize));
        ldr(AG, OF(FP, fpArgs));
        // Pick up return address
        ldr(X16, OF(FP, fpLink));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        // Put return value on stack
        pushStkOp(jit, vl);
        releaseReg(jit, vl);
        mov(X0, IM(Normal));
        br(X16);
        pc++;
        continue;
      }
      case XRet: {
        // exception return
        armReg vl = popStkOp(jit, findFreeReg(jit));

        // Pick up the caller program
        ldr(X16, OF(FP, fpProg));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust Star stack and args register
        add(SSP, AG, IM(codeArity(jit->mtd) * pointerSize));
        ldr(AG, OF(FP, fpArgs));
        // Pick up return address
        ldr(X16, OF(FP, fpLink));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        // Put return value on stack
        pushStkOp(jit, vl);
        releaseReg(jit, vl);
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
          .code = code,
          .valStk = block->valStk,
          .startPc = pc - 1,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
        };

        ret = jitBlock(&subBlock, pc, pc + blockLen);

        pc += blockLen;
        bind(brkLbl);
        sub(SSP, AG, IM((lclCount(jit->mtd) + blockHeight) * pointerSize));
        continue;
      }
      case Break: {
        // leave block
        ret = breakOut(block, pc + code[pc].alt + 1, False);
        pc++;
        continue;
      }
      case Result: {
        // return value out of block
        ret = breakOut(block, pc + code[pc].alt + 1, True);
        pc++;
        continue;
      }
      case Loop: {
        // jump back to start of block
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo tgt = loopLabel(tgtBlock);
        assert(tgt != Null);
        sub(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst) * pointerSize));
        b(tgt);
        pc++;
        continue;
      }
      case Drop: {
        // drop top of stack
        add(SSP, SSP, IM(pointerSize));
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
        armReg src = findFreeReg(jit);
        armReg tmp = findFreeReg(jit);

        mov(src, RG(SSP));
        ldr(swp, PSX(src, -pointerSize));

        for (int32 ix = 1; ix <= rotationHeight; ix++) {
          ldr(tmp, PSX(src, -pointerSize));
          str(tmp, OF(src, pointerSize));
        }
        str(swp, PRX(src, pointerSize));
        releaseReg(jit, swp);
        releaseReg(jit, src);
        releaseReg(jit, tmp);
        pc++;
        continue;
      }
      case Rst: {
        // reset stack height to a fixed height
        int32 stkHeight = code[pc].fst;
        int32 lx = lclCount(jit->mtd);
        int32 heightOffset = (lx + stkHeight) * pointerSize;

        sub(SSP, AG, IM(heightOffset));
        pc++;
        continue;
      }
      case Pick: {
        // adjust stack to n depth, using top k elements
        int32 height = code[pc].fst;
        int32 keep = code[pc].alt;
        armReg tgt = findFreeReg(jit);
        armReg src = findFreeReg(jit);
        armReg tmp = findFreeReg(jit);
        sub(tgt, AG, IM(-(height - keep) * pointerSize));
        add(src, STK, IM(keep * pointerSize));

        for (int32 ix = 0; ix < keep; ix++) {
          ldr(tmp, PRX(src, -pointerSize));
          str(tmp, PRX(tgt, pointerSize));
        }

        mov(SSP, RG(tgt));
        releaseReg(jit, tmp);
        releaseReg(jit, src);
        releaseReg(jit, tgt);
        pc++;
        continue;
      }
      case Fiber: { // Create new fiber
      }
      case Suspend: {// suspend fiber
      }
      case Resume: { // resume fiber
      }
      case Retire: { // retire a fiber
        return Error;
      }
      case Underflow: {// underflow from current stack
        armReg val = popStkOp(jit, findFreeReg(jit));
        stp(val, XZR, PRX(SP, -2 * pointerSize));
        stashRegisters(jit);
        ret = callIntrinsic(ctx, (runtimeFn) dropStack, 1, RG(STK));
        unstashRegisters(jit);
        ldp(val, XZR, PSX(SP, 2 * pointerSize));
        pushStkOp(jit,val);
        releaseReg(jit,val);
        pc++;
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
        armReg cn = loadConstant(jit, key, findFreeReg(jit));

        pushStkOp(jit, cn);
        releaseReg(jit, cn);

        pc++;
        continue;
      }
      case LdA: {
        // load stack from args[xx]
        int32 argNo = code[pc].fst;
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(AG, argOffset(argNo)));
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
        ldur(rg, AG, offset);
        pushStkOp(jit, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case StL: {
        // store tos to local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vl = popStkOp(jit, findFreeReg(jit));
        stur(vl, AG, offset);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case StV: {
        // clear a local to void
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vd = findFreeReg(jit);
        loadConstant(jit, voidIndex, vd);
        stur(vd, AG, offset);
        releaseReg(jit, vd);
        pc++;
        continue;
      }
      case TL: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vl = topStkOp(jit);
        stur(vl, AG, offset);
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

        labelPo glbLbl = findLbl(globalVarName(glbVr), 0);
        if (glbLbl == Null)
          return jitError(jit, "no label definition for global %s", globalVarName(glbVr));

        int32 lblKey = defineConstantLiteral((termPo) glbLbl);
        loadConstant(jit, lblKey, X16);

        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, 26);

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
        ret = breakOut(block, pc + code[pc].alt + 1, False);
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

        ldr(sng, OF(sng, OffsetOf(SingleRecord, content)));
        tst(sng, IM((integer) Null));
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);
        csel(sng, tr, fl, EQ);
        pushStkOp(jit, sng);
        releaseReg(jit, sng);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        pc++;
        continue;
      }
      case StSav: { // store a value into a single assignment
        armReg sng = popStkOp(jit, findFreeReg(jit));
        armReg val = popStkOp(jit, findFreeReg(jit));

        codeLblPo ok = newLabel(ctx);
        armReg cont = findFreeReg(jit);
        ldr(cont, OF(sng, OffsetOf(SingleRecord, content)));
        cbnz(cont, ok);

        bailOut(jit, 27);
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

        bailOut(jit, 28);
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
        pc++;
        continue;
      }
      case Assign: {
        // assign to a R/W cell
        armReg cel = popStkOp(jit, findFreeReg(jit));
        armReg vl = popStkOp(jit, findFreeReg(jit));
        str(vl, OF(cel, OffsetOf(CellRecord, content)));
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

        ldr(tmp, OF(vl, 0)); // pick up the class
        loadConstant(jit, key, vl);
        cmp(tmp, RG(vl));
        beq(ok);

        bind(fail);
        ret = breakOut(block, pc + code[pc].alt + 1, False);

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
        ret = breakOutNe(block, pc + code[pc].alt + 1);
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
        invokeCFunc2(jit, (Cfunc2) sameTerm);
        tst(X0, RG(X0));
        ret = breakOutEq(block, pc + code[pc].alt + 1);
        pc++;
        releaseReg(jit, X0);
        releaseReg(jit, X1);
        continue;
      }

      case Nth: {
        // T --> el, pick up the nth element
        armReg vl = popStkOp(jit, findFreeReg(jit));
        ldr(vl, OF(vl, (code[pc].fst + 1) * pointerSize));
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
        ret = breakOutNe(block, pc + code[pc].alt + 1);
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
        ret = breakOutEq(block, pc + code[pc].alt + 1);
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
          callIntrinsic(ctx, (runtimeFn) hashTerm, 1, RG(X0));
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
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          sub(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
          pushStkOp(jit, a2);
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
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          sub(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
          pushStkOp(jit, divisor);
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

        cmp(a1, RG(a2));
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, NE);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

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

        cmp(a1, RG(a2));
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, LT);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

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

        cmp(a1, RG(a2));
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, GE);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

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
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          sub(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
          pushStkOp(jit, a2);
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
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          sub(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
          pushStkOp(jit, a2);
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
      case dBug: // debugging prefix
      case maxOpCode:
      case illegalOp:
        return Error;
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
  ValueStack valueStack = {.stackHeight = 0};
  JitBlock block = {
    .jit = jit,
    .code = entryPoint(mtd),
    .startPc = 0, .valStk = &valueStack, .breakLbl = Null, .loopLbl = Null, .parent = Null
  };

  return jitBlock(&block, 0, codeSize(mtd));
}

retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) fun, 1, RG(X0));
}

retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) fun, 2, RG(X0), RG(X1));
}

retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) fun, 3, RG(X0), RG(X1), RG(X2));
}

retCode loadStackIntoArgRegisters(jitCompPo jit, armReg startRg, uint32 arity) {
  assemCtxPo ctx = assemCtx(jit);
  assert(arity < 9);

  for (uint32 ix = 0; ix < arity; ix++) {
    ldr((armReg) (startRg + ix), PSX(SSP, pointerSize));
  }
  return Ok;
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

retCode breakOutEq(jitBlockPo block, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    beq(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOutNe(jitBlockPo block, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    bne(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOut(jitBlockPo block, int32 tgt, logical keepTop) {
  jitBlockPo tgtBlock = breakBlock(block, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);
  insPo code = block->code;

  if (lbl != Null) {
    if (keepTop) {
      armReg tp = findFreeReg(jit);
      popStkOp(jit, tp);
      sub(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
      pushStkOp(jit, tp);
      releaseReg(jit, tp);
    }
    b(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

jitBlockPo breakBlock(jitBlockPo block, int32 tgt) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(block->code[block->startPc].op == Block);
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

  strMsg(jit->errMsg, jit->msgLen, RED_ESC_ON "%s"RED_ESC_OFF, buff);
  return Error;
}

retCode bailOut(jitCompPo jit, int32 code) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) star_exit, 1, IM(code));

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
  int32 delta = (int32) (stackDelta(mtd) + FrameCellCount) * pointerSize;

  if (is16bit(delta))
    sub(X16, SSP, IM(delta));
  else {
    mov(X16, IM(delta));
    sub(X16, SSP, RG(X16));
  }
  cmp(X16, RG(FP));
  bhi(okLbl);

  str(AG, OF(STK, OffsetOf(StackRecord, args)));
  str(SSP, OF(STK, OffsetOf(StackRecord, sp)));
  str(FP, OF(STK, OffsetOf(StackRecord, fp)));
  mov(X0, IM((integer) mtd));
  str(X0, OF(STK, OffsetOf(StackRecord, prog)));

  stp(CO, X0, PRX(SP, 2 * pointerSize));

  tryRet(callIntrinsic(ctx, (runtimeFn) handleStackOverflow, 3, RG(STK), IM(delta), IM((integer) codeArity(mtd))));

  mov(STK, RG(X0));
  ldp(CO, X0, PSX(SP, 2 * pointerSize));

  ldr(FP, OF(STK, OffsetOf(StackRecord, fp)));
  ldr(SSP, OF(STK, OffsetOf(StackRecord, sp)));
  ldr(AG, OF(STK, OffsetOf(StackRecord, args)));

  bind(okLbl);
  return Ok;
}

// When we call a C intrinsic, we need to preserve important registers, especially in case of a GC
void stashRegisters(jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  stp(PR, CO, PRX(SP, -2 * pointerSize)); // stash process & constants
  str(AG, OF(STK, OffsetOf(StackRecord, args)));
  str(SSP, OF(STK, OffsetOf(StackRecord, sp)));
  str(FP, OF(STK, OffsetOf(StackRecord, fp)));
}

void unstashRegisters(jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  ldp(PR, CO, PSX(SP, 2 * pointerSize)); // pick up process and constants
  ldr(STK, OF(PR, OffsetOf(ProcessRec, stk)));
  ldr(AG, OF(STK, OffsetOf(StackRecord, args)));
  ldr(SSP, OF(STK, OffsetOf(StackRecord, sp)));
  ldr(FP, OF(STK, OffsetOf(StackRecord, fp)));
}

void reserveHeapSpace(jitCompPo jit, integer amnt, codeLblPo ok) {
  assemCtxPo ctx = assemCtx(jit);

  armReg h = findFreeReg(jit);
  armReg c = findFreeReg(jit);
  armReg l = findFreeReg(jit);
  ldr(h, OF(PR, OffsetOf(ProcessRec, heap)));
  ldr(c, OF(h, OffsetOf(HeapRecord, curr)));
  ldr(l, OF(h, OffsetOf(HeapRecord, limit)));
  add(c, c, IM(amnt * pointerSize));
  cmp(c, RG(l));
  blt(ok);
  releaseReg(jit, h);
  releaseReg(jit, c);
  releaseReg(jit, l);
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
  ldr(h, OF(PR, OffsetOf(ProcessRec, heap)));
  ldr(c, OF(h, OffsetOf(HeapRecord, curr)));
  ldr(l, OF(h, OffsetOf(HeapRecord, limit)));
  mov(reslt, RG(c));
  add(c, c, IM(amnt * pointerSize));
  str(c, OF(h, OffsetOf(HeapRecord, curr)));
  cmp(c, RG(l));
  blt(ok);
  // Restore h->curr
  str(reslt, OF(h, OffsetOf(HeapRecord, curr)));
  stashRegisters(jit); // Slow path
  callIntrinsic(ctx, (runtimeFn) allocateObject, 2, IM((integer) class), IM(amnt));
  unstashRegisters(jit);
  mov(reslt, RG(X0));
  cmp(X0, IM((integer) Null));
  bne(ok);
  callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(99)); // no return from this
  bind(ok);
  mov(c, IM((integer) class));
  str(c, OF(reslt, OffsetOf(TermRecord, clss)));
  releaseReg(jit, h);
  releaseReg(jit, c);
  releaseReg(jit, l);
  releaseReg(jit, X0);
  return reslt;
}
