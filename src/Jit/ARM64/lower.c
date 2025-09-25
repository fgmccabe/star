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
#include "closureP.h"
#include "debug.h"
#include "engineP.h"
#include "errorCodes.h"
#include "abort.h"
#include "debugP.h"

/* Lower Star VM code to Arm64 code */

static retCode jitBlock(jitBlockPo block, insPo code, int32 from, int32 endPc);
static void pshFrame(jitBlockPo block, armReg mtdRg);
static void dropArgs(valueStackPo stack, jitCompPo jit, int32 count);
static armReg allocSmallStruct(jitBlockPo block, clssPo class, integer amnt);
static void handleBreakTable(jitBlockPo block, insPo code, int32 pc, int32 count);

retCode jitInstructions(jitCompPo jit, methodPo mtd, char *errMsg, integer msgLen) {
  ValueStack stack = {
    .argPnt = NumberOf(stack.local) - mtdArity(mtd),
    .stackPnt = NumberOf(stack.local) - mtdArity(mtd) - lclCount(mtd), .vTop = 0,
    .propagated = False
  };

#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method %L\n", mtd);
  }
#endif

  for (int32 ax = 0; ax < mtdArity(mtd); ax++) {
    stack.local[stack.argPnt + ax] = (LocalEntry){.kind = isLocal, .stkOff = ax, .inited = True};
  }
  for (int32 lx = 1; lx <= lclCount(mtd); lx++) {
    stack.local[stack.argPnt - lx] = (LocalEntry){.kind = isLocal, .stkOff = -lx, .inited = False};
  }
  for (int32 i = 0; i < stack.stackPnt; i++) {
    stack.local[i] = (LocalEntry){.kind = isConstant, .key = voidIndex, .stkOff = i - stack.stackPnt, .inited = False};
  }

  JitBlock block = {
    .jit = jit,
    .startPc = 0, .endPc = codeSize(mtd),
    .lclCnt = lclCount(mtd),
    .breakLbl = Null, .loopLbl = Null,
    .parent = Null,
    .exitHeight = 0,
    .stack = stack
  };

  return jitBlock(&block, entryPoint(mtd), 0, codeSize(mtd));
}

retCode jitSpecialInstructions(jitCompPo jit, methodPo mtd, int32 depth) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit special method %L\n", mtd);
  }
#endif

  ValueStack stack = {
    .argPnt = NumberOf(stack.local) - mtdArity(mtd),
    .stackPnt = NumberOf(stack.local) - mtdArity(mtd) - lclCount(mtd), .vTop = depth, .propagated = False
  };

  for (int32 ax = 0; ax < mtdArity(mtd); ax++) {
    stack.local[stack.argPnt + ax] = (LocalEntry){.kind = isLocal, .stkOff = ax};
  }
  for (int32 lx = 1; lx <= lclCount(mtd); lx++) {
    stack.local[stack.argPnt - lx] = (LocalEntry){.kind = isLocal, .stkOff = -lx};
  }
  for (int32 sx = 1; sx <= depth; sx++) {
    stack.local[stack.stackPnt - sx] = (LocalEntry){.kind = inStack, .stkOff = -(lclCount(mtd) + sx)};
  }

  JitBlock block = {
    .jit = jit,
    .startPc = 0, .endPc = codeSize(mtd), .breakLbl = Null, .loopLbl = Null, .parent = Null,
    .lclCnt = lclCount(mtd),
    .stack = stack
  };

  return jitBlock(&block, entryPoint(mtd), 0, codeSize(mtd));
}

retCode jitBlock(jitBlockPo block, insPo code, int32 from, int32 endPc) {
  retCode ret = Ok;
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  valueStackPo stack = &block->stack;

#ifdef TRACEJIT
  if (traceJit >= generalTracing) {
    outMsg(logFile, "Jit block %d -> %d\n", from, endPc);
  }
#endif
  for (int32 pc = from; ret == Ok && pc < endPc; pc++) {
#ifdef TRACEJIT
    if (traceJit >= generalTracing) {
      disass(logFile, Null, jit->mtd, &code[pc]);
      outMsg(logFile, "\n%_");
    }
    if (traceJit >= detailedTracing) {
      dRegisterMap(jit->freeRegs);
      outMsg(logFile, "vTop = %d\n%_", stack->vTop);
    }
#endif

    switch (code[pc].op) {
      case Halt: {
        // Stop execution
        ExitCode errCode = (ExitCode) code[pc].fst;
        return callIntrinsic(ctx, criticalRegs(), (runtimeFn) star_exit, 1, IM(errCode));
      }
      case Abort: {
        // abort with message
        armReg codeReg = findFreeReg(jit);
        loadConstant(jit, code[pc].fst, codeReg);
        armReg val = popValue(stack, jit);
        stash(block);
        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) abort_star, 3, RG(PR), RG(codeReg), RG(val));
        releaseReg(jit, codeReg);
        releaseReg(jit, val);
        return ret;
      }
      case Call: {
        int32 key = code[pc].fst;
        int32 arity = lblArity(C_LBL(getConstant(key)));
        spillStack(stack, jit);
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
        pshFrame(block, X17);
        blr(X16);
        dropArgs(stack, jit, arity);
        continue;
      }
      case XCall: {
        int32 key = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);

        labelPo callee = C_LBL(getConstant(key));
        int32 arity = lblArity(callee);

        spillStack(stack, jit); // Make sure everything is stashed

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
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArgs(stack, jit, arity);
        ret = testResult(block, tgtBlock);
        continue;
      }

      case OCall: {
        int32 arity = code[pc].fst;
        armReg clos = popValue(stack, jit); // Pick up the closure
        ldr(X17, OF(clos, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        armReg freeReg = findFreeReg(jit);
        ldr(freeReg, OF(clos, OffsetOf(ClosureRecord, free))); // Pick up the free term
        releaseReg(jit, clos);
        pushRegister(stack, freeReg); // The free term is the first argument
        spillStack(stack, jit); // Make sure everything is stashed

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArgs(stack, jit, arity);
        continue;
      }
      case XOCall: {
        int32 arity = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);

        armReg clos = popValue(stack, jit); // Pick up the closure
        ldr(X17, OF(clos, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        armReg freeReg = findFreeReg(jit);
        ldr(freeReg, OF(clos, OffsetOf(ClosureRecord, free))); // Pick up the free term
        releaseReg(jit, clos);
        pushRegister(stack, freeReg); // The free term is the first argument
        spillStack(stack, jit); // Make sure everything is stashed

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArgs(stack, jit, arity);
        ret = testResult(block, tgtBlock);
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
        // Update current frame
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        frameOverride(block, arity);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);

        return ret;
      }
      case TOCall: {
        // TOCall
        int32 arity = code[pc].fst;

        // Tail Call closure
        armReg clos = popValue(stack, jit); // Pick up the closure
        ldr(X17, OF(clos, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));

        armReg freeReg = findFreeReg(jit);
        ldr(freeReg, OF(clos, OffsetOf(ClosureRecord, free))); // Pick up the free term
        releaseReg(jit, clos);
        pushRegister(stack, freeReg); // The free term is the first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        frameOverride(block, arity);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);

        return ret;
      }
      case Escape: {
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        spillStack(stack, jit);

        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) escapeFun(esc), 1, RG(PR));
        unstash(jit);
        dropArgs(stack, jit, arity);
        // X0 is the return code - which we ignore for normal escapes
        continue;
      }
      case XEscape: {
        int32 escNo = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);

        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);
        spillStack(stack, jit);

        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) escapeFun(esc), 1, RG(PR));
        unstash(jit);
        dropArgs(stack, jit, arity);
        ret = testResult(block, tgtBlock);
        continue;
      }
      case Entry: {
        // locals definition
        int32 lclCnt = code[pc].fst;
        assert(lclCnt >= 0);
        stack->vTop = 0;

        str(LR, OF(FP, OffsetOf(StackFrame, link)));

        tryRet(stackCheck(jit, jit->mtd));

        if (lclCnt > 0) {
          for (int32 ix = 1; ix <= lclCnt; ix++) {
            localVarPo lcl = localSlot(stack, ix);
            *lcl = (LocalEntry){.kind = isConstant, .key = voidIndex, .stkOff = -ix};
          }
        }
        continue;
      }
      case Ret: {
        // return
        armReg vl = popValue(stack, jit);
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

        return ret;
      }
      case XRet: {
        // exception return
        armReg vl = popValue(stack, jit);
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

        return ret;
      }
      case Block: {
        // block of instructions
        int32 blockLen = code[pc].alt;
        codeLblPo brkLbl = newLabel(ctx);

        int32 exitHeight = code[pc].fst;
        JitBlock subBlock = {
          .jit = jit,
          .lclCnt = block->lclCnt,
          .startPc = pc,
          .endPc = pc + blockLen + 1,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .exitHeight = exitHeight,
          .parent = block,
          .stack = block->stack,
        };

        ret = jitBlock(&subBlock, code, pc + 1, pc + blockLen + 1);
        pc += blockLen; // Skip over the block
        bind(brkLbl);
#ifdef TRACEJIT
        if (traceJit)
          dRegisterMap(jit->freeRegs);
#endif

        continue;
      }
      case Break: {
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);
        setStackDepth(stack, jit, tgtBlock->exitHeight);
        spillStack(stack, jit);
        propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
        return breakOut(block, tgtBlock);
      }
      case Result: {
        // return value out of block
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);

        int32 tgtHeight = tgtBlock->exitHeight;

        // already at the right height?
        if (tgtHeight != block->stack.vTop) {
          propagateVar(jit, stackSlot(stack, 0), stackSlot(stack, stack->vTop - tgtHeight));
        }
        setStackDepth(stack, jit, tgtHeight);
        spillStack(stack, jit);
        propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);

        return breakOut(block, tgtBlock);
      }
      case Loop: {
        // jump back to start of block
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);
        codeLblPo loop = loopLabel(tgtBlock);
        assert(loop != Null);
        setStackDepth(stack, jit, code[tgtBlock->startPc].fst);
        spillStack(stack, jit);
        propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
        b(loop);
        return ret;
      }
      case Drop: {
        // drop top of stack
        dropValue(stack, jit);
        continue;
      }
      case Dup: {
        // duplicate top of stack
        armReg tgt = topValue(stack, jit);
        pushRegister(stack, tgt);
        continue;
      }
      case Rot: {
        // Pull up nth element of stack
        int32 cnt = code[pc].fst;
        if (cnt > 0) {
          LocalEntry tmp = *stackSlot(stack,0);
          for (int32 ix = 0; ix < cnt; ix++) {
            *stackSlot(stack, ix) = *stackSlot(stack, ix + 1);
          }
          *stackSlot(stack, cnt) = tmp;
        }
        continue;
      }
      case Rst: {
        // reset stack height to a fixed height
        setStackDepth(stack, jit, code[pc].fst);
        continue;
      }
      case Fiber: {
        // Create new fiber
        if (reserveReg(jit, X0) != Ok)
          bailOut(jit, fiberCode);
        armReg lamReg = popValue(stack, jit);
        armReg heapReg = findFreeReg(jit);
        ldr(heapReg, OF(PR, OffsetOf(EngineRecord, heap)));
        spillStack(stack, jit);
        stash(block);

        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) newStack, 3, RG(heapReg), IM(True), RG(lamReg));
        if (ret == Ok) {
          unstash(jit);
          pushRegister(stack, X0);
          releaseReg(jit, lamReg);
          releaseReg(jit, heapReg);
        } else {
          bailOut(jit, fiberCode);
        }
        continue;
      }
      case Suspend: {
        armReg stk = popValue(stack, jit);
        armReg evt = popValue(stack, jit);
        armReg tmp = findFreeReg(jit);
        codeLblPo rtn = newLabel(ctx);
        adr(tmp, rtn);
        str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
        spillStack(stack, jit);
        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) detachStack, 3, RG(PR), RG(stk), RG(evt));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        releaseReg(jit, tmp);
        releaseReg(jit, evt);
        releaseReg(jit, stk);
        pushBlank(stack);
        continue;
      }
      case Resume: {
        armReg stk = popValue(stack, jit);
        armReg evt = popValue(stack, jit);
        codeLblPo rtn = newLabel(ctx);
        adr(X16, rtn);
        str(X16, OF(STK, OffsetOf(StackRecord, pc)));
        spillStack(stack, jit);
        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) attachStack, 3, RG(PR), RG(stk), RG(evt));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        releaseReg(jit, stk);
        releaseReg(jit, evt);
        pushBlank(stack);
        continue;
      }
      case Retire: {
        // Similar to suspend, except that we trash the suspending stack
        armReg stk = popValue(stack, jit);
        armReg evt = popValue(stack, jit);
        spillStack(stack, jit);
        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) detachDropStack, 3, RG(PR), RG(stk), RG(evt));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        releaseReg(jit, evt);
        releaseReg(jit, stk);

        return ret;
      }
      case Underflow: {
        // underflow from current stack
        armReg val = popValue(stack, jit);
        stash(block);
        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) detachDropStack, 3, RG(PR), RG(STK), RG(val));
        unstash(jit);
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        releaseReg(jit, val);
        continue;
      }
      case LdV: {
        // Place a void value on stack
        pushValue(stack, (LocalEntry){.kind = isConstant, .key = voidIndex});
        continue;
      }
      case LdC: {
        // load literal from constant pool
        int32 key = code[pc].fst;
        pushValue(stack, (LocalEntry){.kind = isConstant, .key = key});
        continue;
      }
      case LdA: {
        // load stack from args[xx]
        int32 argNo = code[pc].fst;
        pushValue(stack, (LocalEntry){.kind = isLocal, .stkOff = argNo});
        continue;
      }
      case LdL: {
        // load stack from local[xx]
        int32 lclNo = code[pc].fst;
        pushValue(stack, (LocalEntry){.kind = isLocal, .stkOff = -lclNo});
        continue;
      }
      case StL: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        armReg vl = popValue(stack, jit);
        storeLocal(jit, vl, -lclNo);
        setLocal(stack, lclNo, (LocalEntry){.kind = isLocal, .stkOff = -lclNo});
        releaseReg(jit, vl);
        continue;
      }
      case StV: {
        // clear a local to void
        int32 lclNo = code[pc].fst;
        armReg vd = findFreeReg(jit);
        loadConstant(jit, voidIndex, vd);
        storeLocal(jit, vd, -lclNo);
        setLocal(stack, lclNo, (LocalEntry){.kind = isConstant, .key = voidIndex});
        releaseReg(jit, vd);
        continue;
      }
      case TL: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        armReg vl = popValue(stack, jit);
        storeLocal(jit, vl, -lclNo);
        setLocal(stack, lclNo, (LocalEntry){.kind = isLocal, .stkOff = -lclNo});
        pushRegister(stack, vl);
        continue;
      }
      case LdG: {
        // load a global variable
        armReg glb = findFreeReg(jit);
        armReg content = findFreeReg(jit);
        globalPo glbVr = findGlobalVar(code[pc].fst);
        spillStack(stack, jit); // We spill because we may have to call the global function

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
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));

        codeLblPo returnPc = newLabel(ctx);
        adr(LR, returnPc);
        br(X16);

        bind(haveContent);
        pushRegister(stack, content);
        bind(returnPc);
        releaseReg(jit, glb);
        continue;
      }
      case StG: {
        // store into a global variable
        armReg tmp = popValue(stack, jit);
        armReg glb = findFreeReg(jit);

        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Assign to the global var's content field
        str(tmp, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, tmp);
        releaseReg(jit, glb);
        continue;
      }
      case TG: {
        // copy into a global variable
        armReg glb = findFreeReg(jit);
        armReg vl = topValue(stack, jit);

        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Assign to the global var's content field
        str(vl, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, vl);
        releaseReg(jit, glb);
        continue;
      }
      case Sav: {
        // create a single assignment variable
        spillStack(stack, jit);
        armReg sng = allocSmallStruct(block, singleClass, SingleCellCount);
        armReg tmp = findFreeReg(jit);
        mov(tmp, IM((integer) Null));
        str(tmp, OF(sng, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        pushRegister(stack, sng);
        continue;
      }
      case LdSav: {
        // dereference a sav, break if not set
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);
        armReg sng = popValue(stack, jit);

        ldr(sng, OF(sng, OffsetOf(SingleRecord, content)));
        codeLblPo skip = newLabel(ctx);
        cbnz(sng, skip);
        setStackDepth(stack, jit, tgtBlock->exitHeight);
        spillStack(stack, jit);
        propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
        ret = breakOut(block, tgtBlock);
        bind(skip);
        pushRegister(stack, sng);
        continue;
      }
      case TstSav: {
        // test a sav, return a logical
        armReg sng = popValue(stack, jit);
        armReg tr = findFreeReg(jit);
        armReg fl = findFreeReg(jit);

        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);
        ldr(sng, OF(sng, OffsetOf(SingleRecord, content)));
        tst(sng, IM((integer) Null));
        csel(sng, tr, fl, EQ);
        pushRegister(stack, sng);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        continue;
      }
      case StSav: {
        // store a value into a single assignment
        armReg sng = popValue(stack, jit);
        armReg val = popValue(stack, jit);

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
        continue;
      }
      case TSav: {
        armReg sng = popValue(stack, jit);
        armReg val = topValue(stack, jit);

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
        continue;
      }
      case Cell: {
        // create R/W cell
        spillStack(stack, jit);
        armReg cel = allocSmallStruct(block, cellClass, CellCellCount);
        armReg tmp = popValue(stack, jit);
        str(tmp, OF(cel, OffsetOf(CellRecord, content)));
        releaseReg(jit, tmp);
        pushRegister(stack, cel);
        continue;
      }
      case Get: {
        // access a R/W cell
        armReg cel = popValue(stack, jit);
        ldr(cel, OF(cel, OffsetOf(CellRecord, content)));
        pushRegister(stack, cel);
        continue;
      }
      case Assign: {
        // assign to a R/W cell
        armReg cel = popValue(stack, jit);
        armReg vl = popValue(stack, jit);
        str(vl, OF(cel, OffsetOf(CellRecord, content)));
        releaseReg(jit, cel);
        releaseReg(jit, vl);
        continue;
      }
      case CLbl: {
        // T,Lbl --> test for a data term, break if not lbl
        int32 key = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        armReg vl = popValue(stack, jit);
        armReg tmp = findFreeReg(jit);

        and(tmp, vl, IM(0b11));
        cmp(tmp, IM(0b00));
        breakOutNe(block, code, tgt);

        ldr(tmp, OF(vl, OffsetOf(TermRecord,clss))); // pick up the class
        loadConstant(jit, key, vl);
        cmp(tmp, RG(vl));

        breakOutNe(block, code, tgt);
        releaseReg(jit, tmp);
        releaseReg(jit, vl);
        continue;
      }

      case CInt:
      case CChar:
      case CFlt: {
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);
        armReg st = popValue(stack, jit);

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

        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(stack, jit, tgtBlock->exitHeight);
          spillStack(stack, jit);
          propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
          bne(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgt);
        continue;
      }
      case CLit: {
        // T,lit --> test for a literal value, break if not
        int32 key = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);

        armReg lit = findFreeReg(jit);
        loadConstant(jit, key, lit);
        armReg vl = popValue(stack, jit);

        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) sameTerm, 2, RG(lit), RG(vl));
        unstash(jit);
        tst(X0, RG(X0));

        valueStackPo tgtStack = &tgtBlock->stack;
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          spillStack(stack, jit);
          setStackDepth(tgtStack, jit, tgtBlock->exitHeight);
          propagateStack(jit, stack, tgtStack, tgtBlock->exitHeight);
          beq(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgt);

        releaseReg(jit, vl);
        releaseReg(jit, lit);
        continue;
      }

      case Nth: {
        // T --> el, pick up the nth element
        armReg vl = popValue(stack, jit);
        loadOffset(jit, vl, vl, code[pc].fst + 1);
        pushRegister(stack, vl);
        continue;
      }
      case StNth: {
        // T el --> store in nth element
        armReg trm = popValue(stack, jit);
        armReg vl = popValue(stack, jit);
        str(vl, OF(trm, (code[pc].fst + 1) * pointerSize));

        releaseReg(jit, vl);
        releaseReg(jit, trm);
        continue;
      }
      case If: {
        // break if true
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);
        armReg vl = popValue(stack, jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, trueIndex, tr);
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        setStackDepth(stack, jit, tgtBlock->exitHeight);
        spillStack(stack, jit);
        propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
        ret = breakOutEq(block, code, tgt);
        continue;
      }
      case IfNot: {
        // break if false
        int32 tgt = pc + code[pc].alt + 1;
        jitBlockPo tgtBlock = breakBlock(block, code, tgt);
        armReg vl = popValue(stack, jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, trueIndex, tr);
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        setStackDepth(stack, jit, tgtBlock->exitHeight);
        spillStack(stack, jit);
        propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
        ret = breakOutNe(block, code, tgt);
        continue;
      }
      case ICase: {
        armReg gr = popValue(stack, jit);
        int32 tableSize = code[pc].fst;
        spillStack(stack, jit); // We do this to ensure following breaks compile to one instruction
        getIntVal(jit, gr);
        and(gr, gr, IM(LARGE_INT61));
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(tableSize));
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
        handleBreakTable(block, code, pc + 1, tableSize);
        return ret;
      }
      case Case: {
        // T --> T, case <Max>
        int32 tableSize = code[pc].fst;
        armReg vl = popValue(stack, jit);
        spillStack(stack, jit);
        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) hashTerm, 1, RG(vl));
        releaseReg(jit, vl);
        unstash(jit);
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(tableSize));
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
        bind(jmpTbl);
        handleBreakTable(block, code, pc + 1, tableSize);
        return ret;
      }
      case IxCase: {
        // check and jump on index
        int32 tableSize = code[pc].fst;
        armReg tgt = popValue(stack, jit);
        spillStack(stack, jit);
        armReg ix = findFreeReg(jit);
        ldr(ix, OF(tgt, 0)); // Pick up the label
        ldr(ix, OF(ix, OffsetOf(LblRecord, index)));
        // Make sure that it is less than max
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(tableSize));
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
        handleBreakTable(block, code, pc + 1, tableSize);
        return ret;
      }
      case IAdd: {
        // L R --> L+R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        add(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case ISub: {
        // L R --> L-R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        sub(a1, a1, RG(a2));

        mkIntVal(jit, a1);
        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case IMul: {
        // L R --> L*R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        mul(a1, a2, a1);

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case IDiv: {
        // L R --> L/R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        cbnz(a2, skip);

        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(stack, jit, tgtBlock->exitHeight - 1);
          pushValue(stack, (LocalEntry){.kind = isConstant, .key = divZeroIndex});
          spillStack(stack, jit);
          propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        sdiv(a1, a1, a2);
        mkIntVal(jit, a1);
        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case IMod: {
        // L R --> L%R
        armReg a1 = popValue(stack, jit);
        armReg divisor = popValue(stack, jit);
        getIntVal(jit, a1);
        getIntVal(jit, divisor);

        codeLblPo skip = newLabel(ctx);
        cbnz(divisor, skip);

        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(stack, jit, tgtBlock->exitHeight - 1);
          pushValue(stack, (LocalEntry){.kind = isConstant, .key = divZeroIndex});
          spillStack(stack, jit);
          propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);

        armReg quotient = findFreeReg(jit);
        sdiv(quotient, a1, divisor);
        msub(a1, divisor, quotient, a1);

        mkIntVal(jit, a1);
        pushRegister(stack, a1);

        releaseReg(jit, divisor);
        releaseReg(jit, quotient);
        continue;
      }
      case IAbs: {
        // L --> abs(L)
        armReg a1 = popValue(stack, jit);

        getIntVal(jit, a1);

        cmp(a1, IM(0));
        csneg(a1, a1, a1, GE);

        mkIntVal(jit, a1);
        pushRegister(stack, a1);
        continue;
      }
      case CEq:
      case IEq: {
        // L R --> L==R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findFreeReg(jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, NE);
        pushRegister(stack, a1);
        releaseReg(jit, a2);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        continue;
      }
      case CLt:
      case ILt: {
        // L R --> L<R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findFreeReg(jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);

        cmp(a1, RG(a2));
        csel(a1, tr, fl, LT);
        pushRegister(stack, a1);
        releaseReg(jit, a2);
        releaseReg(jit, fl);
        releaseReg(jit, tr);
        continue;
      }
      case CGe:
      case IGe: {
        // L R --> L>=R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findFreeReg(jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);

        cmp(a1, RG(a2));
        csel(a1, tr, fl, GE);
        pushRegister(stack, a1);
        releaseReg(jit, a2);
        releaseReg(jit, fl);
        releaseReg(jit, tr);
        continue;
      }

      case BAnd: {
        // L R --> L&R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        and(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case BOr: {
        // L R --> L|R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        orr(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case BXor: {
        // L R --> L^R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        eor(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case BLsl: {
        // L R --> L<<R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsl(a1, a1, RG(a2));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case BLsr: {
        // L R --> L>>R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsr(a1, a1, RG(a2));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case BAsr: {
        // L R --> L>>>R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        asr(a1, a1, RG(a2));

        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case BNot: {
        // L --> ~L
        armReg a1 = popValue(stack, jit);

        getIntVal(jit, a1);
        mvn(a1, a1, LSL, 0);
        mkIntVal(jit, a1);

        pushRegister(stack, a1);
        continue;
      }
      case FAdd: {
        // L R --> L+R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fadd(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FSub: {
        // L R --> L-R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fsub(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FMul: {
        // L R --> L*R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fmul(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FDiv: {
        // L R --> L/R
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        cbnz(a2, skip);

        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(stack, jit, tgtBlock->exitHeight - 1);
          pushValue(stack, (LocalEntry){.kind = isConstant, .key = divZeroIndex});
          spillStack(stack, jit);
          propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fdiv(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FMod: {
        // L R --> L%R
        jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        cbnz(a2, skip);

        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(stack, jit, tgtBlock->exitHeight - 1);
          pushValue(stack, (LocalEntry){.kind = isConstant, .key = divZeroIndex});
          spillStack(stack, jit);
          propagateStack(jit, stack, &tgtBlock->stack, tgtBlock->exitHeight);
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

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FAbs: {
        // L --> abs(L)
        armReg a1 = popValue(stack, jit);
        getFltVal(jit, a1);

        fmov(FP(F0), RG(a1));
        fabs(F0, F0);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);

        pushRegister(stack, a1);
        continue;
      }
      case FEq: {
        // L R --> L==
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));

        fcmp(F0, F1);
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, NE);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FLt: {
        // L R --> L<R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));

        fcmp(F0, F1);

        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, GE);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case FGe: {
        // L R --> L>=R
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);

        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));

        fcmp(F0, F1);

        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, LT);

        pushRegister(stack, a1);
        releaseReg(jit, a2);
        continue;
      }
      case Alloc: {
        // new structure, elements from stack
        int32 key = code[pc].fst;
        labelPo label = C_LBL(getConstant(key));
        int32 arity = lblArity(label);
        spillStack(stack, jit);
        armReg term = allocSmallStruct(block, (clssPo) label, NormalCellCount(arity));

        for (int32 ix = 0; ix < arity; ix++) {
          armReg tmp = popValue(stack, jit);
          str(tmp, OF(term, (ix + 1) * pointerSize));
          releaseReg(jit, tmp);
        }

        pushRegister(stack, term);
        continue;
      }
      case Closure: {
        int32 key = code[pc].fst;

        spillStack(stack, jit);
        armReg term = allocSmallStruct(block, closureClass, ClosureCellCount);

        armReg tmp = findFreeReg(jit);
        loadConstant(jit, key, tmp);
        str(tmp, OF(term, OffsetOf(ClosureRecord, lbl)));
        releaseReg(jit, tmp);
        tmp = popValue(stack, jit); // pick up the free value
        str(tmp, OF(term, OffsetOf(ClosureRecord, free)));
        releaseReg(jit, tmp);

        pushRegister(stack, term);
        continue;
      }
      case Frame: {
        // frame instruction
        check(stack->vTop==code[pc].fst, "inconsistent frame height");
        continue;
      }
      case dBug: {
        // enter the line debugger
        int32 locKey = code[pc].fst;
        armReg loc = findFreeReg(jit);
        loadConstant(jit, locKey, loc);
        int32 npc = pc + 1;
        spillStack(stack, jit);
        stash(block);
        switch (code[npc].op) {
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
            loadConstant(jit, code[npc].fst, lbl);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) callDebug, 4, RG(PR), IM(code[npc].op), RG(loc),
                                RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case TCall: {
            armReg lbl = findFreeReg(jit);
            loadConstant(jit, code[npc].fst, lbl);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) tcallDebug, 3, RG(PR), RG(loc), RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case OCall:
          case XOCall: {
            armReg lbl = topValue(stack, jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) ocallDebug, 4, RG(PR), IM(code[npc].op), RG(loc),
                                RG(lbl));
            releaseReg(jit, lbl);
            break;
          }
          case Ret: {
            armReg vl = topValue(stack, jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) retDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case XRet: {
            armReg vl = topValue(stack, jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) xretDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Assign: {
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) assignDebug, 2, RG(PR), RG(loc));
            break;
          }
          case Fiber: {
            armReg vl = topValue(stack, jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) fiberDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Suspend: {
            armReg vl = topValue(stack, jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) suspendDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Resume: {
            armReg vl = topValue(stack, jit);
            ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) resumeDebug, 3, RG(PR), RG(loc), RG(vl));
            releaseReg(jit, vl);
            break;
          }
          case Retire: {
            armReg vl = topValue(stack, jit);
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
        stash(block);
        ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) lineDebug, 2, RG(PR), RG(loc));
        unstash(jit);
        releaseReg(jit, loc);
        continue;
      }
      default:
        return jitError(jit, "unknown instruction: %s", opNames[code[pc].op]);
    }
  }
#ifdef TRACEJIT
  if (traceJit >= generalTracing) {
    outMsg(logFile, "Non-breaking exit from block %d -> %d\n", from, endPc);
  }
#endif

  // We only come here if the block does not have a breaking
  setStackDepth(stack, jit, block->exitHeight);
  if (block->parent != Null)
    propagateStack(jit, stack, &block->parent->stack, block->exitHeight);

  return ret;
}

armReg allocSmallStruct(jitBlockPo block, clssPo class, integer amnt) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);

  codeLblPo ok = newLabel(ctx);

  reserveReg(jit, X0);
  armReg h = findFreeReg(jit);
  armReg c = findFreeReg(jit);
  armReg l = findFreeReg(jit);
  armReg reslt = findFreeReg(jit);
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
  stash(block); // Slow path
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

void pshFrame(jitBlockPo block, armReg mtdRg) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, OffsetOf(StackFrame, args)));
  sub(AG, AG, IM(trueStackDepth(&block->stack)*pointerSize));
  armReg tmp = findFreeReg(jit);
  ldr(tmp, OF(STK, OffsetOf(StackRecord, prog)));
  str(tmp, OF(FP, OffsetOf(StackFrame, prog))); // We know what program we are executing
  str(mtdRg, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  releaseReg(jit, tmp);
}

void dropArgs(valueStackPo stack, jitCompPo jit, int32 count) {
  dropValues(stack, jit, count);
  pushBlank(stack);
}

void handleBreakTable(jitBlockPo block, insPo code, int32 pc, int32 count) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  for (int ix = 0; ix < count; ix++, pc++) {
    check(code[pc].op==Break, "Expecting a Break instruction");
    jitBlockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1);
    setStackDepth(&tgtBlock->stack, jit, tgtBlock->exitHeight);
    codeLblPo lbl = breakLabel(tgtBlock);
    b(lbl);
  }
}
