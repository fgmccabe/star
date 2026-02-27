//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include <string.h>
#include "engineOptions.h"
#include "analyseP.h"
#include "cellP.h"
#include "lowerAP.h"
#include "stackP.h"
#include "singleP.h"
#include "globalsP.h"
#include "constantsP.h"
#include "jitP.h"
#include "closureP.h"
#include "debug.h"
#include "engineP.h"
#include "abort.h"
#include "arithmetic.h"
#include "arithP.h"
#include "debugP.h"
#include "errorCodes.h"
#include "formioP.h"
#include "shuffle.h"
#include "sort.h"

/* Lower Star VM code to Arm64 code */

/*
* X0-X7 = argument registers & scratch registers
* X0 = return status
* X1 = return value
* X8-X11 = Temporary registers
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
* We only use the SP register when entering C calls.
*/

static registerMap defaultArgRegs();
static registerMap systemArgRegs();

static retCode jitBlock(blockPo block, codeGenPo state, insPo code, int32 from, int32 endPc);
static blockPo blockBreak(blockPo block, insPo code, int32 tgt, OpCode blockType);
static void brkOut(assemCtxPo ctx, blockPo tgtBlock);
static void brkOutNe(assemCtxPo ctx, blockPo tgtBlock);

static void verifyState(codeGenPo state, int32 pc);

static void pushFrme(codeGenPo state, int32 pc, armReg mtdRg, int32 argOffset);
static armReg allocSmallStruct(codeGenPo state, int32 pc, clssPo class, integer amnt);
static retCode handleBreakTable(codeGenPo state, int32 pc, blockPo block, insPo code, int32 count);
static void testResult(codeGenPo state, int32 pc, blockPo tgtBlock);
static armReg mkFloat(codeGenPo state, int32 pc);
static void populateLocals(codeGenPo state);
static void pushVar(codeGenPo state, int32 pc, localVarPo v);
static localVarPo popVar(codeGenPo state, int32 pc);
static FlexOp popFlex(codeGenPo state, int32 pc);
static FlexOp topFlex(codeGenPo state, int32 pc);
static void storeFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt);
static void loadFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt);
static FlexOp constantFlex(codeGenPo state, int32 index);
static FlexOp localFlex(codeGenPo state, int32 index);
static void loadRegister(codeGenPo state, FlexOp src, armReg rg);
static void dumpState(codeGenPo state);
static void invokeIntrinsic(codeGenPo state, int32 pc, runtimeFn fn, int32 arity, FlexOp args[]);
static void invokeEscape(codeGenPo state, int32 pc, runtimeFn fn, int32 arity);
static int32 loadArguments(codeGenPo state, int32 pc, int32 arity);
static int32 loadOArguments(codeGenPo state, int32 pc, int32 arity, armReg frReg);
static void dropArguments(codeGenPo state, int32 arity, int32 pc);
static int32 overrideArguments(codeGenPo state, int32 pc, int32 arity);
static int32 overrideOArguments(codeGenPo state, int32 pc, int32 arity, armReg arg1);
static void overrideFrame(codeGenPo state, int32 pc, int32 arity);
static void overrideOFrame(codeGenPo state, int32 pc, int32 arity, armReg arg1);
static void adjustAG(codeGenPo state, int32 pc, int32 tgtOff);
static localVarPo findPhiVariable(codeGenPo state, int32 pc);
static localVarPo markVarStart(codeGenPo state, int32 pc, FlexOp src);
static logical liveVar(localVarPo var, int32 pc);
static FlexOp getLclSrc(codeGenPo state, int32 pc, int32 lclNo);
static int32 minLiveOffset(codeGenPo state, int32 pc);
static void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var);
static armReg findARegister(codeGenPo state, int32 pc);
static retCode showLocalVar(ioPo out, void *data, long depth, long precision, logical alt);

static localVarPo argSlot(codeGenPo state, int32 ax);
static void showLiveLocals(ioPo out, codeGenPo state);
static void retireExpiredVars(codeGenPo state, int32 pc);
static void voidOutFrameLocals(codeGenPo state, int32 pc, int32 minOffset);
static int32 stashLiveLocals(codeGenPo state, int32 pc, int32 cnt);
static void stashLocal(codeGenPo state, int32 pc, localVarPo v);
static localVarPo findLocal(codeGenPo state, int32 pc, int32 lx);
static localVarPo allocateLocal(codeGenPo state, int32 pc);
static int32 nextStkOff(codeGenPo state, int32 pc);

#define argSpec(s,d) (ArgSpec){.src = s, .dst = d, .mark = True, .group = -1}

retCode jitInstructionsA(jitCompPo jit, methodPo mtd, char *errMsg, integer msgLen) {
  AnalysisRecord analysis;
  if (analyseMethod(mtd, &analysis) == Ok) {
    showAnalysis(logFile, &analysis);
  }

  int32 numSlots = slotCount(&analysis);
  LocalVar locals[numSlots];
  localVarPo stack[numSlots];

  CodeGenState state = {
    .mtd = mtd, .analysis = &analysis, .locals = locals, .numLocals = numSlots, .argPt = numSlots - mtdArity(mtd),
    .jit = jit, .top = 0, .stack = stack
  };

  populateLocals(&state);

#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method %L\n", mtd);
    reinstallMsgProc('X', showLocalVar);
  }
#endif

  JitBlock block = {
    .startPc = 0, .endPc = codeSize(mtd),
    .breakLbl = Null, .loopLbl = Null,
    .parent = Null, .phiVar = Null
  };

  retCode ret = jitBlock(&block, &state, entryPoint(mtd), 0, codeSize(mtd));

  tearDownAnalysis(&analysis);
  return ret;
}

retCode jitSpecialInstructionsA(jitCompPo jit, methodPo mtd, int32 depth) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit special method %L\n", mtd);
  }
#endif

  AnalysisRecord analysis;
  if (analyseMethod(mtd, &analysis) == Ok) {
    showAnalysis(logFile, &analysis);
  }

  int32 numSlots = slotCount(&analysis);
  LocalVar locals[numSlots];
  localVarPo stack[numSlots];

  CodeGenState state = {
    .mtd = mtd, .analysis = &analysis, .locals = locals, .numLocals = numSlots, .argPt = numSlots - mtdArity(mtd),
    .jit = jit, .stack = stack
  };
  populateLocals(&state);

  JitBlock block = {
    .startPc = 0, .endPc = codeSize(mtd), .breakLbl = Null, .loopLbl = Null, .parent = Null,
  };

  retCode ret = jitBlock(&block, &state, entryPoint(mtd), 0, codeSize(mtd));

  tearDownAnalysis(&analysis);
  return ret;
}

retCode jitBlock(blockPo block, codeGenPo state, insPo code, int32 from, int32 endPc) {
  retCode ret = Ok;
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);

#ifdef TRACEJIT
  if (traceJit >= generalTracing) {
    outMsg(logFile, "Jit block %d -> %d\n%_", from, endPc);
  }
#endif
  for (int32 pc = from; ret == Ok && pc < endPc; pc++) {
    retireExpiredVars(state, pc);
    verifyState(state, pc);
#ifdef TRACEJIT
    if (traceJit >= generalTracing) {
      disass(logFile, Null, jit->mtd, &code[pc]);
      outMsg(logFile, "\n%_");
    }
    if (traceJit >= detailedTracing) {
      dumpState(state);
    }
#endif
    switch (code[pc].op) {
      case Halt: {
        // Stop execution
        FlexOp src = popFlex(state, pc);
        invokeIntrinsic(state, pc, (runtimeFn) star_exit, 2, (FlexOp[]){RG(PR), src});
        continue;
      }
      case Abort: {
        // abort with message
        armReg loc = findARegister(state, pc);
        FlexOp val = popFlex(state, pc);
        loadConstant(jit, code[pc].fst, loc);
        invokeIntrinsic(state, pc, (runtimeFn) abort_star, 3, (FlexOp[]){RG(PR), RG(loc), val});
        releaseReg(jit, loc);
        continue;
      }
      case Call: {
        int32 key = code[pc].fst;
        int32 arity = lblArity(C_LBL(getConstant(key)));
        int32 argOffset = loadArguments(state, pc, arity);
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
        pushFrme(state, pc, X17, argOffset);
        blr(X16);
        dropArguments(state, arity, pc);
        pushVar(state, pc, markVarStart(state, pc, RG(RTV)));
        continue;
      }
      case XCall: {
        int32 key = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        int32 arity = lblArity(C_LBL(getConstant(key)));

        int32 argOffset = loadArguments(state, pc, arity);
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
        pushFrme(state, pc, X17, argOffset);
        blr(X16);
        dropArguments(state, arity, pc);
        testResult(state, pc, blockBreak(block, code, tgt, Valof));
        continue;
      }

      case OCall: {
        int32 arity = code[pc].fst;
        armReg temp = findARegister(state, pc);
        FlexOp cl = popFlex(state, pc); // Pick up the closure
        loadRegister(state, cl, temp);
        ldr(X17, OF(temp, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(temp, OF(temp, OffsetOf(ClosureRecord, free))); // Pick up the free term
        int32 argOffset = loadOArguments(state, pc, arity, temp);
        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pushFrme(state, pc, X17, argOffset);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArguments(state, arity, pc);
        pushVar(state, pc, markVarStart(state, pc, RG(RTV)));
        continue;
      }
      case XOCall: {
        int32 arity = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        armReg temp = findARegister(state, pc);
        FlexOp cl = popFlex(state, pc); // Pick up the closure
        loadRegister(state, cl, temp);
        ldr(X17, OF(temp, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        ldr(temp, OF(temp, OffsetOf(ClosureRecord, free))); // Pick up the free term
        int32 argOffset = loadOArguments(state, pc, arity, temp);

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pushFrme(state, pc, X17, argOffset);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArguments(state, arity, pc);
        testResult(state, pc, blockBreak(block, code, tgt, Valof));
        continue;
      }
      case TCall: {
        // // TCall <prog>
        int32 key = code[pc].fst;
        int arity = lblArity(C_LBL(getConstant(key)));

        loadConstant(jit, key, X16);
        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));
        // Update current frame
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        overrideFrame(state, pc, arity);
        str(AG, OF(STK, OffsetOf(StackRecord,args)));

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);
        return ret;
      }
      case TOCall: {
        int32 arity = code[pc].fst;
        armReg temp = findARegister(state, pc);
        FlexOp cl = popFlex(state, pc); // Pick up the closure
        loadRegister(state, cl, temp);

        ldr(X17, OF(temp, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        // Update current frame
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
        ldr(temp, OF(temp, OffsetOf(ClosureRecord, free))); // Pick up the free term
        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        overrideOFrame(state, pc, arity, temp);

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
        invokeEscape(state, pc, (runtimeFn) escapeFun(esc), arity);
        pushVar(state, pc, markVarStart(state, pc, RG(RTV)));
        continue;
      }
      case XEscape: {
        int32 escNo = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;

        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);
        invokeEscape(state, pc, (runtimeFn) escapeFun(esc), arity);
        testResult(state, pc, blockBreak(block, code, tgt, Valof));
        continue;
      }
      case Entry: {
        // locals definition
        continue;
      }
      case Ret: {
        FlexOp vl = popFlex(state, pc);

        // Pick up the caller program
        ldr(X16, OF(FP, OffsetOf(StackFrame, prog)));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Only need this for debugging
        if (mtdArity(jit->mtd) != 1) {
          int32 delta = mtdArity(jit->mtd) - 1;
          if (delta < 0)
            sub(AG, AG, IM(-delta*pointerSize));
          else
            add(AG, AG, IM(delta*pointerSize));
        }
        str(AG, OF(STK, OffsetOf(StackRecord,sp)));

        // Adjust args register
        ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
        // Pick up return address
        ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        mov(RTV, vl);
        mov(RTS, IM(Normal));
        br(X16);

        return ret;
      }
      case XRet: {
        // exception return
        FlexOp vl = popFlex(state, pc);

        // Only need this for debugging
        if (mtdArity(jit->mtd) != 1) {
          int32 delta = mtdArity(jit->mtd) - 1;
          if (delta < 0)
            sub(AG, AG, IM(-delta*pointerSize));
          else
            add(AG, AG, IM(delta*pointerSize));
        }
        stur(AG, STK, OffsetOf(StackRecord,sp));

        // Pick up the caller program
        ldr(X16, OF(FP, OffsetOf(StackFrame, prog)));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust args register
        ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
        // Pick up return address
        ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        mov(RTV, vl);
        mov(RTS, IM(Abnormal));
        br(X16);

        return ret;
      }
      case Valof: {
        // vlof block of instructions
        int32 blockExitDepth = code[pc].fst;
        int32 blockLen = code[pc].alt;
        codeLblPo brkLbl = newLabel(ctx);

        JitBlock subBlock = {
          .startPc = pc,
          .endPc = pc + blockLen + 1,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
          .phiVar = findPhiVariable(state, pc + blockLen + 1)
        };

        ret = jitBlock(&subBlock, state, code, pc + 1, pc + blockLen + 1);
        pc += blockLen; // Skip over the block
        bind(brkLbl);
        retireExpiredVars(state, pc);
        state->top = blockExitDepth;
        continue;
      }
      case Block: {
        // block of instructions
        int32 blockExitDepth = code[pc].fst;
        int32 blockLen = code[pc].alt;
        codeLblPo brkLbl = newLabel(ctx);

        JitBlock subBlock = {
          .startPc = pc,
          .endPc = pc + blockLen + 1,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
          .phiVar = Null
        };

        ret = jitBlock(&subBlock, state, code, pc + 1, pc + blockLen + 1);
        pc += blockLen; // Skip over the block
        retireExpiredVars(state, pc);
        bind(brkLbl);
        state->top = blockExitDepth;

        continue;
      }
      case Break: {
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = blockBreak(block, code, tgt, Block);
        brkOut(ctx, tgtBlock);
        return Ok;
      }
      case Result: {
        // return value out of block
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = blockBreak(block, code, tgt, Valof);
        blockPo parent = tgtBlock->parent;
        localVarPo phiVar = parent->phiVar;

        FlexOp val = popFlex(state, pc);
        storeVar(state, pc, val, phiVar);

        brkOut(ctx, tgtBlock);
        return Ok;
      }
      case Loop: {
        // jump back to start of block
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = blockBreak(block, code, tgt, Block);
        codeLblPo loop = loopLabel(tgtBlock);
        assert(loop != Null);
        b(loop);
        return ret;
      }
      case Drop: {
        state->top--;
        continue;
      }
      case Rot: {
        // Pull up nth element of stack
        int32 cnt = code[pc].fst;
        assert(cnt>=0 && cnt<state->top);
        if (cnt > 0) {
          localVarPo tmp = state->stack[state->top];
          for (int32 ix = 0; ix < cnt; ix++) {
            state->stack[ix] = state->stack[ix + 1];
          }
          state->stack[cnt] = tmp;
        }
        continue;
      }
      case Rst: {
        // reset stack height to a fixed height
        state->top = code[pc].fst;
        continue;
      }
      case Fiber: {
        FlexOp lam = popFlex(state, pc);
        invokeIntrinsic(state, pc, (runtimeFn) newStack, 3, (FlexOp[]){RG(PR), IM(True), lam});
        localVarPo nv = markVarStart(state, pc, RG(RTV));
        pushVar(state, pc, nv);
        continue;
      }
      case Suspend: {
        armReg tmp = findARegister(state, pc);
        FlexOp stk = popFlex(state, pc);
        FlexOp evt = popFlex(state, pc);
        codeLblPo rtn = newLabel(ctx);
        adr(tmp, rtn);
        str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
        invokeIntrinsic(state, pc, (runtimeFn) detachStack, 3, (FlexOp[]){RG(PR), stk, evt});
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        releaseReg(jit, tmp);
        pushVar(state, pc, markVarStart(state, pc, RG(RTV)));
        continue;
      }
      case Resume: {
        FlexOp stk = popFlex(state, pc);
        FlexOp evt = popFlex(state, pc);
        codeLblPo rtn = newLabel(ctx);
        adr(X16, rtn);
        str(X16, OF(STK, OffsetOf(StackRecord, pc)));
        invokeIntrinsic(state, pc, (runtimeFn) attachStack, 3, (FlexOp[]){RG(PR), stk, evt});
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        pushVar(state, pc, markVarStart(state, pc, RG(RTV)));
        continue;
      }
      case Retire: {
        // Similar to suspend, except that we trash the suspending stack
        FlexOp stk = popFlex(state, pc);
        FlexOp evt = popFlex(state, pc);
        invokeIntrinsic(state, pc, (runtimeFn) detachDropStack, 2, (FlexOp[]){stk, evt});
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        return ret;
      }
      case Underflow: {
        // underflow from current stack
        FlexOp val = popFlex(state, pc);
        invokeIntrinsic(state, pc, (runtimeFn) detachDropStack, 3, (FlexOp[]){RG(PR), RG(STK), val});
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        continue;
      }
      case LdV: {
        // Place a void value on stack
        localVarPo nv = markVarStart(state, pc, constantFlex(state, voidIndex));
        pushVar(state, pc, nv);
        continue;
      }
      case LdC: {
        // load literal from constant pool
        pushVar(state, pc, markVarStart(state, pc, constantFlex(state, code[pc].fst)));
        continue;
      }
      case Ld: {
        // load stack from lcl[xx]
        localVarPo vr = findLocal(state, pc, code[pc].fst);
        pushVar(state, pc, markVarStart(state, pc, vr->src));
        continue;
      }
      case St: {
        // copy tos to local[xx]
        localVarPo lcl = findLocal(state, pc, code[pc].fst);
        FlexOp vl = popFlex(state, pc);
        storeVar(state, pc, vl, lcl);
        continue;
      }
      case StV: {
        // clear a local to void
        localVarPo lcl = findLocal(state, pc, code[pc].fst);
        storeVar(state, pc, constantFlex(state, voidIndex), lcl);
        continue;
      }
      case Tee: {
        // copy tos to local[xx]
        localVarPo lcl = findLocal(state, pc, code[pc].fst);
        FlexOp vl = topFlex(state, pc);
        storeVar(state, pc, vl, lcl);
        continue;
      }
      case LdG: {
        // load a global variable
        armReg glb = findFreeReg(jit);
        armReg content = findFreeReg(jit);
        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr));
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
        int32 argOffset = loadArguments(state, pc, 0); // No actual arguments!

        pushFrme(state, pc, X17, argOffset);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));

        codeLblPo returnPc = newLabel(ctx);
        adr(LR, returnPc);
        br(X16);

        bind(haveContent);
        mov(RTV, RG(content));

        bind(returnPc);

        pushVar(state, pc, markVarStart(state, pc, RG(RTV)));
        releaseReg(jit, glb);
        releaseReg(jit, content);
        continue;
      }
      case StG: {
        // store into a global variable
        FlexOp tmp = popFlex(state, pc);
        armReg glb = findFreeReg(jit);

        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Assign to the global var's content field
        storeFlex(state, pc, tmp, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, glb);
        continue;
      }
      case TG: {
        // copy into a global variable
        armReg glb = findARegister(state, pc);
        FlexOp vl = topFlex(state, pc);

        globalPo glbVr = findGlobalVar(code[pc].fst);

        mov(glb, IM((integer) glbVr)); // Global var structures are not subject to GC

        // Assign to the global var's content field
        storeFlex(state, pc, vl, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, glb);
        continue;
      }
      case Sav: {
        // create a single assignment variable
        armReg cel = allocSmallStruct(state, pc, singleClass, SingleCellCount);
        armReg tmp = findARegister(state, pc);
        mov(tmp, IM((integer) Null));
        storeFlex(state, pc, constantFlex(state, voidIndex), OF(cel, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        pushVar(state, pc, markVarStart(state, pc, RG(cel)));
        continue;
      }
      case LdSav: {
        // dereference a sav, break if not set
        FlexOp sng = popFlex(state, pc);
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        armReg vd = findARegister(state, pc);
        loadConstant(jit, voidIndex, vd);
        ldr(tmp, OF(tmp, OffsetOf(SingleRecord, content)));
        cmp(vd, RG(tmp));
        brkOutNe(ctx, blockBreak(block, code, pc + code[pc].alt + 1, Block));
        pushVar(state, pc, markVarStart(state, pc, RG(tmp)));
        continue;
      }
      case TstSav: {
        // test a sav, return a logical
        FlexOp sng = popFlex(state, pc);
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        armReg tr = findARegister(state, pc);
        armReg fl = findARegister(state, pc);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);
        armReg vd = findARegister(state, pc);
        loadConstant(jit, voidIndex, vd);
        cmp(vd, OF(tmp, OffsetOf(SingleRecord, content)));
        csel(tmp, tr, fl, EQ);
        pushVar(state, pc, markVarStart(state, pc, RG(tmp)));
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        releaseReg(jit, vd);
        continue;
      }
      case StSav: {
        // store a value into a single assignment
        FlexOp sng = popFlex(state, pc);
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        FlexOp val = popFlex(state, pc);
        storeFlex(state, pc, val, OF(tmp, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        continue;
      }
      case TSav: {
        FlexOp sng = topFlex(state, pc);
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        FlexOp val = popFlex(state, pc);
        storeFlex(state, pc, val, OF(tmp, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        continue;
      }
      case Cell: {
        // create R/W cell
        armReg cel = allocSmallStruct(state, pc, cellClass, CellCellCount);
        FlexOp vl = popFlex(state, pc);
        storeFlex(state, pc, vl,OF(cel, OffsetOf(CellRecord, content)));
        pushVar(state, pc, markVarStart(state, pc, RG(cel)));
        continue;
      }
      case Get: {
        // access a R/W cell
        FlexOp cel = popFlex(state, pc);
        armReg vl = findARegister(state, pc);

        loadFlex(state, pc, cel,RG(vl));
        ldr(vl, OF(vl, OffsetOf(CellRecord, content)));
        pushVar(state, pc, markVarStart(state, pc, RG(vl)));
        continue;
      }
      case Assign: {
        // assign to a R/W cell
        FlexOp cel = popFlex(state, pc);
        FlexOp vl = popFlex(state, pc);
        armReg tmp = findARegister(state, pc);
        armReg tmp2 = findARegister(state, pc);
        loadFlex(state, pc, cel,RG(tmp));
        loadFlex(state, pc, vl,RG(tmp2));

        str(tmp2, OF(tmp, OffsetOf(CellRecord, content)));

        releaseReg(jit, tmp);
        releaseReg(jit, tmp2);
        continue;
      }
      case CLbl: {
        // T,Lbl --> test for a data term, break if not lbl
        int32 key = code[pc].fst;
        blockPo tgt = blockBreak(block, code, pc + code[pc].alt + 1, Block);
        armReg tmp = findARegister(state, pc);
        armReg tmp2 = findARegister(state, pc);
        FlexOp vl = popFlex(state, pc);
        loadRegister(state, vl, tmp);
        tst(tmp, IM(0b11));
        brkOutNe(ctx, tgt);

        ldr(tmp, OF(tmp, OffsetOf(TermRecord,clss))); // pick up the class
        loadConstant(jit, key, tmp2);
        cmp(tmp2, RG(tmp));

        brkOutNe(ctx, tgt);
        releaseReg(jit, tmp);
        releaseReg(jit, tmp2);
        continue;
      }

      case CInt:
      case CChar: {
        armReg tmp = findARegister(state, pc);
        FlexOp vl = popFlex(state, pc);
        loadRegister(state, vl, tmp);

        integer lit = (integer) getConstant(code[pc].fst);
        if (is12bit(lit))
          cmp(tmp, IM(lit));
        else {
          armReg litReg = findFreeReg(jit);
          loadConstant(jit, code[pc].fst, litReg);
          cmp(tmp, RG(litReg));
          releaseReg(jit, litReg);
        }
        releaseReg(jit, tmp);
        blockPo tgt = blockBreak(block, code, pc + code[pc].alt + 1, Block);
        brkOutNe(ctx, tgt);
        continue;
      }
      case CFlt:
      case CLit: {
        // T,lit --> test for a literal value, break if not
        int32 key = code[pc].fst;
        FlexOp vl = popFlex(state, pc);
        invokeIntrinsic(state, pc, (runtimeFn) sameTerm, 2, (FlexOp[]){vl, constantFlex(state, key)});
        tst(RTV, RG(RTV));
        blockPo tgt = blockBreak(block, code, pc + code[pc].alt + 1, Block);
        brkOutNe(ctx, tgt);
        continue;
      }

      case Nth: {
        // T --> el, pick up the nth element
        armReg tmp = findARegister(state, pc);
        FlexOp vl = popFlex(state, pc);
        loadRegister(state, vl, tmp);
        loadElement(jit, tmp, tmp, code[pc].fst + 1);
        pushVar(state, pc, markVarStart(state, pc, RG(tmp)));
        continue;
      }
      case StNth: {
        // T el --> store in nth element
        armReg tmp = findARegister(state, pc);
        armReg tmp2 = findARegister(state, pc);
        FlexOp trm = popFlex(state, pc);
        FlexOp vl = popFlex(state, pc);
        loadRegister(state, trm, tmp);
        loadRegister(state, vl, tmp2);
        storeElement(jit, tmp2, tmp, (code[pc].fst + 1));
        releaseReg(jit, tmp);
        releaseReg(jit, tmp2);
        continue;
      }
      case If: {
        // break if true
        int32 tgt = pc + code[pc].alt + 1;
        armReg tmp = findARegister(state, pc);
        FlexOp vl = popFlex(state, pc);
        loadConstant(jit, trueIndex, tmp);
        cmp(tmp, vl);
        breakOutEq(block, code, tgt);
        releaseReg(jit, tmp);
        continue;
      }
      case IfNot: {
        // break if false
        blockPo tgt = blockBreak(block, code, pc + code[pc].alt + 1, Block);
        armReg tmp = findARegister(state, pc);
        FlexOp vl = popFlex(state, pc);
        loadConstant(jit, trueIndex, tmp);
        cmp(tmp, vl);
        brkOutNe(ctx, tgt);
        releaseReg(jit, tmp);
        continue;
      }
      case ICase: {
        armReg ix = findARegister(state, pc);
        int32 tableSize = code[pc].fst;
        localVarPo tgt = popVar(state, pc);
        assert(tgt->live);
        loadRegister(state, tgt->src, ix);
        getIntVal(jit, ix);
        and(ix, ix, IM(LARGE_INT61));

        immModulo(ctx, ix, tableSize, jit->freeRegs);

        codeLblPo jmpTbl = newLabel(ctx);
        armReg off = findARegister(state, pc);
        adr(off, jmpTbl);
        add(off, off, LS(ix, 2));
        br(off);
        releaseReg(jit, off);
        releaseReg(jit, ix);
        bind(jmpTbl);
        return handleBreakTable(state, pc + 1, block, code, tableSize);
      }
      case Case: {
        // T --> T, case <Max>
        armReg ix = findARegister(state, pc);
        int32 tableSize = code[pc].fst;
        localVarPo tgt = popVar(state, pc);
        assert(tgt->live);
        invokeIntrinsic(state, pc, (runtimeFn) hashTerm, 1, (FlexOp[]){tgt->src});
        mov(ix, RG(RTV));
        and(ix, ix, IM(LARGE_INT61));

        immModulo(ctx, ix, tableSize, jit->freeRegs);

        codeLblPo jmpTbl = newLabel(ctx);
        armReg off = findARegister(state, pc);
        adr(off, jmpTbl);
        add(off, off, LS(ix, 2));
        br(off);
        releaseReg(jit, off);
        releaseReg(jit, ix);
        bind(jmpTbl);
        return handleBreakTable(state, pc + 1, block, code, tableSize);
      }
      case IxCase: {
        // check and jump on index
        armReg ix = findARegister(state, pc);
        int32 tableSize = code[pc].fst;
        FlexOp tgt = popFlex(state, pc);
        loadRegister(state, tgt, ix);
        ldr(ix, OF(ix, 0)); // Pick up the label
        ldr(ix, OF(ix, OffsetOf(LblRecord, index)));
        immModulo(ctx, ix, tableSize, jit->freeRegs);
        codeLblPo jmpTbl = newLabel(ctx);
        armReg off = findARegister(state, pc);
        adr(off, jmpTbl);
        add(off, off, LS(ix, 2));
        br(off);
        releaseReg(jit, off);
        releaseReg(jit, ix);
        bind(jmpTbl);
        return handleBreakTable(state, pc + 1, block, code, tableSize);
      }
      case IAdd: {
        // L R --> L+R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        add(a1, a2, RG(a1));

        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case ISub: {
        // L R --> L-R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        sub(a1, a1, RG(a2));

        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case IMul: {
        // L R --> L*R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        mul(a1, a1, a2);

        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case IDiv: {
        // L R --> L/R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg divisor = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, divisor);
        getIntVal(jit, a1);
        getIntVal(jit, divisor);

        codeLblPo skip = newLabel(ctx);
        cbnz(divisor, skip);

        blockPo tgtBlock = blockBreak(block, code, pc + code[pc].alt + 1, Valof);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null && tgtBlock->phiVar != Null) {
          storeVar(state, pc, constantFlex(state, divZeroIndex), tgtBlock->phiVar);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", pc);

        bind(skip);
        sdiv(a1, a1, divisor);
        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, divisor);
        continue;
      }
      case IMod: {
        // L R --> L%R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg divisor = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, divisor);
        getIntVal(jit, a1);
        getIntVal(jit, divisor);

        codeLblPo skip = newLabel(ctx);
        cbnz(divisor, skip);

        blockPo tgtBlock = blockBreak(block, code, pc + code[pc].alt + 1, Valof);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null && tgtBlock->phiVar != Null) {
          storeVar(state, pc, constantFlex(state, divZeroIndex), tgtBlock->phiVar);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", pc);

        bind(skip);
        armReg quotient = findARegister(state, pc);
        sdiv(quotient, a1, divisor);
        msub(a1, divisor, quotient, a1);

        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, divisor);
        releaseReg(jit, quotient);
        continue;
      }
      case IAbs: {
        // L --> abs(L)
        FlexOp left = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        loadRegister(state, left, a1);
        getIntVal(jit, a1);

        cmp(a1, IM(0));
        csneg(a1, a1, a1, GE);

        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        continue;
      }
      case CEq:
      case IEq: {
        // L R --> L==R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, NE);

        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, a2);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        continue;
      }
      case CLt:
      case ILt: {
        // L R --> L<R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, LT);

        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, a2);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        continue;
      }
      case CGe:
      case IGe: {
        // L R --> L>=R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, GE);

        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, a2);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        continue;
      }

      case BAnd: {
        // L R --> L&R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        and(a1, a1, RG(a2));
        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case BOr: {
        // L R --> L|R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        orr(a1, a2, RG(a1));
        mkIntVal(jit, a1);

        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, a2);
        continue;
      }
      case BXor: {
        // L R --> L^R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        eor(a1, a2, RG(a1));
        mkIntVal(jit, a1);

        pushVar(state, pc, markVarStart(state, pc, RG(a1)));

        releaseReg(jit, a2);
        continue;
      }
      case BLsl: {
        // L R --> L<<R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsl(a1, a1, RG(a2));
        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case BLsr: {
        // L R --> L>>R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsr(a1, a1, RG(a2));
        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case BAsr: {
        // L R --> L>>>R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        asr(a1, a1, RG(a2));
        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case BNot: {
        // // L --> ~L
        FlexOp left = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        loadRegister(state, left, a1);
        getIntVal(jit, a1);

        mvn(a1, a1, LSL, 0);
        mkIntVal(jit, a1);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        continue;
      }
      case FAdd: {
        // L R --> L+R
        armReg reslt = mkFloat(state, pc); // We create it first
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);
        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);
        fadd(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushVar(state, pc, markVarStart(state, pc, RG(reslt)));
        continue;
      }
      case FSub: {
        // L R --> L-R
        armReg reslt = mkFloat(state, pc); // We create it first
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);
        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);
        fsub(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushVar(state, pc, markVarStart(state, pc, RG(reslt)));
        continue;
      }
      case FMul: {
        // L R --> L*R
        armReg reslt = mkFloat(state, pc); // We create it first
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);
        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);
        fmul(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushVar(state, pc, markVarStart(state, pc, RG(reslt)));
        continue;
      }
      case FDiv: {
        // L R --> L/R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg divisor = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, divisor);

        getFltVal(jit, a1, F0);
        getFltVal(jit, divisor, F1);

        fmov(FP(F2), RG(XZR));
        fcmp(F1, F2);
        codeLblPo skip = newLabel(ctx);
        bne(skip);

        blockPo tgtBlock = blockBreak(block, code, pc + code[pc].alt + 1, Valof);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null && tgtBlock->phiVar != Null) {
          storeVar(state, pc, constantFlex(state, divZeroIndex), tgtBlock->phiVar);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", pc);

        bind(skip);

        stpf(F0, F1, PRX(SP,-16));
        armReg reslt = mkFloat(state, pc);
        ldpf(F0, F1, PSX(SP,16));
        fdiv(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        pushVar(state, pc, markVarStart(state, pc, RG(reslt)));
        releaseReg(jit, a1);
        releaseReg(jit, divisor);
        continue;
      }
      case FMod: {
        // L R --> L%R
        armReg reslt = mkFloat(state, pc); // Get space for the result

        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        armReg divisor = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, divisor);

        getFltVal(jit, a1, F0);
        getFltVal(jit, divisor, F1);

        fmov(FP(F2), RG(XZR));
        fcmp(F1, F2);
        codeLblPo skip = newLabel(ctx);
        bne(skip);

        blockPo tgtBlock = blockBreak(block, code, pc + code[pc].alt + 1, Valof);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null && tgtBlock->phiVar != Null) {
          storeVar(state, pc, constantFlex(state, divZeroIndex), tgtBlock->phiVar);
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", pc);

        bind(skip);

        fdiv(F0, F0, F1);
        fmsub(F2, F2, F1, F0);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        pushVar(state, pc, markVarStart(state, pc, RG(reslt)));
        releaseReg(jit, a1);
        releaseReg(jit, divisor);
        continue;
      }
      case FAbs: {
        // L --> abs(L)
        armReg reslt = mkFloat(state, pc);

        FlexOp left = popFlex(state, pc);

        armReg a1 = findARegister(state, pc);
        loadRegister(state, left, a1);

        getFltVal(jit, a1, F0);

        fabs(F0, F0);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushVar(state, pc, markVarStart(state, pc, RG(reslt)));
        releaseReg(jit, a1);
        continue;
      }
      case FEq: {
        // L R --> L==
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);
        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        fcmp(F0, F1);
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, NE);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case FLt: {
        // L R --> L<R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);
        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        fcmp(F0, F1);
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, GE);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case FGe: {
        // L R --> L>=R
        FlexOp left = popFlex(state, pc);
        FlexOp right = popFlex(state, pc);
        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, left, a1);
        loadRegister(state, right, a2);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        fcmp(F0, F1);
        loadConstant(jit, falseIndex, a1);
        loadConstant(jit, trueIndex, a2);
        csel(a1, a1, a2, LT);
        pushVar(state, pc, markVarStart(state, pc, RG(a1)));
        releaseReg(jit, a2);
        continue;
      }
      case Alloc: {
        // new structure, elements from stack
        int32 key = code[pc].fst;
        labelPo label = C_LBL(getConstant(key));
        int32 arity = lblArity(label);

        armReg term = allocSmallStruct(state, pc, (clssPo) label, NormalCellCount(arity));

        for (int32 ix = 0; ix < arity; ix++) {
          FlexOp tmp = popFlex(state, pc);
          storeFlex(state, pc, tmp,OF(term, (ix + 1) * pointerSize));
        }

        pushVar(state, pc, markVarStart(state, pc, RG(term)));
        continue;
      }
      case Closure: {
        int32 key = code[pc].fst;

        armReg term = allocSmallStruct(state, pc, closureClass, ClosureCellCount);

        armReg tmp = findARegister(state, pc);
        loadConstant(jit, key, tmp);

        str(tmp, OF(term, OffsetOf(ClosureRecord, lbl)));
        releaseReg(jit, tmp);

        FlexOp freeTerm = popFlex(state, pc);
        storeFlex(state, pc, freeTerm,OF(term, OffsetOf(ClosureRecord, free)));

        pushVar(state, pc, markVarStart(state, pc, RG(term)));
        continue;
      }
      case Frame: {
        // // frame instruction
        // check(stack->vTop==code[pc].fst, "inconsistent frame height");
        continue;
      }
      case dBug: {
        // enter the line debugger
        if (lineDebugging) {
          int32 locKey = code[pc].fst;
          int32 npc = pc + 1;
          stash(block);
          switch (code[npc].op) {
            case Abort: {
              invokeIntrinsic(state, pc, (runtimeFn) abortDebug, 2, (FlexOp[]){RG(PR), constantFlex(state, locKey)});
              break;
            }
            case Entry: {
              int32 lblKey = defineConstantLiteral((termPo) mtdLabel(jit->mtd));
              invokeIntrinsic(state, pc, (runtimeFn) entryDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), constantFlex(state, lblKey)
                              });
              break;
            }
            case Call:
            case XCall: {
              invokeIntrinsic(state, pc, (runtimeFn) callDebug, 4, (FlexOp[]){
                                RG(PR), IM(code[npc].op), constantFlex(state, locKey),
                                constantFlex(state, code[npc].fst)
                              });
              break;
            }
            case TCall: {
              invokeIntrinsic(state, pc, (runtimeFn) tcallDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey),
                                constantFlex(state, code[npc].fst)
                              });
              break;
            }
            case OCall:
            case XOCall: {
              FlexOp lbl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) ocallDebug, 4, (FlexOp[]){
                                RG(PR), IM(code[npc].op),
                                constantFlex(state, locKey), lbl
                              });
              break;
            }
            case TOCall: {
              FlexOp lbl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) tocallDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), lbl
                              });
              break;
            }
            case Ret: {
              FlexOp vl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) retDebug, 4, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), vl
                              });
              break;
            }
            case XRet: {
              FlexOp vl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) xretDebug, 4, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), vl
                              });
              break;
            }
            case Assign: {
              invokeIntrinsic(state, pc, (runtimeFn) assignDebug, 2, (FlexOp[]){RG(PR), constantFlex(state, locKey)});
              break;
            }
            case Fiber: {
              FlexOp vl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) fiberDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), vl
                              });
              break;
            }
            case Suspend: {
              FlexOp vl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) suspendDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), vl
                              });
              break;
            }
            case Resume: {
              FlexOp vl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) resumeDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), vl
                              });
              break;
            }
            case Retire: {
              FlexOp vl = topFlex(state, pc);
              invokeIntrinsic(state, pc, (runtimeFn) retireDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(state, locKey), vl
                              });
              break;
            }
            default:
              return jitError(jit, "invalid instruction following DBug");
          }
        }
        continue;
      }
      case Line: {
        if (lineDebugging) {
          int32 locKey = code[pc].fst;
          invokeIntrinsic(state, pc, (runtimeFn) lineDebug, 2, (FlexOp[]){RG(PR), constantFlex(state, locKey)});
        }
        continue;
      }
      case Bind: {
        if (lineDebugging) {
          int32 varKey = code[pc].fst;
          FlexOp vl = topFlex(state, pc);
          invokeIntrinsic(state, pc, (runtimeFn) bindDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(state, varKey), vl
                          });
        }
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

  return ret;
}

armReg allocSmallStruct(codeGenPo state, int32 pc, clssPo class, integer amnt) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);

  codeLblPo ok = newLabel(ctx);

  armReg h = findARegister(state, pc);
  armReg c = findARegister(state, pc);
  armReg l = findARegister(state, pc);
  armReg reslt = findARegister(state, pc);
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

  invokeIntrinsic(state, pc, (runtimeFn) allocateObject, 3, (FlexOp[]){
                    OF(PR, OffsetOf(EngineRecord, heap)), IM((integer) class), IM(amnt)
                  });
  mov(reslt, RG(X0));
  bind(ok);
  mov(c, IM((integer) class));
  str(c, OF(reslt, OffsetOf(TermRecord, clss)));
  releaseReg(jit, h);
  releaseReg(jit, c);
  releaseReg(jit, l);
  return reslt;
}

armReg mkFloat(codeGenPo state, int32 pc) {
  return allocSmallStruct(state, pc, floatClass, FloatCellCount);
}

void pushFrme(codeGenPo state, int32 pc, armReg mtdRg, int32 argOffset) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, OffsetOf(StackFrame, args)));
  adjustAG(state, pc, argOffset);
  armReg tmp = findFreeReg(jit);
  ldr(tmp, OF(STK, OffsetOf(StackRecord, prog)));
  str(tmp, OF(FP, OffsetOf(StackFrame, prog))); // We know what program we are executing
  str(mtdRg, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  releaseReg(jit, tmp);
}

void overrideFrame(codeGenPo state, int32 pc, int32 arity) {
#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    outMsg(logFile, "override frame in: ");
    dumpState(state);
  }
#endif

  int32 tgtOff = overrideArguments(state, pc, arity);
  adjustAG(state, pc, tgtOff);
}

void overrideOFrame(codeGenPo state, int32 pc, int32 arity, armReg arg1) {
#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    outMsg(logFile, "override frame in: ");
    dumpState(state);
  }
#endif

  int32 tgtOff = overrideOArguments(state, pc, arity, arg1);
  adjustAG(state, pc, tgtOff);
}

blockPo blockBreak(blockPo block, insPo code, int32 tgt, OpCode blockType) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(code[block->startPc].op == blockType);
      return block;
    }
    block = block->parent;
  }
  return Null;
}

retCode handleBreakTable(codeGenPo state, int32 pc, blockPo block, insPo code, int32 count) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  for (int ix = 0; ix < count; ix++, pc++) {
    check(code[pc].op==Break||code[pc].op==Loop, "Expecting a Break instruction");
    blockPo tgtBlock = blockBreak(block, code, pc + code[pc].alt + 1, Block);
    codeLblPo lbl = (code[pc].op == Break ? breakLabel(tgtBlock) : loopLabel(tgtBlock));
    b(lbl);
  }
  return Ok;
}

void testResult(codeGenPo state, int32 pc, blockPo tgtBlock) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo skip = newLabel(ctx);
  cmp(RTS, IM(Normal));
  beq(skip);

  blockPo parent = tgtBlock->parent;
  localVarPo phiVar = parent->phiVar;
  assert(phiVar!=Null);

  FlexOp er = popFlex(state, pc);
  storeVar(state, pc, er, phiVar);

  breakOut(ctx, tgtBlock);
  bind(skip);
}

void populateLocals(codeGenPo state) {
  int32 arity = mtdArity(state->mtd);
  for (int32 ix = 0; ix < state->numLocals; ix++)
    state->locals[ix].live = False;

  registerMap argRegs = defaultArgRegs();

  for (int32 ax = 0; ax < arity; ax++) {
    varDescPo var = findVar(state->analysis, ax);
    localVarPo slot = argSlot(state, ax);
    armReg rg = nxtAvailReg(argRegs);
    if (!sameFlexOp(slot->src,RG(XZR))) {
      slot->src = RG(rg);
      slot->stashed = False;
      argRegs = dropReg(argRegs, rg);
      reserveReg(state->jit, rg);
    } else {
      slot->stashed = True;
      slot->src = OF(AG, ax*pointerSize);
    }
    slot->live = True;
    slot->stkOff = ax;
    slot->desc = var;
  }
#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    showRegisterMap(logFile, argRegs);
  }
#endif
}

void loadRegister(codeGenPo state, FlexOp src, armReg rg) {
  assemCtxPo ctx = state->jit->assemCtx;
  if (isRegisterOp(src))
    mov(rg, src);
  else
    ldr(rg, src);
}

void dumpState(codeGenPo state) {
  showLiveLocals(logFile, state);
  dRegisterMap(state->jit->freeRegs);
  for (int32 ix = 0; ix < state->top; ix++) {
    outMsg(logFile, "SP[%d] = %X\n%_", ix, state->stack[ix]);
  }
}

static void argMove(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap *freeRegs) {
  move(ctx, dst, src, *freeRegs);
}

void invokeIntrinsic(codeGenPo state, int32 pc, runtimeFn fn, int32 arity, FlexOp args[]) {
  assemCtxPo ctx = assemCtx(state->jit);
  if (traceJit >= detailedTracing) {
    showLiveLocals(logFile, state);
  }
  int32 stackBase = stashLiveLocals(state, pc, MAX_INT32);
  voidOutFrameLocals(state, pc, stackBase); // void out gaps in the locals map

  ArgSpec operands[arity];

  registerMap argRegs = systemArgRegs();
  registerMap saveMap = criticalRegs();

  for (int32 ix = 0; ix < arity; ix++) {
    armReg ax = nxtAvailReg(argRegs);
    argRegs = dropReg(argRegs, ax);
    assert(ax!=XZR);
    operands[ix] = (ArgSpec){.src = args[ix], .dst = RG(ax), .mark = True, .group = -1};
  }
  registerMap tmpMap = fixedRegSet(X16);

  shuffleVars(ctx, operands, arity, &tmpMap, argMove);

  stashEngineState(state->jit, -stackBase);
  saveRegisters(ctx, saveMap);
  mov(X16, IM((integer) fn));
  blr(X16);
  restoreRegisters(ctx, saveMap);
  unstashEngineState(state->jit);
  if (traceJit >= detailedTracing) {
    showLiveLocals(logFile, state);
  }
}

void invokeEscape(codeGenPo state, int32 pc, runtimeFn fn, int32 arity) {
  assemCtxPo ctx = assemCtx(state->jit);
  loadArguments(state, pc, arity);
  registerMap saveMap = criticalRegs();
  saveRegisters(ctx, saveMap);
  mov(X16, IM((integer) fn));
  blr(X16);
  restoreRegisters(ctx, saveMap);
  dropArguments(state, arity, pc);
}


static void stashLocal(codeGenPo state, int32 pc, localVarPo v) {
  if (liveVar(v, pc) && isRegisterOp(v->src)) {
    v->stkOff = minLiveOffset(state,pc)-1;
    FlexOp lclFlex = localFlex(state, v->stkOff);
    storeFlex(state, pc, v->src, lclFlex);
    v->src = lclFlex;
  }
}

 int32 minLiveOffset(codeGenPo state, int32 pc) {
  int32 minOffset = 0;
  for (int32 ix = 0; ix < state->numLocals ; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc)) {
      if (!isRegisterOp(var->src)) {
        if (var->stkOff < minOffset)
          minOffset = var->stkOff;
      }
    }
  }
  return minOffset;
}

int32 stashLiveLocals(codeGenPo state, int32 pc, int32 cnt) {
  int32 minOffset = 0;
  for (int32 ix = 0; ix < state->numLocals && cnt > 0; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc)) {
      if (isRegisterOp(var->src)) {
        stashLocal(state, pc, var);
        minOffset = min(minOffset,var->stkOff);
      }
    }
  }
  return minOffset;
}

static void voidOutSlot(codeGenPo state, int32 pc, int32 offset,armReg vdCon, logical *loaded) {
   for (int32 vx = 0; vx < state->numLocals; vx++) {
    localVarPo var = &state->locals[vx];
    if (liveVar(var, pc) && var->stkOff == offset)
      return;
  }
  if (!*loaded) {
    loadConstant(state->jit, voidIndex, vdCon);
    *loaded = True;
  }
  storeFlex(state, pc, RG(vdCon),OF(AG, offset*pointerSize));
}

void voidOutFrameLocals(codeGenPo state, int32 pc, int32 minOffset) {
  armReg vdCon = findARegister(state, pc);
  logical loaded = False;
  for (int32 ix = -1; ix > minOffset; ix--) {
    voidOutSlot(state, pc, ix,vdCon,&loaded);
  }
  releaseReg(state->jit,vdCon);
}

int32 loadArguments(codeGenPo state, int32 pc, int32 arity) {
  ArgSpec operands[arity];
  registerMap argRegs = defaultArgRegs();

  assert(state->top >= arity);
  int32 minOffset = stashLiveLocals(state, pc + 1, MAX_INT32); // save vars that will be live after the call
  voidOutFrameLocals(state, pc, minOffset); // void out gaps in the locals map

  for (int32 ix = 0; ix < arity; ix++) {
    localVarPo var = state->stack[ix];
    armReg ax = nxtAvailReg(argRegs);
    int32 argSLot = minOffset - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = (ArgSpec){.src = var->src, .dst = RG(ax), .mark = True, .group = -1};
    } else {
      operands[ix] = (ArgSpec){.src = var->src, .dst = OF(AG, argSLot*pointerSize), .mark = True, .group = -1};
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity, &tmpMap, argMove);
  return minOffset - arity;
}

int32 overrideArguments(codeGenPo state, int32 pc, int32 arity) {
  ArgSpec operands[arity];
  registerMap argRegs = defaultArgRegs();

  assert(state->top >= arity);

  int32 tgtOff = argCount(state->jit->mtd);

  for (int32 ix = 0; ix < arity; ix++) {
    localVarPo var = state->stack[ix];
    armReg ax = nxtAvailReg(argRegs);
    int32 argSLot = tgtOff - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = (ArgSpec){.src = var->src, .dst = RG(ax), .mark = True, .group = -1};
    } else {
      operands[ix] = (ArgSpec){.src = var->src, .dst = OF(AG, argSLot*pointerSize), .mark = True, .group = -1};
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity, &tmpMap, argMove);
  return tgtOff - arity;
}

int32 overrideOArguments(codeGenPo state, int32 pc, int32 arity, armReg arg1) {
  ArgSpec operands[arity + 1];
  registerMap argRegs = defaultArgRegs();

  assert(state->top >= arity);

  int32 tgtOff = argCount(state->jit->mtd);
  operands[0] = (ArgSpec){.src = RG(arg1), .dst = RG(X0), .mark = True, .group = -1};

  for (int32 ix = 0; ix < arity; ix++) {
    localVarPo var = state->stack[ix];
    armReg ax = nxtAvailReg(argRegs);
    int32 argSLot = tgtOff - arity + ix + 1;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix + 1] = argSpec(var->src, RG(ax));
    } else {
      operands[ix + 1] = argSpec(var->src, OF(AG, argSLot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity, &tmpMap, argMove);
  return tgtOff - arity;
}

// Special version for handling indirect calls

int32 loadOArguments(codeGenPo state, int32 pc, int32 arity, armReg frReg) {
  ArgSpec operands[arity + 1];

  assert(state->top >= arity);
  int32 minOffset = stashLiveLocals(state, pc, MAX_INT32);
  voidOutFrameLocals(state, pc, minOffset);

  operands[0] = (ArgSpec){.src = RG(frReg), .dst = RG(X0), .mark = True, .group = -1};

  registerMap argRegs = dropReg(defaultArgRegs(), frReg);

  for (int32 ix = 0; ix < arity; ix++) {
    localVarPo var = state->stack[ix];
    armReg ax = nxtAvailReg(argRegs);
    int32 argSLot = minOffset - arity + ix + 1;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix + 1] = (ArgSpec){.src = var->src, .dst = RG(ax), .mark = True, .group = -1};
    } else {
      operands[ix + 1] = (ArgSpec){.src = var->src, .dst = OF(AG, argSLot*pointerSize), .mark = True, .group = -1};
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity, &tmpMap, argMove);
  return minOffset - arity;
}

void adjustAG(codeGenPo state, int32 pc, int32 tgtOff) {
  int32 delta = tgtOff * pointerSize;
  assemCtxPo ctx = assemCtx(state->jit);
  if (delta > 0) {
    if (is12bit(delta))
      add(AG, AG, IM(delta));
    else {
      armReg tmp = findARegister(state, pc);
      mov(tmp, IM(delta));
      add(AG, AG, RG(tmp));
      releaseReg(state->jit, tmp);
    }
  } else {
    delta = -delta;
    if (is12bit(delta))
      sub(AG, AG, IM(delta));
    else {
      armReg tmp = findARegister(state, pc);
      mov(tmp, IM(delta));
      sub(AG, AG, RG(tmp));
      releaseReg(state->jit, tmp);
    }
  }
}

void dropArguments(codeGenPo state, int32 arity, int32 pc) {
  state->top -= arity;
}

localVarPo findPhiVariable(codeGenPo state, int32 pc) {
  localVarPo var = markVarStart(state, pc, RG(XZR));

  FlexOp src = getLclSrc(state, pc, var->desc->varNo);
  var->src = src;

  return var;
}

void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var) {
  if (var->desc->registerCandidate) {
    if (isRegisterOp(var->src) && var->src.reg == XZR) {
      armReg rg = findARegister(state, pc);
      var->src = RG(rg);
    }
  }
  storeFlex(state, pc, val, var->src);
}

retCode showLocalVar(ioPo out, void *data, long depth, long precision, logical alt) {
  localVarPo var = (localVarPo) data;
  return outMsg(out, "%s[%d] @ %F", varKindName(var->desc->kind), var->desc->varNo, &var->src);
}

localVarPo argSlot(codeGenPo state, int32 ax) {
  assert(ax>=0 && ax<mtdArity(state->mtd));
  return &state->locals[state->argPt + ax];
}

int32 nextStkOff(codeGenPo state, int32 pc) {
  int32 minSlot = 0;

  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (liveVar(lcl, pc)) {
      if (lcl->stkOff < minSlot)
        minSlot = lcl->stkOff;
    }
  }

  return minSlot - 1;
}

localVarPo allocateLocal(codeGenPo state, int32 pc) {
  for (int32 lx = 0; lx < state->numLocals; lx++) {
    localVarPo l = &state->locals[lx];
    if (!liveVar(l, pc))
      return l;
  }
  return Null;
}

static logical liveVar(localVarPo var, int32 pc) {
  return var->live && var->desc->end > pc;
}

FlexOp getLclSrc(codeGenPo state, int32 pc, int32 lclNo) {
  localVarPo lcl = findLocal(state, pc, lclNo);
  assert(lcl!=Null && liveVar(lcl,pc));
  return lcl->src;
}

localVarPo markVarStart(codeGenPo state, int32 pc, FlexOp src) {
  varDescPo stkVar = varStart(state->analysis, pc);
  assert(stkVar!=Null);
  localVarPo lcl = allocateLocal(state, pc);
  assert(lcl!=Null);
  lcl->stkOff = src.immediate/pointerSize;
  lcl->live = True;
  lcl->src = src;
  lcl->desc = stkVar;
  lcl->stashed = !isRegisterOp(src);
  return lcl;
}

registerMap defaultArgRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7;
}

registerMap systemArgRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7;
}

void showLiveLocals(ioPo out, codeGenPo state) {
  outMsg(out, "Live locals:\n");
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->live)
      outMsg(out, "%X\n", lcl);
  }
}

static localVarPo findLocal(codeGenPo state, int32 pc, int32 lx) {
  varDescPo varDesc = findVar(state->analysis, lx);
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->live && lcl->desc == varDesc)
      return lcl;
  }
  localVarPo lcl = allocateLocal(state, pc);

  lcl->live = True;
  lcl->desc = varDesc;
  lcl->stkOff = (varDesc->kind != local ? nextStkOff(state, pc) : varDesc->slot);
  lcl->stashed = False;
  lcl->src = (varDesc->registerCandidate ? RG(XZR) : localFlex(state, lcl->stkOff));
  return lcl;
}

static logical registerInUse(codeGenPo state, FlexOp src) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    if (state->locals[ix].live && sameFlexOp(state->locals[ix].src, src)) {
      return True;
    }
  }
  return False;
}

void retireExpiredVars(codeGenPo state, int32 pc) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->live) {
      varDescPo desc = lcl->desc;
      if (desc->end <= pc) {
#ifdef TRACEJIT
        if (traceJit >= detailedTracing) {
          outMsg(logFile, "Retire variable %X at %d\n", lcl, pc);
        }
#endif
        lcl->live = False;
        if (isRegisterOp(lcl->src) && !registerInUse(state, lcl->src)) {
          releaseReg(state->jit, lcl->src.reg);
          lcl->src = RG(XZR);
        }
      }
    }
  }
}

void pushVar(codeGenPo state, int32 pc, localVarPo v) {
  assert(v!=Null && state->top<state->argPt);
  state->stack[state->top++] = v;
}

localVarPo popVar(codeGenPo state, int32 pc) {
  assert(state->top>0);
  return state->stack[--state->top];
}

FlexOp popFlex(codeGenPo state, int32 pc) {
  assert(state->top>0);
  localVarPo lcl = state->stack[--state->top];
  return lcl->src;
}

FlexOp topFlex(codeGenPo state, int32 pc) {
  assert(state->top>0);
  localVarPo lcl = state->stack[state->top - 1];
  return lcl->src;
}

static void storeFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt) {
  assemCtxPo ctx = assemCtx(state->jit);
  if (isRegisterOp(tgt)) {
    loadRegister(state, src, tgt.reg);
  } else if (isRegisterOp(src)) {
    str(src.reg, tgt);
  } else {
    armReg tmp = findFreeReg(state->jit);
    loadRegister(state, src, tmp);
    str(tmp, tgt);
    releaseReg(state->jit, tmp);
  }
}

void loadFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt) {
  assemCtxPo ctx = assemCtx(state->jit);
  if (isRegisterOp(tgt))
    loadRegister(state, src, tgt.reg);
  else if (isRegisterOp(src))
    move(ctx, tgt, src, state->jit->freeRegs);
  else {
    armReg tmp = findFreeReg(state->jit);
    loadRegister(state, src, tmp);
    move(ctx, tgt,RG(tmp), state->jit->freeRegs);
    releaseReg(state->jit, tmp);
  }
}

FlexOp constantFlex(codeGenPo state, int32 index) {
  return OF(CO, index*pointerSize);
}

FlexOp localFlex(codeGenPo state, int32 index) {
  return OF(AG, index*pointerSize);
}

armReg findARegister(codeGenPo state, int32 pc) {
  armReg tmp = findFreeReg(state->jit);
  if (tmp == XZR) {
    if (stashLiveLocals(state, pc, 1) < 1) // We just stash one of them...
      syserr("Not enough free registers");

    tmp = findFreeReg(state->jit);
    assert(tmp!=XZR);
  }
  return tmp;
}

void verifyState(codeGenPo state, int32 pc) {
  assert(state->top>=0);
  registerMap freeRegs = state->jit->freeRegs;
  for (int32 ix = 0; ix < state->top; ix++) {
    localVarPo v = state->stack[ix];
    assert(v->live);
    assert(v->desc->end >= pc);
    assert(isRegisterOp(v->src) ? !isRegInMap(freeRegs,v->src.reg):v->src.immediate==v->stkOff*pointerSize);
  }
}

void brkOut(assemCtxPo ctx, blockPo tgtBlock) {
  codeLblPo lbl = breakLabel(tgtBlock);
  b(lbl);
}

void brkOutNe(assemCtxPo ctx, blockPo tgtBlock) {
  codeLblPo lbl = breakLabel(tgtBlock);
  bne(lbl);
}
