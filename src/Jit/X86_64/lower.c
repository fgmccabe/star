//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "engineOptions.h"
#include "analyseP.h"
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
#include "abort.h"
#include "arithmetic.h"
#include "arithP.h"
#include "debugP.h"
#include "errorCodes.h"
#include "formioP.h"
#include "shuffle.h"
#include "sort.h"
#include "disass.h"
#include "labels.h"
#include "normalP.h"

/* Lower Star VM code to X86_64 code */
/*
 * RAX = RTS = return status
 * RDX = RTV = return value / X2 = argument 3
 * RDI = X0 = argument 1
 * RSI = X1 = argument 2
 * RCX = X3 = argument 4
 * R8  = X4 = argument 5
 * R9  = X5 = argument 6
 * R10 = X16 = intra-procedure call scratch register / lambda register
 * R11 = LR = link register (holds return address for calls)
 * R12 = CO = constants vector pointer
 * R13 = AG = args pointer (points to arguments on stack frame)
 * R14 = STK = current stack structure pointer
 * R15 = PR = current process structure pointer
 * RBP = FP = frame pointer
 * RSP = SP = system stack pointer (only used when entering C calls)
 * RBX = unused callee-saved scratch register
 *
 * Float operand registers:
 * F0-F15 = XMM0-XMM15
 */

static retCode jitBlock(blockPo block, codeGenPo state, ssaInsPo code, int32 from, int32 endPc);

static void pushFrme(codeGenPo state, int32 pc, int32 argOffset);
static void allocSmallStruct(codeGenPo state, int32 pc, int32 livePc, int32 index, integer amnt);
static void allocUnary(codeGenPo state, int32 pc, int32 livePc, int32 index, localVarPo arg);
static void allocBinary(codeGenPo state, int32 pc, int32 livePc, int32 index, localVarPo left, localVarPo right);
static mcRegister allocCallArgVector(codeGenPo state, int32 argPc, int32 livePc);
static retCode handleBreakTable(codeGenPo state, ssaInsPo code, blockPo block, int32 pc, int32 limit);
static void mkFloat(codeGenPo state, int32 pc, int32 livePc, fpReg dx);
static void populateLocals(codeGenPo state, int32 arity, registerMap registerArgs);
static int32 operand(codeGenPo state, int32 pc, int32 ox);
static FlexOp localFlex(codeGenPo state, int32 pc, int32 vrNo);
static FlexOp sourceOperandFlex(codeGenPo state, int32 pc, int32 ax);
static localVarPo operandVar(codeGenPo state, int32 pc, int32 ax);
static int32 loadArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity);
static int32 loadLambdaArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity);
static int32 loadEscapeArguments(codeGenPo state, int32 pc, int32 livePc, int32 arity, int32 argBase);
static void dropArguments(codeGenPo state, int32 pc);
static int32 overrideArguments(codeGenPo state, registerMap argRegs, int32 argPc, int32 arity);
static void adjustAG(codeGenPo state, int32 pc, int32 tgtOff);
static localVarPo findPhiVariable(codeGenPo state, int32 pc, int32 vrNo);
static void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var);
static FlexOp varSrc(codeGenPo state, int32 pc, localVarPo var);

static void retireExpiredVars(codeGenPo state, int32 pc);
static logical registerInUse(codeGenPo state, FlexOp src);

#define opand(ox) operand(state, pc, (ox))

void debug_state(codeGenPo state) {
  volatile int dummy = 0;
}

retCode jitInstructions(jitCompPo jit, methodPo mtd, registerMap argRegisters, char* errMsg, integer msgLen) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method", mtd);
  }
#endif

  AnalysisRecord analysis;

  retCode ret;
  logical failed = False;
  if ((ret = analyseMethod(mtd, &analysis)) == Ok) {
    int32 numSlots = slotCount(&analysis);
    int32 arity = mtdArity(mtd);
    LocalVar locals[numSlots];
    logical voided[numSlots];

    CodeGenState state = {
      .mtd = mtd, .code = entryPoint(mtd), .analysis = &analysis, .locals = locals, .numLocals = numSlots,
      .jit = jit, .voided = voided, .argMark = numSlots - arity
    };

    populateLocals(&state, arity, argRegisters);

    JitBlock block = {
      .startPc = 0, .endPc = codeSize(mtd),
      .breakLbl = Null, .loopLbl = Null,
      .parent = Null, .phiCnt = 0, .phiVars = Null
    };

    ret = jitBlock(&block, &state, entryPoint(mtd), 0, codeSize(mtd));
    failed = state.jit->failed;
    debug_state(&state);
  }


  tearDownAnalysis(&analysis);
  return (ret == Ok && failed) ? Error : ret;
}

retCode jitBlock(blockPo block, codeGenPo state, ssaInsPo code, int32 from, int32 endPc) {
  retCode ret = Ok;
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);

#ifdef TRACEJIT
  if (traceJit >= generalTracing) {
    outMsg(logFile, "Jit block %d -> %d\n%_", from, endPc);
  }
#endif
  for (int32 pc = from; ret == Ok && !state->jit->failed && pc < endPc;) {
    retireExpiredVars(state, pc);
    verifyState(state, pc);
#ifdef TRACEJIT
    if (traceJit >= detailedTracing) {
      dumpState(state, pc);
    }
    if (traceJit >= generalTracing) {
      showIns(logFile, state->mtd, Null, &code[pc]);
      outMsg(logFile, "\n%_");
    }
#endif
    switch (code[pc].op.op) {
    case sHalt: {
      // Stop execution
      int32 insSize = 2;
      FlexOp src = localFlex(state, pc, opand(1));
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)star_exit, 1, (FlexOp[]){src}, False, 0,
                      Null);
      pc += insSize;
      continue;
    }
    case sAbort: {
      // abort with message
      int32 insSize = 3;
      mcRegister loc = findMcRegister(state, pc);
      adr(loc, here());
      str(loc, OF(STK,OffsetOf(StackRecord,pc)));
      FlexOp val = sourceOperandFlex(state, pc, 2);
      loadConstant(jit, opand(1), loc);
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)abort_star, 3, (FlexOp[]){RG(PR), RG(loc), val},
                      False, 0, Null);
      releaseReg(jit, loc);
      pc += insSize;
      continue;
    }
    case sCall: {
      int32 insSize = opand(2) + 3;
      int32 key = opand(1);
      int32 arity = lblArity(C_LBL(getConstant(key)));
      int32 nextPc = pc + insSize;

      labelPo tgt = C_LBL(getConstant(key));
#ifdef TRACEJIT
      if (traceJit >= detailedTracing) {
        methodPo debug_callee = labelMtd(tgt);
        outMsg(logFile, "JIT Call pc %d target: %s (", pc, tgt->lbl.name);
        for (int i = 0; tgt->lbl.name[i] != 0; i++) {
          outMsg(logFile, "%02x ", (unsigned char)tgt->lbl.name[i]);
        }
        outMsg(logFile, "), mtd: 0x%lx, jit: 0x%lx\n",
               (long)debug_callee, debug_callee != Null ? (long)debug_callee->jit.code : 0L);
      }
#endif
      methodPo callee = labelMtd(tgt);

      int32 argPnt = loadArguments(state, nextPc, pc + 3, arity);

      if (callee != Null && hasJitCode(callee)) {
        jittedCode jitted = jitCode(callee);
#ifdef TRACEJIT
        if (traceJit >= detailedTracing) {
          outMsg(logFile, "JIT Call pc %d EMITTING jit: 0x%lx\n", pc, (long)jitted);
        }
#endif
        mov(RG(R10), IM((uinteger)jitted));
      }
      else {
        loadConstant(jit, key, R10);

        // pick up the pointer to the method
        mov(RG(R10), BS(R10, OffsetOf(LblRecord, mtd)));
        codeLblPo noMtd = newLabel(ctx);
        test(RG(R10), RG(R10));
        je(noMtd);
        // Pick up the jit code itself
        mov(RG(R10), BS(R10, OffsetOf(MethodRec, jit.code)));
        codeLblPo runMtd = newLabel(ctx);
        test(RG(R10), RG(R10));
        jne(runMtd);

        bind(noMtd);
        bailOut(state, pc, undefinedCode);

        bind(runMtd);
      }

      pushFrme(state, pc, argPnt);

      codeLblPo rtn = newLabel(ctx);
      lea(RG(LR), LB(rtn)); // Use LR (R11) symbolically
      jmp(RG(R10));
      bind(rtn);
      dropArguments(state, nextPc);
      pc = nextPc;
      continue;
    }
    case sOCall: {
      int32 numArgs = opand(2);
      int32 insSize = numArgs + 3;
      int32 nextPc = pc + insSize;
      FlexOp lam = sourceOperandFlex(state, pc, 1); // Pick up the closure
      mcRegister lamReg = X16;
      loadRegister(state, lamReg, lam);
      int32 argPnt = loadLambdaArguments(state, nextPc, pc + 3, numArgs) - 1;
      ldr(X0, OF(lamReg, OffsetOf(ClosureRecord, free)));
      ldr(lamReg, OF(lamReg, OffsetOf(ClosureRecord, lbl))); // Pick up the label
      // pick up the pointer to the method
      ldr(lamReg, OF(lamReg, OffsetOf(LblRecord, mtd)));
      codeLblPo haveMtd = newLabel(ctx);
      cbnz(lamReg, haveMtd);

      bailOut(state, pc, undefinedCode);

      bind(haveMtd);
      pushFrme(state, pc, argPnt);

      // Pick up the jit code itself
      ldr(X16, OF(lamReg, OffsetOf(MethodRec, jit.code)));
      blr(X16);
      dropArguments(state, pc + insSize);
      pc = nextPc;
      continue;
    }
    case sTCall: {
      int32 insSize = opand(2) + 3;
      int32 nextPc = pc + insSize;
      int32 key = opand(1);
      labelPo tgt = C_LBL(getConstant(key));
#ifdef TRACEJIT
      if (traceJit >= detailedTracing) {
        methodPo debug_callee = labelMtd(tgt);
        outMsg(logFile, "JIT TCall pc %d target: %s, mtd: 0x%lx, jit: 0x%lx\n",
               pc, tgt->lbl.name, (long)debug_callee, debug_callee != Null ? (long)debug_callee->jit.code : 0L);
      }
#endif
      int32 arity = lblArity(tgt);
      methodPo callee = labelMtd(tgt);

      int32 argPc = pc + 3;
      int32 tgtOff = overrideArguments(state, defaultArgRegs(), argPc, arity);

      if (callee != Null && hasJitCode(callee)) {
        jittedCode jitted = jitCode(callee);
#ifdef TRACEJIT
        if (traceJit >= detailedTracing) {
          outMsg(logFile, "JIT TCall pc %d EMITTING jit: 0x%lx\n", pc, (long)jitted);
        }
#endif
        mov(RG(R10), IM((uinteger)jitted));
      }
      else {
        loadConstant(jit, key, R10);

        // pick up the pointer to the method
        mov(RG(R10), BS(R10, OffsetOf(LblRecord, mtd)));
        codeLblPo noMtd = newLabel(ctx);
        test(RG(R10), RG(R10));
        je(noMtd);
        // Pick up the jit code itself
        mov(RG(R10), BS(R10, OffsetOf(MethodRec, jit.code)));
        codeLblPo runMtd = newLabel(ctx);
        test(RG(R10), RG(R10));
        jne(runMtd);

        bind(noMtd);
        bailOut(state, pc, undefinedCode);

        bind(runMtd);
      }
      adjustAG(state, pc, tgtOff);
      mov(BS(STK, OffsetOf(StackRecord, args)), RG(AG));

      // Pick up the old return address
      mov(RG(LR), BS(FP, OffsetOf(StackFrame, link)));
      jmp(RG(R10));
      pc = nextPc;
      continue;
    }
    case sTOCall: {
      int32 insSize = opand(2) + 3;
      int32 numArgs = opand(2);
      int32 argPc = pc + 3;
      int32 nextPc = pc + insSize;
      mcRegister lamReg = X16;
      FlexOp lam = sourceOperandFlex(state, pc, 1); // Pick up the closure
      loadRegister(state, lamReg, lam);
      int32 tgtOff = overrideArguments(state, lambdaArgRegs(), argPc, numArgs) - 1;

      ldr(X0, OF(lamReg, OffsetOf(ClosureRecord, free)));
      ldr(lamReg, OF(lamReg, OffsetOf(ClosureRecord, lbl))); // Pick up the label
      // pick up the pointer to the method
      ldr(lamReg, OF(lamReg, OffsetOf(LblRecord, mtd)));
      codeLblPo haveMtd = newLabel(ctx);
      codeLblPo noMtd = newLabel(ctx);
      cbz(lamReg, noMtd);
      ldr(X16, OF(lamReg, OffsetOf(MethodRec, jit.code)));
      cbnz(X16, haveMtd);
      bind(noMtd);
      bailOut(state, pc, undefinedCode);

      bind(haveMtd);
      adjustAG(state, pc, tgtOff);
      str(AG, OF(STK, OffsetOf(StackRecord,args)));
      // Pick up the old return address
      ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
      br(X16);
      pc = nextPc;
      continue;
    }
    case sEscape: {
      int32 insSize = opand(2) + 3;
      int32 nextPc = pc + insSize;
      int32 escNo = opand(1);
      escapePo esc = getEscape(escNo);
      int32 arity = escapeArity(esc);
      assert(arity==opand(2));

      int32 tgtOff = loadEscapeArguments(state, pc, nextPc, arity, pc + 3);
      stashEngineState(state->jit, tgtOff, fixedRegSet(X16));
      adjustAG(state, pc, tgtOff);
      registerMap saveMap = criticalRegs();
      saveRegisters(ctx, saveMap);
      mov(RG(X16), IM((integer) escapeCode(esc)));
      call(RG(X16));
      restoreRegisters(ctx, saveMap);
      unstashEngineState(state->jit);
      dropArguments(state, pc + insSize);

      pc = nextPc;
      continue;
    }
    case sEntry: {
      int32 nextPc = pc + 3;
      mov(BS(FP, OffsetOf(StackFrame, link)), RG(LR)); // Use FP and LR symbolically
      flushArguments(state, nextPc);
      stackCheck(state, pc, opand(1), opand(2));
      pc = nextPc;
      continue;
    }
    case sRSP: {
      int32 insSize = 2;
      codeLblPo rsltOk = newLabel(ctx);
      cbz_w(RTS, rsltOk);

      bailOut(state, pc, unhandledExceptionCode);

      bind(rsltOk);
      localVarPo tgt = localTarget(state, pc, opand(1));
      storeVar(state, pc,RG(RTV), tgt);

      if (!registerInUse(state, RG(RTV))) {
        if (!isRegInMap(jit->freeRegs, RTV))
          releaseReg(jit,RTV);
      }
      if (!registerInUse(state, RG(RTS))) {
        if (!isRegInMap(jit->freeRegs, RTS))
          releaseReg(jit,RTS);
      }
      pc += insSize;
      continue;
    }
    case sRSX: {
      int32 insSize = 3;
      codeLblPo rsltOk = newLabel(ctx);
      cbz_w(RTS, rsltOk);
      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock!=Null && tgtBlock->phiCnt==1);

      storeVar(state, pc,RG(RTV), tgtBlock->phiVars[0]);
      b(breakLabel(tgtBlock));
      bind(rsltOk);

      localVarPo tgt = localTarget(state, pc, opand(2));
      storeVar(state, pc,RG(RTV), tgt);
      if (!registerInUse(state, RG(RTV))) {
        if (!isRegInMap(jit->freeRegs, RTV))
          releaseReg(jit,RTV);
      }
      if (!registerInUse(state, RG(RTS))) {
        if (!isRegInMap(jit->freeRegs, RTS))
          releaseReg(jit,RTS);
      }

      pc += insSize;
      continue;
    }
    case sRet: {
      int32 insSize = 2;
      FlexOp vl = sourceOperandFlex(state, pc, 1); // Pick up the result variable
      loadRegister(state, RTV, vl);

      // Adjust args register
      ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
      // Pick up return address
      ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
      // Drop frame
      sub(RG(FP), IM(sizeof(StackFrame)));
      mov(RG(RTS), IM(Normal));
      br(X16);

      pc += insSize;
      continue;
    }
    case sXRet: {
      int32 insSize = 2;
      FlexOp vl = sourceOperandFlex(state, pc, 1); // Pick up the result variable
      loadRegister(state, RTV, vl);

      // Adjust args register
      ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
      // Pick up return address
      ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
      // Drop frame
      sub(RG(FP), IM(sizeof(StackFrame)));
      mov(RG(RTS), IM(Abnormal));
      br(X16);
      pc += insSize;
      continue;
    }
    case sRtn: {
      int32 insSize = 1;

      // Adjust args register (restore caller's AG)
      mov(RG(AG), BS(FP, OffsetOf(StackFrame, args)));
      // Pick up return address
      mov(RG(R10), BS(FP, OffsetOf(StackFrame, link)));
      // Drop frame
      sub(RG(FP), IM(sizeof(StackFrame)));
      loadRegister(state, RTV, constantFlex(voidIndex));
      mov(RG(RTS), IM(Normal));
      jmp(RG(R10));
      pc += insSize;
      continue;
    }
    case sBlock: {
      // vlof block of instructions
      int32 arity = opand(1);
      int32 blockLen = opand(arity+2);
      int32 nextPc = pc + blockLen;
      codeLblPo brkLbl = newLabel(ctx);
      localVarPo phiVars[arity];

      for (int32 ax = 0; ax < arity; ax++)
        phiVars[ax] = findPhiVariable(state, pc,opand(ax+2));

      JitBlock subBlock = {
        .blockType = sBlock,
        .startPc = pc,
        .endPc = nextPc,
        .breakLbl = brkLbl,
        .loopLbl = here(),
        .parent = block,
        .phiCnt = arity,
        .phiVars = phiVars
      };

      ret = jitBlock(&subBlock, state, code, pc + arity + 3, nextPc);
      pc = nextPc; // Skip over the block
      bind(brkLbl);
      continue;
    }
    case sBreak: {
      int32 insSize = 2;
      int32 tgt = pc + opand(1);
      blockPo tgtBlock = targetBlock(block, tgt, sBlock);
      breakOut(state, pc + insSize, tgtBlock);
      pc += insSize;
      continue;
    }
    case sResult: {
      // return value out of block
      int32 arity = opand(2);
      int32 insSize = arity + 3;
      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);

      assert(tgtBlock->phiCnt==arity);

      ArgSpec operands[arity];
      for (int32 ax = 0; ax < arity; ax++) {
        FlexOp src = localFlex(state, pc, opand(ax+3)); // result arg
        FlexOp dst = varSrc(state, pc, tgtBlock->phiVars[ax]);
        operands[ax] = argSpec(src, dst);
      }
      shuffleVars(state->jit, operands, arity, &jit->freeRegs);

      breakOut(state, pc + insSize, tgtBlock);
      pc += insSize;
      continue;
    }
    case sCont: {
      // jump back to start of block
      int32 insSize = 2;
      int32 tgt = pc + opand(1);
      blockPo tgtBlock = targetBlock(block, tgt, sBlock);
      codeLblPo loop = loopLabel(tgtBlock);
      assert(loop != Null);
      b(loop);
      pc += insSize;
      continue;
    }
    case sICase: {
      int32 insSize = 3;
      mcRegister ix = findMcRegister(state, pc);
      int32 skip = opand(2);
      localVarPo govVr = operandVar(state, pc, 1);
      assert(govVr->inUse);
      loadRegister(state, ix, govVr->src);
      getIntVal(jit, ix);
      mcRegister tmp = findMcRegister(state, pc);
      mov(RG(tmp), IM(LARGE_INT61));
      and(RG(ix), RG(tmp));
      releaseReg(jit, tmp);

      int32 mx = (skip - insSize) / 2;
      immModulo(ctx, ix, mx, jit->freeRegs);

      codeLblPo jmpTbl = newLabel(ctx);
      mcRegister off = findMcRegister(state, pc);
      adr(off, jmpTbl);
      lea(RG(off), IX(off, ix, 4, 0));
      add(RG(off), RG(ix));
      br(off);
      releaseReg(jit, off);
      releaseReg(jit, ix);
      bind(jmpTbl);
      return handleBreakTable(state, code, block, pc + 3, pc + skip);
    }
    case sCase: {
      // T --> T, case <Max>
      int32 skip = opand(2);
      int32 insSize = 3;
      localVarPo govVr = operandVar(state, pc, 1);
      assert(govVr->inUse);
      mcRegister ix = findMcRegister(state, pc);
      invokeIntrinsic(state, pc, pc, (runtimeFn)hashTerm, 1, (FlexOp[]){govVr->src}, True, 1,
                      (FlexOp[]){RG(ix)});
      int32 mx = (skip - insSize) / 2;
      immModulo(ctx, ix, mx, jit->freeRegs);
      codeLblPo jmpTbl = newLabel(ctx);
      mcRegister off = findMcRegister(state, pc);
      adr(off, jmpTbl);
      lea(RG(off), IX(off, ix, 4, 0));
      add(RG(off), RG(ix));
      br(off);
      releaseReg(jit, off);
      releaseReg(jit, ix);
      bind(jmpTbl);
      return handleBreakTable(state, code, block, pc + 3, pc + skip);
    }
    case sIxCase: {
      // check and jump on index
      int32 insSize = 3;
      mcRegister ix = findMcRegister(state, pc);
      int32 skip = opand(2);
      localVarPo govVr = operandVar(state, pc, 1);
      assert(govVr->inUse);
      loadRegister(state, ix, govVr->src);
      ldrw(ix, OF(ix, OffsetOf(TermHead,lblIndex))); // pick up the label index
      mcRegister labels = findMcRegister(state, pc);
      mov(RG(labels), IM((uinteger)labelConstructorIndex));
      ldrw(ix, IX(labels, ix, 4, 0));
      int32 mx = (skip - insSize) / 2;
      immModulo(ctx, ix, mx, jit->freeRegs);

      codeLblPo jmpTbl = newLabel(ctx);
      mcRegister off = findMcRegister(state, pc);
      adr(off, jmpTbl);
      lea(RG(off), IX(off, ix, 4, 0));
      add(RG(off), RG(ix));
      br(off);
      releaseReg(jit, off);
      releaseReg(jit, ix);
      bind(jmpTbl);
      return handleBreakTable(state, code, block, pc + 3, pc + skip);
    }
    case sCLbl: {
      // T,Lbl --> test for a data term, break if not lbl
      int32 insSize = 4;
      int32 key = opand(1);
      blockPo tgt = targetBlock(block, pc + opand(2), sBlock);
      mcRegister tmp = findMcRegister(state, pc);
      FlexOp vl = localFlex(state, pc, opand(3));
      loadRegister(state, tmp, vl);
      test(RG(tmp), IM(0b11));
      jne(breakLabel(tgt));

      ldrw(tmp, OF(tmp, OffsetOf(TermHead,lblIndex))); // pick up the class
      labelPo lit = C_LBL(getConstant(key));
      cmp_w(tmp, IM(lit->labelIndex));
      jne(breakLabel(tgt));
      releaseReg(jit, tmp);
      pc += insSize;
      continue;
    }
    case sCInt:
    case sCChar: {
      int32 insSize = 4;
      mcRegister tmp = findMcRegister(state, pc);
      FlexOp vl = localFlex(state, pc, opand(3));
      loadRegister(state, tmp, vl);
      int32 key = opand(1);

      integer lit = (integer)getConstant(key);
      if (isI32(lit))
        cmp(RG(tmp), IM(lit));
      else {
        mcRegister litReg = findFreeReg(jit);
        loadConstant(jit, key, litReg);
        cmp(RG(tmp), RG(litReg));
        releaseReg(jit, litReg);
      }
      releaseReg(jit, tmp);
      blockPo tgt = targetBlock(block, pc + opand(2), sBlock);
      jne(breakLabel(tgt));
      pc += insSize;
      continue;
    }
    case sCFlt:
    case sCLit: {
      // T,lit --> test for a literal value, break if not
      int32 insSize = 4;
      int32 key = opand(1);
      FlexOp vl = localFlex(state, pc, opand(3));
      invokeIntrinsic(state, pc, pc, (runtimeFn)sameTerm, 2, (FlexOp[]){vl, constantFlex(key)}, True,
                      1, (FlexOp[]){RG(RTV)});
      cmp_w(RTV, IM(True));
      blockPo tgt = targetBlock(block, pc + opand(2), sBlock);
      jne(breakLabel(tgt));
      pc += insSize;
      continue;
    }
    case sMC: {
      // Place a literal from constant pool
      int32 insSize = 3;
      int32 key = opand(2);
      localVarPo dst = localTarget(state, pc,opand(1));
      storeVar(state, pc, constantFlex(key), dst);
      pc += insSize;
      continue;
    }
    case sMv: {
      // Copy variables
      int32 insSize = 3;
      localVarPo src = localSource(state, pc,opand(2));
      localVarPo dst = localTarget(state, pc,opand(1));
      storeVar(state, pc, src->src, dst);
      pc += insSize;
      continue;
    }
    case sLG: {
      // load a global variable
      int32 insSize = 2;
      int32 nextPc = pc + insSize;
      int32 key = opand(1);
      mcRegister glb = findFreeReg(jit);
      globalPo glbVr = findGlobalVar(key);
      mov(RG(glb), IM((integer) glbVr));
      // Check if global is set
      ldr(RTV, OF(glb, OffsetOf(GlobalRecord, content)));
      releaseReg(jit, glb);
      codeLblPo haveContent = newLabel(ctx);
      codeLblPo callGetter = newLabel(ctx);
      cbz(RTV, callGetter);

      // If set, return value is in RTV. Set RTS to success (0)
      mov(RG(RTS), IM(0));
      b(haveContent);

      bind(callGetter);
      labelPo glbLbl = declareLbl(globalVarName(glbVr), 0, 0);
      if (glbLbl == Null)
        return jitError(jit, "no label definition for global %s", globalVarName(glbVr));

      int32 lblKey = defineConstantLiteral((termPo)glbLbl);
      loadConstant(jit, lblKey, X16);

      // pick up the pointer to the method
      ldr(X16, OF(X16, OffsetOf(LblRecord, mtd)));

      codeLblPo haveMtd = newLabel(ctx);
      cbnz(X16, haveMtd);

      bailOut(state, pc, undefinedCode);

      bind(haveMtd);

      int32 minOffset = stashLiveLocals(state, nextPc, True); // save vars that will be live after the call
      voidOutFrameLocals(state, nextPc, minOffset);           // void out gaps in the locals map
      pushFrme(state, pc, minOffset);

      // Pick up the jit code itself
      ldr(X16, OF(X16, OffsetOf(MethodRec, jit.code)));

      blr(X16);
      dropArguments(state, nextPc);
      bind(haveContent);
      pc = nextPc;
      continue;
    }
    case sSG: {
      int32 insSize = 3;
      globalPo glbVr = findGlobalVar(opand(1));
      mcRegister glbReg = findFreeReg(jit);
      mov(RG(glbReg), IM((integer) glbVr)); // Global var names are not subject to GC

      // store into a global variable
      localVarPo src = localSource(state, pc, opand(2));

      // Assign to the global var's content field
      storeFlex(state, pc, src->src, OF(glbReg, OffsetOf(GlobalRecord, content)));
      releaseReg(jit, glbReg);

      pc += insSize;
      continue;
    }
    case sSav: {
      // create a single assignment variable
      int32 insSize = 2;
      int32 nextPc = pc + insSize;

      mcRegister savReg = findMcRegister(state, pc);

      invokeIntrinsic(state, pc, nextPc, (runtimeFn)newSingleVar, 0, Null, False, 1, (FlexOp[]){RG(savReg)});

      localVarPo tgt = localTarget(state, pc, opand(1));
      storeVar(state, pc,RG(savReg), tgt);
      releaseReg(jit, savReg);
      pc = nextPc;
      continue;
    }
    case sLdSav: {
      int32 insSize = 4;
      // dereference a sav, break if not set
      FlexOp sng = localFlex(state, pc, opand(3));
      mcRegister tmp = findMcRegister(state, pc);
      loadFlex(state, pc, sng,RG(tmp));
      ldr(tmp, OF(tmp, OffsetOf(SingleRecord, content)));
      cbz(tmp, breakLabel(targetBlock(block, pc + opand(2), sBlock)));

      localVarPo tgt = localTarget(state, pc, opand(1));
      storeVar(state, pc,RG(tmp), tgt);
      releaseReg(jit, tmp);
      pc += insSize;
      continue;
    }
    case sTstSav: {
      // test a sav, return a logical
      int32 insSize = 3;
      FlexOp sng = localFlex(state, pc, opand(2));
      mcRegister tmp = findMcRegister(state, pc);
      loadFlex(state, pc, sng,RG(tmp));
      mcRegister tr = findMcRegister(state, pc);
      mcRegister fl = findMcRegister(state, pc);
      loadConstant(jit, falseIndex, fl);
      loadConstant(jit, trueIndex, tr);
      ldr(tmp, OF(tmp, OffsetOf(SingleRecord, content)));
      test(RG(tmp), RG(tmp));
      csel(tmp, tr, fl, NE);
      localVarPo tgt = localTarget(state, pc, opand(1));
      storeVar(state, pc, RG(tmp), tgt);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sStSav: {
      // store a value into a single assignment
      int32 insSize = 4;
      FlexOp sng = localFlex(state, pc, opand(2));
      writeBarrier(state, pc, sng);
      mcRegister sngReg = findMcRegister(state, pc);
      loadFlex(state, pc, sng,RG(sngReg));
      localVarPo val = localSource(state, pc, opand(3));
      if (isRegisterOp(val->src)) {
        storeFlex(state, pc, val->src,OF(sngReg, OffsetOf(SingleRecord, content)));
        storeVar(state, pc, val->src, localTarget(state, pc, opand(1)));
      }
      else {
        mcRegister vlReg = findMcRegister(state, pc);
        loadRegister(state, vlReg, val->src);
        storeFlex(state, pc, RG(vlReg), OF(sngReg, OffsetOf(SingleRecord, content)));
        localVarPo tgt = localTarget(state, pc, opand(1));
        storeVar(state, pc, RG(vlReg), tgt);
        releaseReg(jit, vlReg);
      }
      releaseReg(jit, sngReg);
      pc += insSize;
      continue;
    }
    case sCell: {
      // create R/W cell
      int32 insSize = 3;
      int32 nextPc = pc + insSize;

      mcRegister cell = findMcRegister(state, pc);

      invokeIntrinsic(state, pc, nextPc, (runtimeFn)newCell, 1, (FlexOp[]){
                        localFlex(state, pc, opand(2))
                      }, False, 1, (FlexOp[]){RG(cell)});

      localVarPo tgt = localTarget(state, pc, opand(1));
      storeVar(state, pc,RG(cell), tgt);
      releaseReg(jit, cell);

      pc = nextPc;
      continue;
    }
    case sGet: {
      // access a R/W cell
      int32 insSize = 3;
      FlexOp cel = localFlex(state, pc, opand(2));
      mcRegister vl = findMcRegister(state, pc);

      loadFlex(state, pc, cel,RG(vl));
      ldr(vl, OF(vl, OffsetOf(CellRecord, content)));
      localVarPo tgt = localTarget(state, pc, opand(1));
      storeVar(state, pc,RG(vl), tgt);
      releaseReg(jit, vl);
      pc += insSize;
      continue;
    }
    case sAssign: {
      // assign to a R/W cell
      int32 insSize = 3;
      FlexOp cel = localFlex(state, pc, opand(1));
      writeBarrier(state, pc, cel);
      FlexOp vl = localFlex(state, pc, opand(2));
      mcRegister tmp = findMcRegister(state, pc);
      mcRegister tmp2 = findMcRegister(state, pc);
      loadFlex(state, pc, cel,RG(tmp));
      loadFlex(state, pc, vl,RG(tmp2));

      str(tmp2, OF(tmp, OffsetOf(CellRecord, content)));

      releaseReg(jit, tmp);
      releaseReg(jit, tmp2);
      pc += insSize;
      continue;
    }
    case sNth: {
      // T --> el, pick up the nth element
      int32 insSize = 4;
      FlexOp trm = localFlex(state, pc, opand(3));
      mcRegister tmp = findMcRegister(state, pc);
      loadFlex(state, pc, trm,RG(tmp));
      loadElement(jit, tmp, tmp, opand(2) + 1);
      localVarPo dst = localTarget(state, pc, opand(1));
      storeVar(state, pc,RG(tmp), dst);
      releaseReg(jit, tmp);
      pc += insSize;
      continue;
    }
    case sStNth: {
      // T el --> store in nth element
      int32 insSize = 4;
      FlexOp trm = localFlex(state, pc, opand(1));
      writeBarrier(state, pc, trm);
      FlexOp vl = localFlex(state, pc, opand(3));
      mcRegister tmp = findMcRegister(state, pc);
      mcRegister tmp2 = findMcRegister(state, pc);
      loadRegister(state, tmp, trm);
      loadRegister(state, tmp2, vl);
      storeElement(jit, tmp2, tmp, opand(2) + 1);
      releaseReg(jit, tmp);
      releaseReg(jit, tmp2);
      pc += insSize;
      continue;
    }
    case sIAdd: {
      // L R --> L+R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      add(RG(a1), RG(a2));
      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);

      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sISub: {
      // L R --> L-R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      sub(RG(a1), RG(a2));

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sIMul: {
      // L R --> L*R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      imul(RG(a1), RG(a2));

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sIDiv: {
      // L R --> L/R
      int32 insSize = 5;
      FlexOp left = localFlex(state, pc, opand(3));
      FlexOp right = localFlex(state, pc, opand(4));

      registerMap savedFree = state->jit->freeRegs;
      state->jit->freeRegs = dropReg(state->jit->freeRegs, RAX);
      state->jit->freeRegs = dropReg(state->jit->freeRegs, RDX);

      mcRegister divisor = findMcRegister(state, pc);
      loadRegister(state, divisor, right);
      getIntVal(jit, divisor);

      codeLblPo skip = newLabel(ctx);
      cbnz(divisor, skip);

      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];

      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));
      bind(skip);

      mcRegister dividend = findMcRegister(state, pc);
      loadRegister(state, dividend, left);
      getIntVal(jit, dividend);

      logical rax_in_use = !isRegInMap(savedFree, RAX);
      logical rdx_in_use = !isRegInMap(savedFree, RDX);

      if (rax_in_use) push(RG(RAX));
      if (rdx_in_use) push(RG(RDX));

      mov(RG(RAX), RG(dividend));
      cqo();
      idiv(RG(divisor));
      mov(RG(dividend), RG(RAX));

      if (rdx_in_use) pop(RG(RDX));
      if (rax_in_use) pop(RG(RAX));

      mkIntVal(jit, dividend);
      localVarPo dst = localTarget(state, pc, opand(2));
      storeVar(state, pc, RG(dividend), dst);

      releaseReg(jit, dividend);
      releaseReg(jit, divisor);
      if (isRegInMap(savedFree, RAX)) state->jit->freeRegs = addReg(state->jit->freeRegs, RAX);
      if (isRegInMap(savedFree, RDX)) state->jit->freeRegs = addReg(state->jit->freeRegs, RDX);
      pc += insSize;
      continue;
    }
    case sIMod: {
      // L R --> L%R
      int32 insSize = 5;
      FlexOp left = localFlex(state, pc, opand(3));
      FlexOp right = localFlex(state, pc, opand(4));

      registerMap savedFree = state->jit->freeRegs;
      state->jit->freeRegs = dropReg(state->jit->freeRegs, RAX);
      state->jit->freeRegs = dropReg(state->jit->freeRegs, RDX);

      mcRegister divisor = findMcRegister(state, pc);
      loadRegister(state, divisor, right);
      getIntVal(jit, divisor);

      codeLblPo skip = newLabel(ctx);
      cbnz(divisor, skip);
      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];

      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));
      bind(skip);

      mcRegister dividend = findMcRegister(state, pc);
      loadRegister(state, dividend, left);
      getIntVal(jit, dividend);

      logical rax_in_use = !isRegInMap(savedFree, RAX);
      logical rdx_in_use = !isRegInMap(savedFree, RDX);

      if (rax_in_use) push(RG(RAX));
      if (rdx_in_use) push(RG(RDX));

      mov(RG(RAX), RG(dividend));
      cqo();
      idiv(RG(divisor));
      mov(RG(dividend), RG(RDX));

      if (rdx_in_use) pop(RG(RDX));
      if (rax_in_use) pop(RG(RAX));

      mkIntVal(jit, dividend);
      localVarPo dst = localTarget(state, pc, opand(2));
      storeVar(state, pc, RG(dividend), dst);

      releaseReg(jit, dividend);
      releaseReg(jit, divisor);
      if (isRegInMap(savedFree, RAX)) state->jit->freeRegs = addReg(state->jit->freeRegs, RAX);
      if (isRegInMap(savedFree, RDX)) state->jit->freeRegs = addReg(state->jit->freeRegs, RDX);
      pc += insSize;
      continue;
    }
    case sIAbs: {
      // L --> abs(L)
      int32 insSize = 3;
      FlexOp left = localFlex(state, pc, opand(2));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      getIntVal(jit, a1);
      cmp(RG(a1), IM(0));
      codeLblPo skip = newLabel(ctx);
      j_cc_(skip, GE_CC, ctx);
      neg(RG(a1));
      bind(skip);

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sCEq:
    case sIEq: {
      // L R --> L==R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      mcRegister fl = findMcRegister(state, pc);
      mcRegister tr = findMcRegister(state, pc);
      loadConstant(jit, trueIndex, tr);
      loadConstant(jit, falseIndex, fl);

      cmp(RG(a1), RG(a2));
      csel(a1, fl, tr, NE);

      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sCLt:
    case sILt: {
      // L R --> L<R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      mcRegister fl = findMcRegister(state, pc);
      mcRegister tr = findMcRegister(state, pc);
      loadConstant(jit, trueIndex, tr);
      loadConstant(jit, falseIndex, fl);

      cmp(RG(a1), RG(a2));
      csel(a1, tr, fl, LT);

      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sCGe:
    case sIGe: {
      // L R --> L>=R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      mcRegister fl = findMcRegister(state, pc);
      mcRegister tr = findMcRegister(state, pc);
      loadConstant(jit, trueIndex, tr);
      loadConstant(jit, falseIndex, fl);

      cmp(RG(a1), RG(a2));
      csel(a1, tr, fl, GE);

      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sBAnd: {
      // L R --> L&R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      and(RG(a1), RG(a2));

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sBOr: {
      // L R --> L|R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      or(RG(a1), RG(a2));

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sBXor: {
      // L R --> L^R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      xor(RG(a1), RG(a2));

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sBLsl: {
      // L R --> L<<R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      shiftRegister(state, pc, sBLsl, a1, a1, a2);

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sBLsr: {
      // L R --> L>>R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      shiftRegister(state, pc, sBLsr, a1, a1, a2);

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sBAsr: {
      // L R --> L>>>R
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);
      getIntVal(jit, a1);
      getIntVal(jit, a2);

      shiftRegister(state, pc, sBAsr, a1, a1, a2);

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sBNot: {
      // // L --> ~L
      int32 insSize = 3;
      FlexOp left = localFlex(state, pc, opand(2));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      getIntVal(jit, a1);

      not(RG(a1));

      mkIntVal(jit, a1);
      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sFAdd: {
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);

      getFltVal(jit, a1, F0);
      getFltVal(jit, a2, F1);
      releaseReg(jit, a1);
      releaseReg(jit, a2);
      addsd(FLT(F0), FLT(F1));
      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFSub: {
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);

      getFltVal(jit, a1, F0);
      getFltVal(jit, a2, F1);
      releaseReg(jit, a1);
      releaseReg(jit, a2);
      subsd(FLT(F0), FLT(F1));
      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFMul: {
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);

      getFltVal(jit, a1, F0);
      getFltVal(jit, a2, F1);
      releaseReg(jit, a1);
      releaseReg(jit, a2);
      mulsd(FLT(F0), FLT(F1));
      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFDiv: {
      int32 insSize = 5;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(3));
      FlexOp right = localFlex(state, pc, opand(4));
      localVarPo dst = localTarget(state, pc, opand(2));

      mcRegister dividend = findMcRegister(state, pc);
      mcRegister divisor = findMcRegister(state, pc);
      loadRegister(state, dividend, left);
      loadRegister(state, divisor, right);

      getFltVal(jit, dividend, F0);
      getFltVal(jit, divisor, F1);

      releaseReg(jit, dividend);
      releaseReg(jit, divisor);

      xorpd(FLT(F2), FLT(F2));
      ucomisd(FLT(F1), FLT(F2));
      codeLblPo skip = newLabel(ctx);
      jpe(skip);
      jne(skip);

      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];
      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));

      bind(skip);
      divsd(FLT(F0), FLT(F1));
      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFMod: {
      int32 insSize = 5;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(3));
      FlexOp right = localFlex(state, pc, opand(4));
      localVarPo dst = localTarget(state, pc, opand(2));

      mcRegister dividend = findMcRegister(state, pc);
      mcRegister divisor = findMcRegister(state, pc);
      loadRegister(state, dividend, left);
      loadRegister(state, divisor, right);

      getFltVal(jit, dividend, F0);
      getFltVal(jit, divisor, F1);

      releaseReg(jit, dividend);
      releaseReg(jit, divisor);

      xorpd(FLT(F2), FLT(F2));
      ucomisd(FLT(F1), FLT(F2));
      codeLblPo skip = newLabel(ctx);
      jpe(skip);
      jne(skip);

      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];
      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));

      bind(skip);
      movsd(FLT(F2), FLT(F0));
      divsd(FLT(F0), FLT(F1));
      cvttsd2si(RG(RAX), FLT(F0));
      cvtsi2sd(FLT(F0), RG(RAX));
      mulsd(FLT(F0), FLT(F1));
      subsd(FLT(F2), FLT(F0));
      movsd(FLT(F0), FLT(F2));

      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFAbs: {
      int32 insSize = 3;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(2));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      getFltVal(jit, a1, F0);
      releaseReg(jit, a1);

      mov(RG(RAX), IM(0x7fffffffffffffff));
      movq_g2x(FLT(F1), RG(RAX));
      andpd(FLT(F0), FLT(F1));

      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFEq: {
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);

      mcRegister fl = findMcRegister(state, pc);
      mcRegister tr = findMcRegister(state, pc);
      loadConstant(jit, trueIndex, tr);
      loadConstant(jit, falseIndex, fl);

      getFltVal(jit, a1, F0);
      getFltVal(jit, a2, F1);

      ucomisd(FLT(F0), FLT(F1));
      csel(a1, fl, tr, NE_CC);

      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sFLt: {
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);

      mcRegister fl = findMcRegister(state, pc);
      mcRegister tr = findMcRegister(state, pc);
      loadConstant(jit, trueIndex, tr);
      loadConstant(jit, falseIndex, fl);

      getFltVal(jit, a1, F0);
      getFltVal(jit, a2, F1);

      ucomisd(FLT(F0), FLT(F1));
      csel(a1, tr, fl, C_CC);

      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sFGe: {
      int32 insSize = 4;
      FlexOp left = localFlex(state, pc, opand(2));
      FlexOp right = localFlex(state, pc, opand(3));
      localVarPo dst = localTarget(state, pc, opand(1));

      mcRegister a1 = findMcRegister(state, pc);
      mcRegister a2 = findMcRegister(state, pc);
      loadRegister(state, a1, left);
      loadRegister(state, a2, right);

      mcRegister fl = findMcRegister(state, pc);
      mcRegister tr = findMcRegister(state, pc);
      loadConstant(jit, trueIndex, tr);
      loadConstant(jit, falseIndex, fl);

      getFltVal(jit, a1, F0);
      getFltVal(jit, a2, F1);

      ucomisd(FLT(F0), FLT(F1));
      csel(a1, tr, fl, AE_CC);

      storeVar(state, pc, RG(a1), dst);
      releaseReg(jit, a2);
      releaseReg(jit, a1);
      releaseReg(jit, tr);
      releaseReg(jit, fl);
      pc += insSize;
      continue;
    }
    case sAlloc: {
      // new structure, elements from stack
      int32 key = opand(1);
      labelPo label = C_LBL(getConstant(key));
      int32 arity = lblArity(label);
      int32 insSize = arity + 4;
      int32 nextPc = pc + insSize;

      if (arity == 1)
        allocUnary(state, pc, nextPc, label->labelIndex, localSource(state, pc, opand(4)));
      else if (arity == 2)
        allocBinary(state, pc, nextPc, label->labelIndex, localSource(state, pc, opand(4)),
                    localSource(state, pc, opand(5)));
      else {
        allocSmallStruct(state, pc, pc, label->labelIndex, NormalCellCount(arity));
        for (int32 ix = 0; ix < arity; ix++) {
          FlexOp tmp = localFlex(state, pc, opand(ix+4));
          storeFlex(state, pc, tmp,OF(RTV, (ix + 1) * pointerSize));
        }
      }

      storeVar(state, pc,RG(RTV), localTarget(state, pc,opand(2)));

#ifdef TRACEMEM
      if (traceMemory >= detailedTracing) {
        invokeIntrinsic(state, nextPc, nextPc, (runtimeFn)verifyEngine, 1, (FlexOp[]){RG(PR)}, False, 0, Null);
      }
#endif

      pc = nextPc;
      continue;
    }
    case sClosure: {
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      int32 key = opand(1);

      mcRegister cl = findMcRegister(state, pc);
      FlexOp freeTerm = localFlex(state, pc, opand(3));

      invokeIntrinsic(state, pc, nextPc, (runtimeFn)newClosure,
                      2, (FlexOp[]){constantFlex(key), freeTerm}, False, 1,
                      (FlexOp[]){RG(cl)});

      storeVar(state, pc,RG(cl), localTarget(state, pc,opand(2)));
      releaseReg(jit, cl);
      pc += insSize;
      continue;
    }
    case sFiber: {
      int32 insSize = 3;
      FlexOp lam = localFlex(state, pc, opand(2));
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)newStack, 3, (FlexOp[]){RG(PR), IM(True), lam},
                      True,
                      1, (FlexOp[]){RG(RTV)});
      storeVar(state, pc,RG(RTV), localTarget(state, pc,opand(1)));
      pc += insSize;
      continue;
    }
    case sSuspend: {
      int32 insSize = 3;
      mcRegister tmp = findMcRegister(state, pc);
      codeLblPo rtn = newLabel(ctx);
      adr(tmp, rtn);
      str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
      loadRegister(state, RTV, localFlex(state, pc,opand(2)));
      mov(RG(RTS), IM(0));
      push(RG(RTV));
      push(RG(RTS));
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)detachStack, 2, (FlexOp[]){
                        RG(PR), localFlex(state, pc,opand(1))
                      }, True, 0, Null);
      pop(RG(RTS));
      pop(RG(RTV));
      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      bind(rtn);
      releaseReg(jit, tmp);
      pc += insSize;
      continue;
    }
    case sResume: {
      int32 insSize = 3;
      codeLblPo rtn = newLabel(ctx);
      adr(X16, rtn);
      str(X16, OF(STK, OffsetOf(StackRecord, pc)));
      loadRegister(state, RTV, localFlex(state, pc,opand(2)));
      mov(RG(RTS), IM(0));
      push(RG(RTV));
      push(RG(RTS));
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)attachStack, 2, (FlexOp[]){
                        RG(PR), localFlex(state, pc,opand(1))
                      }, True, 0, Null);
      pop(RG(RTS));
      pop(RG(RTV));
      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      bind(rtn);
      pc += insSize;
      continue;
    }
    case sRetire: {
      // Similar to suspend, except that we trash the suspending stack
      int32 insSize = 3;
      loadRegister(state, RTV, localFlex(state, pc,opand(2)));
      mov(RG(RTS), IM(0));
      push(RG(RTV));
      push(RG(RTS));
      invokeIntrinsic(state, pc, pc, (runtimeFn)detachDropStack, 2,
                      (FlexOp[]){RG(PR), localFlex(state, pc,opand(1))}, True, 0, Null);
      pop(RG(RTS));
      pop(RG(RTV));

      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      pc += insSize;
      continue;
    }
    case sUnderflow: {
      int32 insSize = 1;
      // underflow from current stack
      push(RG(RTV));
      push(RG(RTS));
      invokeIntrinsic(state, pc, pc, (runtimeFn)detachDropStack, 2, (FlexOp[]){RG(PR),RG(STK)}, False, 0,
                      Null);
      pop(RG(RTS));
      pop(RG(RTV));
      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      pc += insSize;
      continue;
    }
    case sLine: {
      int32 insSize = 2;
      if (lineDebugging >= detailedTracing) {
        int32 locKey = opand(1);
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)lineDebug, 2, (FlexOp[]){
                          RG(PR), constantFlex(locKey)
                        }, False, 0, Null);
      }
      pc += insSize;
      continue;
    }
    case sBind: {
      int32 insSize = 3;
      if (lineDebugging >= detailedTracing) {
        int32 varKey = opand(1);
        FlexOp vl = localFlex(state, pc, opand(2));
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)bindDebug, 3, (FlexOp[]){
                          RG(PR), constantFlex(varKey), vl
                        }, False, 0, Null);
      }
      pc += insSize;
      continue;
    }
    case sdBug: {
      // enter the line debugger
      int32 insSize = 2;
      if (lineDebugging > noTracing) {
        int32 locKey = opand(1);
        int32 nextPc = pc + insSize;
        switch (code[nextPc].op.op) {
        case sAbort: {
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)abortDebug, 2,
                          (FlexOp[]){RG(PR), constantFlex(locKey)}, False, 0, Null);
          break;
        }
        case sEntry: {
          int32 lblKey = defineConstantLiteral((termPo)mtdLabel(jit->mtd));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)entryDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), constantFlex(lblKey)
                          }, False, 0, Null);
          break;
        }
        case sCall: {
          mcRegister argReg = allocCallArgVector(state, nextPc + 2, nextPc);

          invokeIntrinsic(state, pc, nextPc, (runtimeFn)callDebug, 5, (FlexOp[]){
                            RG(PR), IM(sCall), constantFlex(locKey),
                            constantFlex(operand(state, nextPc, 1)),
                            RG(argReg)
                          }, False, 0, Null);
          releaseReg(jit, argReg);
          break;
        }
        case sTCall: {
          mcRegister argReg = allocCallArgVector(state, nextPc + 2, nextPc);

          invokeIntrinsic(state, pc, nextPc, (runtimeFn)tcallDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey),
                            constantFlex(operand(state, nextPc, 1)),RG(argReg)
                          }, False, 0, Null);
          releaseReg(jit, argReg);
          break;
        }
        case sOCall: {
          mcRegister argReg = allocCallArgVector(state, nextPc + 2, nextPc);
          FlexOp lam = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)ocallDebug, 5, (FlexOp[]){
                            RG(PR), IM(sOCall), constantFlex(locKey), lam, RG(argReg)
                          }, False, 0, Null);
          releaseReg(jit, argReg);
          break;
        }
        case sTOCall: {
          mcRegister argReg = allocCallArgVector(state, nextPc + 2, nextPc);
          FlexOp lam = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)tocallDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), lam, RG(argReg)
                          }, False, 0, Null);
          releaseReg(jit, argReg);
          break;
        }
        case sRet: {
          FlexOp vl = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)retDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), IM((uinteger)state->mtd), vl
                          }, False, 0, Null);
          break;
        }
        case sRtn: {
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)rtnDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), IM((uinteger)state->mtd)
                          }, False, 0, Null);
          break;
        }
        case sXRet: {
          FlexOp vl = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)xretDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), IM((uinteger)state->mtd), vl
                          }, False, 0, Null);
          break;
        }
        case sAssign: {
          FlexOp dst = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp src = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)assignDebug, 4,
                          (FlexOp[]){RG(PR), constantFlex(locKey), dst, src}, False, 0, Null);
          break;
        }
        case sLG: {
          int32 glbKey = operand(state, nextPc, 1);
          globalPo glbVr = findGlobalVar(glbKey);
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)glbDebug, 3,
                          (FlexOp[]){RG(PR), constantFlex(locKey), IM((uint64)glbVr)}, False, 0, Null);
          break;
        }
        case sFiber: {
          FlexOp vl = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)fiberDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), vl
                          }, False, 0, Null);
          break;
        }
        case sSuspend: {
          FlexOp con = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp evt = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)suspendDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), con, evt
                          }, False, 0, Null);
          break;
        }
        case sResume: {
          FlexOp con = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp evt = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)resumeDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), con, evt
                          }, False, 0, Null);
          break;
        }
        case sRetire: {
          FlexOp con = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp evt = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)retireDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), con, evt
                          }, False, 0, Null);
          break;
        }
        default:
          return jitError(jit, "invalid instruction following DBug");
        }
      }
      pc += insSize;
      continue;
    }

    default:
      return jitError(jit, "unknown instruction: %s", ssaOpNames[code[pc].op.op]);
    }
  }

  if (state->jit->failed)
    ret = Error;
  return ret;
}

void allocSmallStruct(codeGenPo state, int32 pc, int32 livePc, int32 index, integer amnt) {
  invokeIntrinsic(state, pc, pc, (runtimeFn)allocateObject, 2, (FlexOp[]){
                    IM(index), IM(amnt)
                  }, True, 1, (FlexOp[]){RG(RTV)});
}

void allocUnary(codeGenPo state, int32 pc, int32 livePc, int32 index, localVarPo arg) {
  invokeIntrinsic(state, pc, livePc, (runtimeFn)allocateUnary, 2, (FlexOp[]){
                    IM(index), arg->src,
                  }, True, 1, (FlexOp[]){RG(RTV)});
}

void allocBinary(codeGenPo state, int32 pc, int32 livePc, int32 index, localVarPo left, localVarPo right) {
  invokeIntrinsic(state, pc, livePc, (runtimeFn)allocateBinary, 3, (FlexOp[]){
                    IM(index), left->src, right->src
                  }, True, 1, (FlexOp[]){RG(RTV)});
}

static void mkFloat(codeGenPo state, int32 pc, int32 livePc, fpReg dx) {
  assemCtxPo ctx = assemCtx(state->jit);
  sub(RG(RSP), IM(16));
  movsd(BS(RSP, 0), FLT(dx));

  allocSmallStruct(state, pc, livePc, floatIndex, FloatCellCount);

  movsd(FLT(dx), BS(RSP, 0));
  add(RG(RSP), IM(16));

  movsd(BS(RTV, OffsetOf(FloatRecord, dx)), FLT(dx));
}

void pushFrme(codeGenPo state, int32 pc, int32 argOffset) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  add(RG(FP), IM(sizeof(StackFrame))); // Use FP symbolically
  mov(BS(FP, OffsetOf(StackFrame, args)), RG(AG)); // Use FP and AG symbolically
  adjustAG(state, pc, argOffset);
}

retCode handleBreakTable(codeGenPo state, ssaInsPo code, blockPo block, int32 pc, int32 limit) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  while (pc < limit) {
    check(code[pc].op.op==sBreak||code[pc].op.op==sCont, "Expecting a Break instruction");
    blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
    codeLblPo lbl = (code[pc].op.op == sBreak ? breakLabel(tgtBlock) : loopLabel(tgtBlock));
    b(lbl);
    pc += 2;
  }
  return Ok;
}

void populateLocals(codeGenPo state, int32 arity, registerMap registerArgs) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    state->locals[ix].inUse = False;
    state->locals[ix].desc = Null;
    state->locals[ix].stkOff = -1;
    state->locals[ix].stashed = False;
    state->locals[ix].inited = False;
    state->locals[ix].src = (FlexOp){.mode = Reg, .size = sz64, .op.reg = XZR};
    state->voided[ix] = False;
  }

  int32 regArgCnt = 0;
  for (int32 ax = 0; ax < arity; ax++) {
    varDescPo desc = findVar(state->analysis, ax);
    localVarPo var = findSpareLocal(state, 0);
    state->voided[state->argMark + ax] = False;

    var->inUse = True;
    var->inited = True;
    var->desc = desc;
    mcRegister rg = nxtAvailArgReg(registerArgs);
    if (rg != XZR) {
      var->src = RG(rg);
      var->stashed = False;
      registerArgs = dropReg(registerArgs, rg);
      reserveReg(state->jit, rg);
      regArgCnt++;
    }
    else {
      var->stkOff = ax;
      var->stashed = True;
      var->src = varFlex(var->stkOff);
    }
  }

#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    showRegisterMap(logFile, registerArgs);
  }
#endif
}

int32 loadArgsToRegisters(codeGenPo state, registerMap argRegs, int32 livePc, int32 argBase, int32 arity) {
  ArgSpec operands[arity];

  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    mcRegister ax = nxtAvailArgReg(argRegs);
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = argSpec(argSrc, RG(ax));
    }
    else {
      int32 argSlot = currVarLimit - arity + ix;
      operands[ix] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(R10);
  shuffleVars(state->jit, operands, arity, &tmpMap);

  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit - arity; // return how must space is needed to preserve current locals and arguments.
}

int32 loadArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity) {
  registerMap argRegs = defaultArgRegs();
  ArgSpec operands[arity];
  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call
  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    mcRegister ax = nxtAvailArgReg(argRegs);
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = argSpec(argSrc, RG(ax));
    }
    else {
      int32 argSlot = currVarLimit - arity + ix;
      operands[ix] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(R10);
  shuffleVars(state->jit, operands, arity, &tmpMap);
  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit - arity; // return how must space is needed to preserve current locals and arguments.
}

int32 loadLambdaArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity) {
  registerMap argRegs = dropReg(defaultArgRegs(), RDI);
  ArgSpec operands[arity];
  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call
  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    mcRegister ax = nxtAvailArgReg(argRegs);
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = argSpec(argSrc, RG(ax));
    }
    else {
      int32 argSlot = currVarLimit - arity + ix;
      operands[ix] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(R10);
  shuffleVars(state->jit, operands, arity, &tmpMap);
  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit - arity; // return how must space is needed to preserve current locals and arguments.
}

int32 loadEscapeArguments(codeGenPo state, int32 pc, int32 livePc, int32 arity, int32 argBase) {
  ArgSpec operands[arity + 1];

  operands[0] = argSpec(RG(PR), RG(RDI));
  registerMap argRegs = dropReg(defaultArgRegs(), X0);

  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    mcRegister ax = nxtAvailArgReg(argRegs);
    int32 argSlot = currVarLimit - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix + 1] = argSpec(argSrc, RG(ax));
    }
    else {
      operands[ix + 1] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(R10);
  shuffleVars(state->jit, operands, arity + 1, &tmpMap);

  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit;                             // return how must space is needed to preserve current locals.
}

int32 overrideArguments(codeGenPo state, registerMap argRegs, int32 argPc, int32 arity) {
  ArgSpec operands[arity];

  int32 callerArity = mtdArity(state->jit->mtd);
  int32 tgtOff = callerArity - arity;

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp arg = sourceOperandFlex(state, argPc, ix);
    mcRegister rx = nxtAvailArgReg(argRegs);
    if (rx != XZR) {
      argRegs = dropReg(argRegs, rx);
      operands[ix] = argSpec(arg, RG(rx));
    }
    else {
      int32 argSlot = tgtOff + ix;
      operands[ix] = argSpec(arg, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(R10);
  shuffleVars(state->jit, operands, arity, &tmpMap);
  return tgtOff;
}

void adjustAG(codeGenPo state, int32 pc, int32 tgtOff) {
  assemCtxPo ctx = assemCtx(state->jit);
  int32 delta = tgtOff * pointerSize;
  if (delta > 0) {
    add(RG(AG), IM(delta));
  }
  else if (delta < 0) {
    sub(RG(AG), IM(-delta));
  }
}

void dropArguments(codeGenPo state, int32 pc) {
  retireExpiredVars(state, pc);
  resetRegMap(state->jit, defltAvailRegSet());
}

localVarPo findPhiVariable(codeGenPo state, int32 pc, int32 vrNo) {
  localVarPo var = localTarget(state, pc, vrNo);
  if (!var->desc->registerCandidate) {
    storeVar(state, pc, RG(XZR), var);
    state->voided[state->argMark + var->stkOff] = True;
  }
  return var;
}

void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var) {
  if (!var->inited) {
    if (var->desc->registerCandidate && haveFreeReg(state->jit)) {
      FlexOp rg = RG(findMcRegister(state, pc));
      storeFlex(state, pc, val, rg);
      var->inited = True;
      var->stashed = False;
      var->src = rg;
    }
    else {
      var->stkOff = nextStkOff(state, pc);
      var->src = varFlex(var->stkOff);
      storeFlex(state, pc, val, var->src);
      var->inited = True;
      var->stashed = True;
    }
  }
  else {
    assert(var->desc->kind == valof);
    storeFlex(state, pc, val, var->src);
  }
}

FlexOp varSrc(codeGenPo state, int32 pc, localVarPo var) {
  if (!var->inited) {
    if (var->desc->registerCandidate && haveFreeReg(state->jit)) {
      FlexOp rg = RG(findMcRegister(state, pc));
      var->inited = True;
      var->stashed = False;
      var->src = rg;
      return rg;
    }
    else {
      var->stkOff = nextStkOff(state, pc);
      var->src = varFlex(var->stkOff);
      var->inited = True;
      var->stashed = True;
      return var->src;
    }
  }
  return var->src;
}

localVarPo localSource(codeGenPo state, int32 pc, int32 lx) {
  varDescPo varDesc = findVar(state->analysis, lx);
  if (varDesc == Null)
    return Null;
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->desc == varDesc)
      return lcl;
  }
  return Null;
}

localVarPo localTarget(codeGenPo state, int32 pc, int32 lx) {
  varDescPo desc = findVar(state->analysis, lx);
  if (desc == Null)
    return Null;
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->inUse && lcl->desc == desc)
      return lcl;
  }
  localVarPo slot = findSpareLocal(state, pc);

  if (slot != Null) {
    slot->inUse = True;
    slot->inited = False;
    slot->desc = desc;
    slot->stashed = False;
  }

  return slot;
}

logical registerInUse(codeGenPo state, FlexOp src) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    if (state->locals[ix].inUse && sameFlexOp(state->locals[ix].src, src)) {
      return True;
    }
  }
  return False;
}

void retireExpiredVars(codeGenPo state, int32 pc) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->inUse) {
      varDescPo desc = lcl->desc;
      if (desc->end <= pc) {
#ifdef TRACEJIT
        if (traceJit >= detailedTracing) {
          outMsg(logFile, "Retire variable %V at %d\n", lcl, pc);
        }
#endif
        lcl->inUse = False;
        state->voided[state->argMark + desc->varNo] = False;
        if (isRegisterOp(lcl->src) && !registerInUse(state, lcl->src)) {
          releaseReg(state->jit, lcl->src.op.reg);
          lcl->src = RG(XZR);
        }
      }
    }
  }
}

int32 operand(codeGenPo state, int32 pc, int32 ox) {
  return state->code[pc + ox].op.ltrl;
}

FlexOp localFlex(codeGenPo state, int32 pc, int32 vrNo) {
  localVarPo lcl = localSource(state, pc, vrNo);
  if (lcl == Null) {
    state->jit->failed = True;
    strMsg(state->jit->errMsg, NumberOf(state->jit->errMsg), "undefined variable %s[%d] at pc %d",
           (vrNo >= 0 ? "A" : "L"), vrNo, pc);
    return RG(XZR);
  }
  return lcl->src;
}

FlexOp sourceOperandFlex(codeGenPo state, int32 pc, int32 ax) {
  return localFlex(state, pc, opand(ax));
}

localVarPo operandVar(codeGenPo state, int32 pc, int32 ax) {
  return localSource(state, pc, opand(ax));
}

mcRegister allocCallArgVector(codeGenPo state, int32 argPc, int32 livePc) {
  int32 arity = operand(state, argPc, 0);
  labelPo label = tplLbl(arity);
  mcRegister tplReg = findMcRegister(state, livePc);

  invokeIntrinsic(state, argPc, livePc, (runtimeFn)allocateObject, 2, (FlexOp[]){
                    IM(label->labelIndex), IM(NormalCellCount(arity))
                  }, True, 1, (FlexOp[]){RG(tplReg)});

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp tmp = localFlex(state, argPc, operand(state, argPc, ix + 1));
    storeFlex(state, argPc, tmp,OF(tplReg, (ix + 1) * pointerSize));
  }
  return tplReg;
}

ValueReturn invokeJitMethod(enginePo P, methodPo mtd) {
  jittedCode code = jitCode(mtd);
  stackPo stk = P->stk;
  int32 arity = lblArity(mtdLabel(mtd));
  ptrPo exitSP = stk->sp + arity - 1;

  int32 ret = Normal;
  termPo val = voidEnum;

  register termPo *co_reg asm("r12") = constAnts;
  register ptrPo ag_reg asm("r13") = stk->args;
  register stackPo stk_reg asm("r14") = stk;
  register enginePo pr_reg asm("r15") = P;
  register jittedCode code_reg asm("rax") = code;
  register framePo fp_reg asm("r10") = stk->fp;
  register int32 arity_reg asm("r11") = arity;

  asm volatile(
      "push %%rbp\n"
      "mov %%r10, %%rbp\n" // FP = stk->fp

      "cmp $1, %%r11\n"
      "jl 0f\n"
      "mov 0(%%r13), %%rdi\n"
      "cmp $2, %%r11\n"
      "jl 0f\n"
      "mov 8(%%r13), %%rsi\n"
      "cmp $3, %%r11\n"
      "jl 0f\n"
      "mov 16(%%r13), %%rdx\n"
      "cmp $4, %%r11\n"
      "jl 0f\n"
      "mov 24(%%r13), %%rcx\n"
      "cmp $5, %%r11\n"
      "jl 0f\n"
      "mov 32(%%r13), %%r8\n"
      "cmp $6, %%r11\n"
      "jl 0f\n"
      "mov 40(%%r13), %%r9\n"
      "0:\n"

      "lea 1f(%%rip), %%r11\n"
      "jmp *%%rax\n"
      "1:\n"

      "mov %%r13, 40(%%r14)\n" // stk->args = AG
      "mov %%rbp, 64(%%r14)\n" // stk->fp = FP
      "pop %%rbp\n"
      : "=a"(ret), "=d"(val)
      : "a"(code_reg), "r"(co_reg), "r"(ag_reg), "r"(stk_reg), "r"(pr_reg), "r"(fp_reg), "r"(arity_reg)
      : "rdi", "rsi", "rcx", "r8", "r9", "memory"
  );

  P->stk->sp = exitSP;
  return (ValueReturn){.value = val, .status = ret};
}

void conditionalSelect(assemCtxPo ctx, FlexOp dst, FlexOp trueVal, FlexOp falseVal, uint8 cond) {
  codeLblPo falseLbl = newLabel(ctx);
  codeLblPo endLbl = newLabel(ctx);
  j_cc_(falseLbl, cond ^ 1, ctx);
  move(ctx, dst, trueVal, scratchRegs());
  jmp(LB(endLbl));
  bind(falseLbl);
  move(ctx, dst, falseVal, scratchRegs());
  bind(endLbl);
}

void shiftRegister(codeGenPo state, int32 pc, ssaOp op, mcRegister dst, mcRegister src1, mcRegister src2) {
  assemCtxPo ctx = assemCtx(state->jit);
  logical swapped = False;
  if (src2 != RCX) {
    xchg(RG(RCX), RG(src2));
    swapped = True;
  }
  mcRegister real_src1 = (src1 == RCX) ? src2 : src1;
  mcRegister real_dst = (dst == RCX) ? src2 : dst;
  if (real_dst != real_src1) {
    mov(RG(real_dst), RG(real_src1));
  }
  switch (op) {
    case sBLsl:
      shl(RG(real_dst), RG(RCX));
      break;
    case sBLsr:
      shr(RG(real_dst), RG(RCX));
      break;
    case sBAsr:
      sar(RG(real_dst), RG(RCX));
      break;
    default:
      syserr("unsupported shift op");
  }
  if (swapped) {
    xchg(RG(RCX), RG(src2));
  }
}
