//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include <string.h>
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
#include "normalP.h"

/* Lower Star VM code to Arm64 code */
/*
* X0-X7 = argument registers & scratch registers
* X10 = return status
* X11 = return value
* X8-X9 = Temporary registers
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

static retCode jitBlock(blockPo block, codeGenPo state, ssaInsPo code, int32 from, int32 endPc);

static void verifyState(codeGenPo state, int32 pc);

static void pushFrme(codeGenPo state, int32 pc, armReg mtdRg, int32 argOffset);
static armReg allocSmallStruct(codeGenPo state, int32 pc, int32 livePc, int32 index, integer amnt);
static retCode handleBreakTable(codeGenPo state, ssaInsPo code, blockPo block, int32 pc, int32 limit);
static armReg mkFloat(codeGenPo state, int32 pc);
static void populateLocals(codeGenPo state, int32 arity, registerMap registerArgs);
static int32 operand(codeGenPo state, int32 pc, int32 ox);
static FlexOp localFlex(codeGenPo state, int32 pc, int32 vrNo);
static FlexOp sourceOperandFlex(codeGenPo state, int32 pc, int32 ax);
static localVarPo operandVar(codeGenPo state, int32 pc, int32 ax);
static void dumpState(codeGenPo state);
static int32 loadArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity);
static int32 loadLambdaArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity);
static int32 loadEscapeArguments(codeGenPo state, int32 pc, int32 arity, int32 argBase);
static void dropArguments(codeGenPo state, int32 pc);
static int32 overrideArguments(codeGenPo state, registerMap argRegs, int32 pc, int32 argPc, int32 arity);
static void adjustAG(codeGenPo state, int32 pc, int32 tgtOff);
static localVarPo findPhiVariable(codeGenPo state, int32 pc, int32 vrNo);
static void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var);

static void retireExpiredVars(codeGenPo state, int32 pc);
static logical registerInUse(codeGenPo state, FlexOp src);

#define opand(ox) operand(state, pc, (ox))

retCode jitInstructions(jitCompPo jit, methodPo mtd, registerMap argRegisters, char *errMsg, integer msgLen) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method %L\n", mtd);
  }
#endif

  AnalysisRecord analysis;

  retCode ret;
  if ((ret = analyseMethod(mtd, &analysis)) == Ok) {
#ifdef TRACEJIT
    if (traceJit > noTracing) {
    showAnalysis(logFile, &analysis);
    }
#endif
    int32 numSlots = slotCount(&analysis);
    LocalVar locals[numSlots];

    CodeGenState state = {
      .mtd = mtd, .code = entryPoint(mtd), .analysis = &analysis, .locals = locals, .numLocals = numSlots, .jit = jit,
    };

    populateLocals(&state, mtdArity(mtd), argRegisters);

    JitBlock block = {
      .startPc = 0, .endPc = codeSize(mtd),
      .breakLbl = Null, .loopLbl = Null,
      .parent = Null, .phiVar = Null
    };

    ret = jitBlock(&block, &state, entryPoint(mtd), 0, codeSize(mtd));
  }

  tearDownAnalysis(&analysis);
  return ret;
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
  for (int32 pc = from; ret == Ok && pc < endPc;) {
    retireExpiredVars(state, pc);
#ifdef TRACEJIT
    if (traceJit >= generalTracing) {
      showIns(logFile, Null, &code[pc]);
      outMsg(logFile, "\n%_");
    }
    if (traceJit >= detailedTracing) {
      dumpState(state);
    }
#endif
    verifyState(state, pc);
    switch (code[pc].op.op) {
      case sHalt: {
        // Stop execution
        int32 insSize = 2;
        FlexOp src = localFlex(state, pc, opand(1));
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) star_exit, 2, (FlexOp[]){RG(PR), src}, 0, (FlexOp[]){});
        pc += insSize;
        continue;
      }
      case sAbort: {
        // abort with message
        int32 insSize = 3;
        armReg loc = findARegister(state, pc);
        adr(loc,here());
        str(loc,OF(STK,OffsetOf(StackRecord,pc)));
        FlexOp val = sourceOperandFlex(state, pc, 2);
        loadConstant(jit, opand(1), loc);
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) abort_star, 3, (FlexOp[]){RG(PR), RG(loc), val}, 0,
                        (FlexOp[]){});
        releaseReg(jit, loc);
        pc += insSize;
        continue;
      }
      case sCall: {
        int32 insSize = opand(2) + 3;
        int32 key = opand(1);
        int32 arity = lblArity(C_LBL(getConstant(key)));
        int32 lclLimit = loadArguments(state, pc+insSize, pc + 3, arity);
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
        bailOut(state, pc, undefinedCode);

        bind(runMtd);
        pushFrme(state, pc, X17, lclLimit - arity);
        blr(X16);
        dropArguments(state, pc + insSize);
        pc += insSize;
        continue;
      }
      case sOCall: {
        int32 numArgs = opand(2);
        int32 insSize = numArgs + 3;
        int32 arity = numArgs + 1;
        FlexOp lam = sourceOperandFlex(state, pc, 1); // Pick up the closure
        armReg lamReg = X16;
        loadRegister(state, lamReg, lam);
        ldr(X0, OF(lamReg, OffsetOf(ClosureRecord, free)));
        int32 lclLimit = loadLambdaArguments(state, pc+insSize, pc + 3, numArgs);
        ldr(lamReg, OF(lamReg, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(lamReg, OffsetOf(LblRecord, mtd)));
        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(state, pc, undefinedCode);

        bind(haveMtd);
        pushFrme(state, pc, X17, lclLimit - arity);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArguments(state, pc + insSize);
        pc += insSize;
        continue;
      }
      case sTCall: {
        int32 insSize = opand(2) + 3;
        int32 key = opand(1);
        int32 arity = lblArity(C_LBL(getConstant(key)));

        int32 argPc = pc + 3;
        int32 tgtOff = overrideArguments(state, defaultArgRegs(), pc, argPc, arity);
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
        bailOut(state, pc, undefinedCode);

        bind(runMtd);
        adjustAG(state, pc, tgtOff);
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
        str(AG, OF(STK, OffsetOf(StackRecord,args)));

        // Pick up the old return address
        ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);
        dropArguments(state, pc + insSize);
        pc += insSize;
        continue;
      }
      case sTOCall: {
        int32 insSize = opand(2) + 3;
        int32 numArgs = opand(2);
        int32 arity = numArgs + 1;
        int32 argPc = pc + 3;
#ifdef TRACEJIT
        if (traceJit >= detailedTracing) {
          outMsg(logFile, "override frame in: ");
          dumpState(state);
        }
#endif
        armReg lamReg = X16;
        FlexOp lam = sourceOperandFlex(state, pc, 1); // Pick up the closure
        loadRegister(state, lamReg, lam);
        int32 tgtOff = overrideArguments(state, lambdaArgRegs(), pc, argPc, numArgs);
        adjustAG(state, pc, tgtOff);

        ldr(X0, OF(lamReg, OffsetOf(ClosureRecord, free)));
        ldr(lamReg, OF(lamReg, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(lamReg, OffsetOf(LblRecord, mtd)));
        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(state, pc, undefinedCode);

        bind(haveMtd);
        adjustAG(state, pc, tgtOff);
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
        str(AG, OF(STK, OffsetOf(StackRecord,args)));

        // Pick up the old return address
        ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);
        dropArguments(state, pc + insSize);
        pc += insSize;
        continue;
      }
      case sEscape: {
        int32 insSize = opand(2) + 3;
        int32 escNo = opand(1);
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);
        assert(arity==opand(2));

        int32 tgtOff = loadEscapeArguments(state, pc, arity, pc + 3);
        adjustAG(state, pc, tgtOff);
        stashEngineState(state->jit, -tgtOff, fixedRegSet(X16));
        registerMap saveMap = criticalRegs();
        saveRegisters(ctx, saveMap);
        mov(X16, IM((integer) escapeCode(esc)));
        blr(X16);
        mov(RTS, RG(X0));
        mov(RTV, RG(X1));
        restoreRegisters(ctx, saveMap);
        unstashEngineState(state->jit);
        dropArguments(state, pc + insSize);

        pc += insSize;
        continue;
      }
      case sEntry: {
        str(LR, OF(FP, OffsetOf(StackFrame, link)));
        stackCheck(state, pc, opand(1), opand(2));
        // locals definition
        pc += 3;
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

        if (!registerInUse(state, RG(RTS))) {
          releaseReg(jit,RTS);
          releaseReg(jit,RTV);
        }
        pc += insSize;
        continue;
      }
      case sRSX: {
        int32 insSize = 3;
        codeLblPo rsltOk = newLabel(ctx);
        cbz_w(RTS, rsltOk);
        blockPo tgtBlock = targetBlock(block, pc + opand(1), sValof);
        assert(tgtBlock!=Null);

        storeVar(state, pc,RG(RTV), tgtBlock->phiVar);
        b(breakLabel(tgtBlock));
        bind(rsltOk);

        localVarPo tgt = localTarget(state, pc, opand(2));
        storeVar(state, pc,RG(RTV), tgt);
        if (!registerInUse(state, RG(RTS))) {
          // Special case for the RTS/RTV registers
          releaseReg(jit,RTS);
          releaseReg(jit,RTV);
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
        sub(FP, FP, IM(sizeof(StackFrame)));
        mov(RTS, IM(Normal));
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
        sub(FP, FP, IM(sizeof(StackFrame)));
        mov(RTS, IM(Abnormal));
        br(X16);
        pc += insSize;
        continue;
      }
      case sRtn: {
        int32 insSize = 1;

        // Adjust args register
        ldr(AG, OF(FP, OffsetOf(StackFrame, args)));
        // Pick up return address
        ldr(X16, OF(FP, OffsetOf(StackFrame, link)));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        loadRegister(state, RTV, constantFlex(voidIndex));
        mov(RTS, IM(Normal));
        br(X16);
        pc += insSize;
        continue;
      }
      case sBlock: {
        // block of instructions
        int32 blockLen = opand(1);
        int32 nextPc = pc + blockLen;
        codeLblPo brkLbl = newLabel(ctx);

        JitBlock subBlock = {
          .blockType = sBlock,
          .startPc = pc,
          .endPc = nextPc,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
          .phiVar = Null
        };

        ret = jitBlock(&subBlock, state, code, pc + 2, nextPc);
        pc = nextPc; // Skip over the block
        retireExpiredVars(state, pc);
        bind(brkLbl);
        continue;
      }
      case sValof: {
        // vlof block of instructions
        int32 blockLen = opand(2);
        int32 nextPc = pc + blockLen;
        codeLblPo brkLbl = newLabel(ctx);

        JitBlock subBlock = {
          .blockType = sValof,
          .startPc = pc,
          .endPc = nextPc,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
          .phiVar = findPhiVariable(state, pc, opand(1))
        };

        ret = jitBlock(&subBlock, state, code, pc + 3, nextPc);
        pc = nextPc; // Skip over the block
        retireExpiredVars(state, pc);
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
        int32 insSize = 3;
        blockPo tgtBlock = targetBlock(block, pc + opand(1), sValof);
        localVarPo phiVar = tgtBlock->phiVar;
        localVarPo val = localSource(state, pc, opand(2)); // result variable
        storeVar(state, pc, val->src, phiVar);
        breakOut(state, pc + insSize, tgtBlock);
        pc += insSize;
        continue;
      }
      case sLoop: {
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
      case sIf: {
        // break if true
        int32 insSize = 3;
        blockPo tgt = targetBlock(block, pc + opand(1), sBlock);
        armReg tmp = findARegister(state, pc);
        FlexOp vl = sourceOperandFlex(state, pc, 2);
        loadConstant(jit, trueIndex, tmp);
        cmp(tmp, vl);
        beq(breakLabel(tgt));
        releaseReg(jit, tmp);
        pc += insSize;
        continue;
      }
      case sIfNot: {
        // break if false
        int32 insSize = 3;
        blockPo tgt = targetBlock(block, pc + opand(1), sBlock);
        armReg tmp = findARegister(state, pc);
        FlexOp vl = sourceOperandFlex(state, pc, 2);
        loadConstant(jit, trueIndex, tmp);
        cmp(tmp, vl);
        bne(breakLabel(tgt));
        releaseReg(jit, tmp);
        pc += insSize;
        continue;
      }
      case sICase: {
        int32 insSize = 3;
        armReg ix = findARegister(state, pc);
        int32 skip = opand(2);
        localVarPo govVr = operandVar(state, pc, 1);
        assert(govVr->live);
        loadRegister(state, ix, govVr->src);
        getIntVal(jit, ix);
        and(ix, ix, IM(LARGE_INT61));

        int32 mx = (skip - insSize) / 2;
        immModulo(ctx, ix, mx, jit->freeRegs);

        codeLblPo jmpTbl = newLabel(ctx);
        armReg off = findARegister(state, pc);
        adr(off, jmpTbl);
        add(off, off, LS(ix, 2));
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
        assert(govVr->live);
        armReg ix = findARegister(state, pc);
        invokeIntrinsic(state, pc, pc, (runtimeFn) hashTerm, 1, (FlexOp[]){govVr->src}, 1, (FlexOp[]){RG(ix)});
        int32 mx = (skip - insSize) / 2;
        immModulo(ctx, ix, mx, jit->freeRegs);
        codeLblPo jmpTbl = newLabel(ctx);
        armReg off = findARegister(state, pc);
        adr(off, jmpTbl);
        add(off, off, LS(ix, 2));
        br(off);
        releaseReg(jit, off);
        releaseReg(jit, ix);
        bind(jmpTbl);
        return handleBreakTable(state, code, block, pc + 3, pc + skip);
      }
      case sIxCase: {
        // check and jump on index
        int32 insSize = 3;
        armReg ix = findARegister(state, pc);
        int32 skip = opand(2);
        localVarPo govVr = operandVar(state, pc, 1);
        assert(govVr->live);
        loadRegister(state, ix, govVr->src);
        ldrw(ix, OF(ix, OffsetOf(TermHead,lblIndex))); // pick up the label
        int32 mx = (skip - insSize) / 2;
        immModulo(ctx, ix, mx, jit->freeRegs);

        codeLblPo jmpTbl = newLabel(ctx);
        armReg off = findARegister(state, pc);
        adr(off, jmpTbl);
        add(off, off, LS(ix, 2));
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
        armReg tmp = findARegister(state, pc);
        armReg tmp2 = findARegister(state, pc);
        FlexOp vl = localFlex(state, pc, opand(3));
        loadRegister(state, tmp, vl);
        tst(tmp, IM(0b11));
        bne(breakLabel(tgt));

        ldrw(tmp, OF(tmp, OffsetOf(TermHead,lblIndex))); // pick up the class
        labelPo lit = C_LBL(getConstant(key));
        mov_w(tmp2, IM(lit->labelIndex));
        cmp_w(tmp, RG(tmp));
        bne(breakLabel(tgt));
        releaseReg(jit, tmp);
        releaseReg(jit, tmp2);
        pc += insSize;
        continue;
      }
      case sCInt:
      case sCChar: {
        int32 insSize = 4;
        armReg tmp = findARegister(state, pc);
        FlexOp vl = localFlex(state, pc, opand(3));
        loadRegister(state, tmp, vl);
        int32 key = opand(1);

        integer lit = (integer) getConstant(key);
        if (is12bit(lit))
          cmp(tmp, IM(lit));
        else {
          armReg litReg = findFreeReg(jit);
          loadConstant(jit, key, litReg);
          cmp(tmp, RG(litReg));
          releaseReg(jit, litReg);
        }
        releaseReg(jit, tmp);
        blockPo tgt = targetBlock(block, pc + opand(2), sBlock);
        bne(breakLabel(tgt));
        pc += insSize;
        continue;
      }
      case sCFlt:
      case sCLit: {
        // T,lit --> test for a literal value, break if not
        int32 insSize = 4;
        int32 key = opand(1);
        FlexOp vl = localFlex(state, pc, opand(3));
        invokeIntrinsic(state, pc, pc, (runtimeFn) sameTerm, 2, (FlexOp[]){vl, constantFlex(key)}, 1,
                        (FlexOp[]){RG(RTV)});
        cmp_w(RTV, IM(True));
        blockPo tgt = targetBlock(block, pc + opand(2), sBlock);
        bne(breakLabel(tgt));
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
        int32 key = opand(1);
        armReg glb = findFreeReg(jit);
        armReg content = findFreeReg(jit);
        globalPo glbVr = findGlobalVar(key);
        mov(glb, IM((integer) glbVr));
        // Check if global is set
        ldr(content, OF(glb, OffsetOf(GlobalRecord, content)));
        codeLblPo haveContent = newLabel(ctx);
        loadRegister(state,RTV,RG(content));
        mov(RTS, IM(0));
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

        bailOut(state, pc, undefinedCode);

        bind(haveMtd);
        int32 argOffset = 0; // No actual arguments!
        pushFrme(state, pc, X17, argOffset);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));

        blr(X16);

        bind(haveContent);
        releaseReg(jit, glb);
        releaseReg(jit, content);
        pc += insSize;
        continue;
      }
      case sSG: {
        int32 insSize = 3;
        globalPo glbVr = findGlobalVar(opand(1));
        armReg glb = findFreeReg(jit);
        mov(glb, IM((integer) glbVr)); // Global var names are not subject to GC

        // store into a global variable
        localVarPo src = localSource(state, pc, opand(2));

        // Assign to the global var's content field
        storeFlex(state, pc, src->src, OF(glb, OffsetOf(GlobalRecord, content)));
        releaseReg(jit, glb);
        pc += insSize;
        continue;
      }
      case sSav: {
        // create a single assignment variable
        int32 insSize = 2;
        armReg cel = allocSmallStruct(state, pc, pc + insSize, singleIndex,SingleCellCount);
        armReg tmp = findARegister(state, pc);
        mov(tmp, IM((integer) Null));
        storeFlex(state, pc, RG(tmp), OF(cel, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        localVarPo tgt = localTarget(state, pc, opand(1));
        storeVar(state, pc,RG(tmp), tgt);
        pc += insSize;
        continue;
      }
      case sLdSav: {
        int32 insSize = 4;
        // dereference a sav, break if not set
        FlexOp sng = localFlex(state, pc, opand(3));
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        ldr(tmp, OF(tmp, OffsetOf(SingleRecord, content)));
        cbz(tmp, breakLabel(targetBlock(block, pc + opand(2), sBlock)));

        localVarPo tgt = localTarget(state, pc, opand(1));
        storeVar(state, pc,RG(tmp), tgt);
        pc += insSize;
        continue;
      }
      case sTstSav: {
        // test a sav, return a logical
        int32 insSize = 3;
        FlexOp sng = localFlex(state, pc, opand(2));
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        armReg tr = findARegister(state, pc);
        armReg fl = findARegister(state, pc);
        loadConstant(jit, falseIndex, fl);
        loadConstant(jit, trueIndex, tr);
        ldr(tmp, OF(tmp, OffsetOf(SingleRecord, content)));
        tst(tmp, RG(XZR));
        csel(tmp, tr, fl, EQ);
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
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, sng,RG(tmp));
        localVarPo val = localSource(state, pc, opand(3));
        storeFlex(state, pc, val->src, OF(tmp, OffsetOf(SingleRecord, content)));
        releaseReg(jit, tmp);
        pc += insSize;
        continue;
      }
      case sCell: {
        // create R/W cell
        int32 insSize = 3;
        armReg cel = allocSmallStruct(state, pc, pc + insSize, cellIndex,CellCellCount);
        FlexOp vl = localFlex(state, pc, opand(2));
        storeFlex(state, pc, vl,OF(cel, OffsetOf(CellRecord, content)));
        localVarPo tgt = localTarget(state, pc, opand(1));
        storeVar(state, pc,RG(cel), tgt);
        releaseReg(jit, cel);
        pc += insSize;
        continue;
      }
      case sGet: {
        // access a R/W cell
        int32 insSize = 3;
        FlexOp cel = localFlex(state, pc, opand(2));
        armReg vl = findARegister(state, pc);

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
        FlexOp vl = localFlex(state, pc, opand(2));
        armReg tmp = findARegister(state, pc);
        armReg tmp2 = findARegister(state, pc);
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
        armReg tmp = findARegister(state, pc);
        loadFlex(state, pc, trm,RG(tmp));
        loadElement(jit, tmp, tmp, opand(2)+1);
        localVarPo dst = localTarget(state, pc, opand(1));
        storeVar(state, pc,RG(tmp), dst);
        releaseReg(jit, tmp);
        pc += insSize;
        continue;
      }
      case sStNth: {
        // T el --> store in nth element
        int32 insSize = 4;
        armReg tmp = findARegister(state, pc);
        armReg tmp2 = findARegister(state, pc);
        FlexOp trm = localFlex(state, pc, opand(1));
        FlexOp vl = localFlex(state, pc, opand(3));
        loadRegister(state, tmp, trm);
        loadRegister(state, tmp2, vl);
        storeElement(jit, tmp2, tmp, opand(2)+1);
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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        add(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        sub(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        mul(a1, a1, a2);

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

        armReg divisor = findARegister(state, pc);
        loadRegister(state, divisor, right);
        getIntVal(jit, divisor);

        codeLblPo skip = newLabel(ctx);
        tst(divisor, RG(XZR));
        bne(skip);
        blockPo tgtBlock = targetBlock(block, pc + opand(1), sValof);
        blockPo parent = tgtBlock->parent;
        localVarPo phiVar = parent->phiVar;

        storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
        b(breakLabel(tgtBlock));
        bind(skip);

        armReg dividend = findARegister(state, pc);
        loadRegister(state, dividend, left);

        sdiv(dividend, dividend, divisor);
        mkIntVal(jit, dividend);
        localVarPo dst = localTarget(state, pc, opand(2));
        storeVar(state, pc, RG(dividend), dst);
        releaseReg(jit, dividend);
        releaseReg(jit, divisor);
        pc += insSize;
        continue;
      }
      case sIMod: {
        // L R --> L%R
        int32 insSize = 5;
        FlexOp left = localFlex(state, pc, opand(3));
        FlexOp right = localFlex(state, pc, opand(4));

        armReg divisor = findARegister(state, pc);
        loadRegister(state, divisor, right);
        getIntVal(jit, divisor);

        codeLblPo skip = newLabel(ctx);
        tst(divisor, RG(XZR));
        bne(skip);
        blockPo tgtBlock = targetBlock(block, pc + opand(1), sValof);
        blockPo parent = tgtBlock->parent;
        localVarPo phiVar = parent->phiVar;

        storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
        b(breakLabel(tgtBlock));
        bind(skip);

        armReg dividend = findARegister(state, pc);
        loadRegister(state, dividend, left);
        armReg quotient = findARegister(state, pc);

        sdiv(dividend, dividend, divisor);
        msub(dividend, divisor, quotient, dividend);

        mkIntVal(jit, dividend);
        localVarPo dst = localTarget(state, pc, opand(2));
        storeVar(state, pc, RG(dividend), dst);
        releaseReg(jit, dividend);
        releaseReg(jit, divisor);
        pc += insSize;

        releaseReg(jit, dividend);
        releaseReg(jit, divisor);
        releaseReg(jit, quotient);
        pc += insSize;
        continue;
      }
      case sIAbs: {
        // L --> abs(L)
        int32 insSize = 3;
        FlexOp left = localFlex(state, pc, opand(2));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        loadRegister(state, a1, left);
        getIntVal(jit, a1);
        cmp(a1, IM(0));
        csneg(a1, a1, a1, GE);

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        cmp(a1, RG(a2));
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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, LT);

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        cmp(a1, RG(a2));
        csel(a1, fl, tr, GE);

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        and(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        orr(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        eor(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsl(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        lsr(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        asr(a1, a1, RG(a2));

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

        armReg a1 = findARegister(state, pc);
        loadRegister(state, a1, left);
        getIntVal(jit, a1);

        mvn(a1, a1, LSL, 0);

        mkIntVal(jit, a1);
        storeVar(state, pc, RG(a1), dst);
        releaseReg(jit, a1);
        pc += insSize;
        continue;
      }
      case sFAdd: {
        // L R --> L+R
        armReg reslt = mkFloat(state, pc); // We create it first
        int32 insSize = 4;
        FlexOp left = localFlex(state, pc, opand(2));
        FlexOp right = localFlex(state, pc, opand(3));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);
        fadd(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        storeVar(state, pc, RG(reslt), dst);
        releaseReg(jit, reslt);
        pc += insSize;
        continue;
      }
      case sFSub: {
        // L R --> L-R
        armReg reslt = mkFloat(state, pc); // We create it first
        int32 insSize = 4;
        FlexOp left = localFlex(state, pc, opand(2));
        FlexOp right = localFlex(state, pc, opand(3));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);
        fsub(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        storeVar(state, pc, RG(reslt), dst);
        releaseReg(jit, reslt);
        pc += insSize;
        continue;
      }
      case sFMul: {
        // L R --> L*R
        armReg reslt = mkFloat(state, pc); // We create it first
        int32 insSize = 4;
        FlexOp left = localFlex(state, pc, opand(2));
        FlexOp right = localFlex(state, pc, opand(3));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);
        fmul(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        storeVar(state, pc, RG(reslt), dst);
        releaseReg(jit, reslt);
        pc += insSize;
        continue;
      }
      case sFDiv: {
        // L R --> L/R
        int32 insSize = 5;
        FlexOp left = localFlex(state, pc, opand(3));
        FlexOp right = localFlex(state, pc, opand(4));
        localVarPo dst = localTarget(state, pc, opand(2));

        armReg dividend = findARegister(state, pc);
        armReg divisor = findARegister(state, pc);
        loadRegister(state, dividend, left);
        loadRegister(state, divisor, right);

        getFltVal(jit, dividend, F0);
        getFltVal(jit, divisor, F1);

        releaseReg(jit, dividend);
        releaseReg(jit, divisor);

        fmov(FP(F2), RG(XZR));
        fcmp(F1, F2);
        codeLblPo skip = newLabel(ctx);
        bne(skip);

        blockPo tgtBlock = targetBlock(block, pc + opand(1), sValof);
        blockPo parent = tgtBlock->parent;
        localVarPo phiVar = parent->phiVar;

        storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
        b(breakLabel(tgtBlock));
        bind(skip);

        stpf(F0, F1, PRX(SP,-16));
        armReg reslt = mkFloat(state, pc);
        ldpf(F0, F1, PSX(SP,16));
        fdiv(F0, F0, F1);

        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        storeVar(state, pc, RG(reslt), dst);
        releaseReg(jit, reslt);
        pc += insSize;
        continue;
      }
      case sFMod: {
        // L R --> L%R
        int32 insSize = 5;
        FlexOp left = localFlex(state, pc, opand(3));
        FlexOp right = localFlex(state, pc, opand(4));
        localVarPo dst = localTarget(state, pc, opand(2));

        armReg a1 = findARegister(state, pc);
        armReg divisor = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, divisor, right);

        getFltVal(jit, a1, F0);
        getFltVal(jit, divisor, F1);

        fmov(FP(F2), RG(XZR));
        fcmp(F1, F2);
        codeLblPo skip = newLabel(ctx);
        bne(skip);

        blockPo tgtBlock = targetBlock(block, pc + opand(1), sValof);
        blockPo parent = tgtBlock->parent;
        localVarPo phiVar = parent->phiVar;

        storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
        b(breakLabel(tgtBlock));
        bind(skip);

        stpf(F0, F1, PRX(SP,-16));
        armReg reslt = mkFloat(state, pc);
        ldpf(F0, F1, PSX(SP,16));
        fdiv(F0, F0, F1);
        fmsub(F2, F2, F1, F0);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        storeVar(state, pc, RG(reslt), dst);
        releaseReg(jit, divisor);
        releaseReg(jit, a1);
        releaseReg(jit, reslt);
        pc += insSize;
        continue;
      }
      case sFAbs: {
        // L --> abs(L)
        armReg reslt = mkFloat(state, pc);

        int32 insSize = 3;
        FlexOp left = localFlex(state, pc, opand(2));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        loadRegister(state, a1, left);
        getFltVal(jit, a1, F0);

        fabs(F0, F0);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        storeVar(state, pc, RG(reslt), dst);
        releaseReg(jit, a1);
        pc += insSize;
        continue;
      }
      case sFEq: {
        // L R --> L==
        int32 insSize = 4;
        FlexOp left = localFlex(state, pc, opand(2));
        FlexOp right = localFlex(state, pc, opand(3));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        fcmp(F0, F1);
        csel(a1, fl, tr, NE);

        storeVar(state, pc, RG(a1), dst);
        releaseReg(jit, a2);
        releaseReg(jit, a1);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        pc += insSize;
        continue;
      }
      case sFLt: {
        // L R --> L<R
        int32 insSize = 4;
        FlexOp left = localFlex(state, pc, opand(2));
        FlexOp right = localFlex(state, pc, opand(3));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        fcmp(F0, F1);
        csel(a1, fl, tr, GE);

        storeVar(state, pc, RG(a1), dst);
        releaseReg(jit, a2);
        releaseReg(jit, a1);
        releaseReg(jit, tr);
        releaseReg(jit, fl);
        pc += insSize;
        continue;
      }
      case sFGe: {
        // L R --> L>=R
        int32 insSize = 4;
        FlexOp left = localFlex(state, pc, opand(2));
        FlexOp right = localFlex(state, pc, opand(3));
        localVarPo dst = localTarget(state, pc, opand(1));

        armReg a1 = findARegister(state, pc);
        armReg a2 = findARegister(state, pc);
        loadRegister(state, a1, left);
        loadRegister(state, a2, right);

        armReg fl = findARegister(state, pc);
        armReg tr = findARegister(state, pc);
        loadConstant(jit, trueIndex, tr);
        loadConstant(jit, falseIndex, fl);

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        fcmp(F0, F1);
        csel(a1, fl, tr, LT);

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

        armReg term = allocSmallStruct(state, pc, pc, label->labelIndex,NormalCellCount(arity));

        for (int32 ix = 0; ix < arity; ix++) {
          FlexOp tmp = localFlex(state, pc, opand(ix+4));
          storeFlex(state, pc, tmp,OF(term, (ix + 1) * pointerSize));
        }

        storeVar(state, pc,RG(term), localTarget(state, pc,opand(2)));
        releaseReg(jit,term);
        pc += insSize;
        continue;
      }
      case sClosure: {
        int32 insSize = 4;
        int32 key = opand(1);

        armReg term = allocSmallStruct(state, pc, pc, closureIndex,ClosureCellCount);

        armReg tmp = findARegister(state, pc);
        loadConstant(jit, key, tmp);

        str(tmp, OF(term, OffsetOf(ClosureRecord, lbl)));
        releaseReg(jit, tmp);

        FlexOp freeTerm = localFlex(state, pc, opand(3));
        storeFlex(state, pc, freeTerm,OF(term, OffsetOf(ClosureRecord, free)));

        storeVar(state, pc,RG(term), localTarget(state, pc,opand(2)));
        pc += insSize;
        continue;
      }
      case sBump:
      case sDrop: {
        int32 insSize = 2;
        bailOut(state, pc, invalidOperationCode);
        pc += insSize;
        continue;
      }
      case sFiber: {
        int32 insSize = 3;
        FlexOp lam = localFlex(state, pc, opand(2));
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) newStack, 3, (FlexOp[]){RG(PR), IM(True), lam}, 1,
                        (FlexOp[]){RG(RTV)});
        storeVar(state, pc,RG(RTV), localTarget(state, pc,opand(1)));
        pc += insSize;
        continue;
      }
      case sSuspend: {
        int32 insSize = 3;
        armReg tmp = findARegister(state, pc);
        codeLblPo rtn = newLabel(ctx);
        adr(tmp, rtn);
        str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
        loadRegister(state, RTV, localFlex(state, pc,opand(2)));
        mov(RTV, IM(0));
        stp(RTV, RTS, PRX(SP,-16));
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) detachStack, 2, (FlexOp[]){
                          RG(PR), localFlex(state, pc,opand(1))
                        }, 0, (FlexOp[]){});
        ldp(RTV, RTS, PSX(SP,16));
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
        mov(RTV, IM(0));
        stp(RTV, RTS, PRX(SP,-16));
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) attachStack, 2, (FlexOp[]){
                          RG(PR), localFlex(state, pc,opand(1))
                        }, 0, (FlexOp[]){});
        ldp(RTV, RTS, PSX(SP,16));
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
        mov(RTV, IM(0));
        stp(RTV, RTS, PRX(SP,-16));
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) detachDropStack, 2,
                        (FlexOp[]){RG(PR), localFlex(state, pc,opand(1))}, 0, (FlexOp[]){});
        ldp(RTV, RTS, PSX(SP,16));

        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        pc += insSize;
        continue;
      }
      case sUnderflow: {
        int32 insSize = 1;
        // underflow from current stack
        // Special concerns: ignore state and assume that X0 = RTS, X1=RTV
        stp(RTV, RTS, PRX(SP,-16));
        invokeIntrinsic(state, pc, pc, (runtimeFn) detachDropStack, 2, (FlexOp[]){RG(PR),RG(STK)}, 0, (FlexOp[]){});
        ldp(RTV, RTS, PSX(SP,16));
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        pc += insSize;
        continue;
      }
      case sLine: {
        int32 insSize = 2;
        if (lineDebugging) {
          int32 locKey = opand(1);
          invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) lineDebug, 2, (FlexOp[]){RG(PR), constantFlex(locKey)},
                          0,
                          (FlexOp[]){});
        }
        pc += insSize;
        continue;
      }
      case sBind: {
        int32 insSize = 3;
        if (lineDebugging) {
          int32 varKey = opand(1);
          FlexOp vl = localFlex(state, pc, opand(2));
          invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) bindDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(varKey), vl
                          }, 0, (FlexOp[]){});
        }
        pc += insSize;
        continue;
      }
      case sdBug: {
        // enter the line debugger
        int32 insSize = 2;
        if (lineDebugging) {
          int32 locKey = opand(1);
          int32 npc = pc + insSize;
          switch (code[npc].op.op) {
            case sAbort: {
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) abortDebug, 2,
                              (FlexOp[]){RG(PR), constantFlex(locKey)}, 0, (FlexOp[]){});
              break;
            }
            case sEntry: {
              int32 lblKey = defineConstantLiteral((termPo) mtdLabel(jit->mtd));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) entryDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey), constantFlex(lblKey)
                              }, 0, (FlexOp[]){});
              break;
            }
            case sCall: {
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) callDebug, 4, (FlexOp[]){
                                RG(PR), IM(sCall), constantFlex(locKey),
                                constantFlex(code[npc + 1].op.ltrl)
                              }, 0, (FlexOp[]){});
              break;
            }
            case sTCall: {
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) tcallDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey),
                                constantFlex(code[npc + 1].op.ltrl)
                              }, 0, (FlexOp[]){});
              break;
            }
            case sOCall: {
              FlexOp lam = localFlex(state, pc, opand(4));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) ocallDebug, 4, (FlexOp[]){
                                RG(PR), IM(sOCall),
                                constantFlex(locKey), lam
                              }, 0, (FlexOp[]){});
              break;
            }
            case sTOCall: {
              FlexOp lam = localFlex(state, pc, opand(4));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) tocallDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey), lam
                              }, 0, (FlexOp[]){});
              break;
            }
            case sRet: {
              FlexOp vl = localFlex(state, pc, opand(4));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) retDebug, 4, (FlexOp[]){
                                RG(PR), constantFlex(locKey), vl
                              }, 0, (FlexOp[]){});
              break;
            }
            case sXRet: {
              FlexOp vl = localFlex(state, pc, opand(4));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) xretDebug, 4, (FlexOp[]){
                                RG(PR), constantFlex(locKey), vl
                              }, 0, (FlexOp[]){});
              break;
            }
            case sAssign: {
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) assignDebug, 2,
                              (FlexOp[]){RG(PR), constantFlex(locKey)}, 0, (FlexOp[]){});
              break;
            }
            case sFiber: {
              FlexOp vl = localFlex(state, pc, opand(5));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) fiberDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey), vl
                              }, 0, (FlexOp[]){});
              break;
            }
            case sSuspend: {
              FlexOp vl = localFlex(state, pc, opand(5));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) suspendDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey), vl
                              }, 0, (FlexOp[]){});
              break;
            }
            case sResume: {
              FlexOp vl = localFlex(state, pc, opand(5));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) resumeDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey), vl
                              }, 0, (FlexOp[]){});
              break;
            }
            case sRetire: {
              FlexOp vl = localFlex(state, pc, opand(5));
              invokeIntrinsic(state, pc, pc + insSize, (runtimeFn) retireDebug, 3, (FlexOp[]){
                                RG(PR), constantFlex(locKey), vl
                              }, 0, (FlexOp[]){});
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

  return ret;
}

armReg allocSmallStruct(codeGenPo state, int32 pc, int32 livePc, int32 index, integer amnt) {
  armReg term = findARegister(state, pc);
  invokeIntrinsic(state, pc, livePc, (runtimeFn) allocateObject, 3, (FlexOp[]){
                    OF(PR, OffsetOf(EngineRecord, heap)), IM(index), IM(amnt)
                  }, 1, (FlexOp[]){RG(term)});
  return term;
}

armReg mkFloat(codeGenPo state, int32 pc) {
  return allocSmallStruct(state, pc, pc, floatIndex,FloatCellCount);
}

void pushFrme(codeGenPo state, int32 pc, armReg mtdRg, int32 argOffset) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, OffsetOf(StackFrame, args)));
  adjustAG(state, pc, argOffset);
  str(mtdRg, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  ldr(mtdRg, OF(STK, OffsetOf(StackRecord, prog)));
  str(mtdRg, OF(FP, OffsetOf(StackFrame, prog))); // We know what program we are executing
}

retCode handleBreakTable(codeGenPo state, ssaInsPo code, blockPo block, int32 pc, int32 limit) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  while (pc < limit) {
    check(code[pc].op.op==sBreak||code[pc].op.op==sLoop, "Expecting a Break instruction");
    blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
    codeLblPo lbl = (code[pc].op.op == sBreak ? breakLabel(tgtBlock) : loopLabel(tgtBlock));
    b(lbl);
    pc += 2;
  }
  return Ok;
}

void populateLocals(codeGenPo state, int32 arity, registerMap registerArgs) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    state->locals[ix].live = False;
    state->locals[ix].desc = Null;
  }

  for (int32 ax = 0; ax < arity; ax++) {
    varDescPo desc = findVar(state->analysis, ax);
    localVarPo slot = findSpareLocal(state, 0);
    slot->live = True;
    slot->inited = True;
    slot->stkOff = ax;
    slot->desc = desc;
    armReg rg = nxtAvailReg(registerArgs);
    if (rg != XZR) {
      slot->src = RG(rg);
      slot->stashed = False;
      registerArgs = dropReg(registerArgs, rg);
      reserveReg(state->jit, rg);
    } else {
      slot->stashed = True;
      slot->src = varFlex(ax);
    }
  }
#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    showRegisterMap(logFile, registerArgs);
  }
#endif
}

void dumpState(codeGenPo state) {
  showLiveLocals(logFile, state);
  dRegisterMap(state->jit->freeRegs);
}

int32 loadArgsToRegisters(codeGenPo state, registerMap argRegs, int32 livePc, int32 argBase, int32 arity) {
  ArgSpec operands[arity];

  int32 minOffset = stashLiveLocals(state, livePc, True); // save vars that will be live after the call
  voidOutFrameLocals(state, livePc, minOffset); // void out gaps in the locals map

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    armReg ax = nxtAvailReg(argRegs);
    int32 argSlot = minOffset - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = argSpec(argSrc, RG(ax));
    } else {
      operands[ix] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity, &tmpMap, argMove);
  return minOffset; // return how must space is needed to preserve current locals.
}

int32 loadArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity) {
  return loadArgsToRegisters(state, defaultArgRegs(), livePc, argBase, arity);
}

int32 loadLambdaArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity) {
  return loadArgsToRegisters(state, dropReg(defaultArgRegs(), X0), livePc, argBase, arity);
}

int32 loadEscapeArguments(codeGenPo state, int32 pc, int32 arity, int32 argBase) {
  ArgSpec operands[arity + 1];

  operands[0] = argSpec(RG(PR), RG(X0));
  registerMap argRegs = dropReg(defaultArgRegs(), X0);

  int32 minOffset = stashLiveLocals(state, argBase, True); // save vars that will be live after the call
  voidOutFrameLocals(state, pc, minOffset); // void out gaps in the locals map

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    armReg ax = nxtAvailReg(argRegs);
    int32 argSlot = minOffset - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix + 1] = argSpec(argSrc, RG(ax));
    } else {
      operands[ix + 1] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity + 1, &tmpMap, argMove);
  return minOffset; // return how must space is needed to preserve current locals.
}

int32 overrideArguments(codeGenPo state, registerMap argRegs, int32 pc, int32 argPc, int32 arity) {
  ArgSpec operands[arity];

  int32 tgtOff = argCount(state->jit->mtd);

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp arg = sourceOperandFlex(state, argPc, ix);
    armReg ax = nxtAvailReg(argRegs);
    int32 argSlot = tgtOff - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix] = argSpec(arg, RG(ax));
    } else {
      operands[ix] = argSpec(arg, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(assemCtx(state->jit), operands, arity, &tmpMap, argMove);
  return tgtOff - arity;
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
  } else if (delta < 0) {
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

void dropArguments(codeGenPo state, int32 pc) {
  retireExpiredVars(state, pc);
  resetRegMap(state->jit, defltAvailRegSet());
  assert(allLocalsStashed(state,pc));
}

localVarPo findPhiVariable(codeGenPo state, int32 pc, int32 vrNo) {
  return localTarget(state, pc, vrNo);
}

void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var) {
  if (!var->inited) {
    if (var->desc->registerCandidate) {
      FlexOp rg = RG(findARegister(state, pc));
      storeFlex(state, pc, val, rg);
      var->inited = True;
      var->stashed = False;
      var->src = rg;
    } else {
      var->stkOff = nextStkOff(state, pc);
      var->src = varFlex(var->stkOff);
      storeFlex(state, pc, val, var->src);
      var->inited = True;
      var->stashed = True;
    }
  } else {
    assert(var->desc->kind == phi);
    storeFlex(state, pc, val, var->src);
  }
}

localVarPo localSource(codeGenPo state, int32 pc, int32 lx) {
  varDescPo varDesc = findVar(state->analysis, lx);
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->desc == varDesc)
      return lcl;
  }
  return Null;
}

localVarPo localTarget(codeGenPo state, int32 pc, int32 lx) {
  varDescPo desc = findVar(state->analysis, lx);
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->live && lcl->desc == desc)
      return lcl;
  }
  localVarPo slot = findSpareLocal(state, pc);

  if (slot != Null) {
    slot->live = True;
    slot->inited = False;
    slot->desc = desc;
    slot->stashed = False;
  }

  return slot;
}

logical registerInUse(codeGenPo state, FlexOp src) {
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
      if (desc->end < pc) {
#ifdef TRACEJIT
        if (traceJit >= detailedTracing) {
          outMsg(logFile, "Retire variable %V at %d\n", lcl, pc);
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

int32 operand(codeGenPo state, int32 pc, int32 ox) {
  return state->code[pc + ox].op.ltrl;
}

FlexOp localFlex(codeGenPo state, int32 pc, int32 vrNo) {
  localVarPo lcl = localSource(state, pc, vrNo);
  return lcl->src;
}

FlexOp sourceOperandFlex(codeGenPo state, int32 pc, int32 ax) {
  return localFlex(state, pc, opand(ax));
}

localVarPo operandVar(codeGenPo state, int32 pc, int32 ax) {
  return localSource(state, pc, opand(ax));
}

void verifyState(codeGenPo state, int32 pc) {
  registerMap freeRegs = state->jit->freeRegs;
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo v = &state->locals[ix];
    if (v->live) {
      assert(v->desc->end >= pc);
      assert(isRegisterOp(v->src) ? !isRegInMap(freeRegs,v->src.reg):
        v->stashed ? v->src.immediate==v->stkOff*pointerSize:True);
      assert(v->stashed ? (v->stkOff>=-state->numLocals && v->stkOff<mtdArity(state->mtd)):True);
    }
  }
}

ValueReturn invokeJitMethod(enginePo P, methodPo mtd) {
  jittedCode code = jitCode(mtd);
  stackPo stk = P->stk;
  int32 arity = lblArity(mtdLabel(mtd));
  int32 numArgRegisters = maxArgRegister + 1;
  int64 argCount = (numArgRegisters - arity) << 2;
  ptrPo exitSP = stk->sp + arity - 1;

  int32 ret = Normal;
  termPo val = voidEnum;

  asm("stp x29,x30, [sp, #-16]!\n"
    "stp x8,x9, [sp, #-16]!\n"
    "stp x10,x11, [sp, #-16]!\n"
    "stp x12,x13, [sp, #-16]!\n"
    "stp x17,x19, [sp, #-16]!\n"
    "mov x14, %[stk]\n"
    "ldr x13, %[ag]\n"
    "mov x12, %[constants]\n"
    "mov x15, %[process]\n"
    "mov x16, %[code]\n"
    "mov x0, %[argcount]\n"
    "adr x1, 1f\n"
    "add x1, x1, x0\n"
    "br  x1\n"
    "1: ldr X7, [x13, #56]\n"
    "ldr x6, [x13, #48]\n"
    "ldr x5, [x13, #40]\n"
    "ldr x4, [x13, #32]\n"
    "ldr x3, [x13, #24]\n"
    "ldr x2, [x13, #16]\n"
    "ldr x1, [x13, #8]\n"
    "ldr x0, [x13, #0]\n"
    "ldr x29, %[fp]\n"
    "blr x16\n"
    "str X13, [x14,#40]\n" // we will need to change these if stack structure changes
    "str x29, [x14,#64]\n"
    "ldp x17,x19, [sp], #16\n"
    "ldp x12,x13, [sp], #16\n"
    "mov %w[ret], w11\n"
    "mov %[val], x10\n"
    "ldp x10,x11, [sp], #16\n"
    "ldp x8,x9, [sp], #16\n"
    "ldp x29,x30, [sp], #16\n"
    : [ret] "=r"(ret), [val] "=r" (val)
    : [process]"r"(P), [stk] "r"(stk), [code] "r"(code), [ag] "m"(stk->args), [argcount] "r" (argCount),
    [constants] "r"(constAnts),[fp] "m"(stk->fp)
    : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10","x11", "x12", "x13", "x14", "x15", "x16",
    "memory");

  P->stk->sp = exitSP;
  return (ValueReturn){.value = val, .status = ret};
}
