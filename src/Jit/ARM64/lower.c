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

static void pushFrme(codeGenPo state, int32 pc, int32 argOffset);
static void emitCallInvoke(codeGenPo state, int32 pc, int32 nextPc, int32 key, int32 argPnt);
static void emitTCallInvoke(codeGenPo state, int32 pc, int32 key, int32 tgtOff);
static void emitOCallInvoke(codeGenPo state, int32 pc, int32 nextPc, armReg lamReg, int32 argPnt);
static void emitTOCallInvoke(codeGenPo state, int32 pc, armReg lamReg, int32 tgtOff);
static void allocSmallStruct(codeGenPo state, int32 pc, int32 livePc, int32 index, integer amnt);
static void allocUnary(codeGenPo state, int32 pc, int32 livePc, int32 index, localVarPo arg);
static void allocBinary(codeGenPo state, int32 pc, int32 livePc, int32 index, localVarPo left, localVarPo right);

typedef void (*aluBinOpFn)(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
typedef void (*fpBinOpFn)(Precision p, fpReg Rd, fpReg Rn, fpReg Rm, assemCtxPo ctx);

static void mulFlex_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);

// Resolve an operand to a register for read-only use (the caller never writes through
// the returned register): if already a register, use it directly -- no allocation, no
// liveness check needed, since nothing here mutates it. Otherwise load into a fresh
// scratch register. *allocated tells the caller whether it owns a register it must
// release; do not use this for an operand the caller will also write to (see
// operandRegister for that case).
static armReg readOperandRegister(codeGenPo state, int32 pc, FlexOp src, logical* allocated);

// Resolve an operand to a register for a destructive binary op: if the operand is
// already in a register AND this use is its last (desc->end <= nextPc, i.e. no
// recorded use at or after nextPc) AND that register isn't `avoid` (the register
// already chosen for the other operand -- guards against e.g. `X op X` aliasing,
// where reusing X's register for both operands would corrupt the first operation's
// input before the second reads it), operate on that register directly. Otherwise
// copy into a fresh register, exactly as before. Pass XZR for `avoid` when there is
// no other operand register to avoid yet (i.e. when resolving the first operand).
static armReg operandRegister(codeGenPo state, int32 pc, int32 nextPc, int32 varOx, FlexOp src, armReg avoid);

static void binaryIntOp(codeGenPo state, int32 pc, int32 nextPc, int32 dstOx, int32 leftOx, int32 rightOx,
                        aluBinOpFn op);

// A tagged integer is (value << 2) | intTg. For AND/OR/ADD/SUB/XOR, operating on the
// tagged bit patterns directly and applying one of these constant fixups afterward gives
// the same result as untag/op/retag, since the low tag bits (both operands' tag is intTg,
// identical) interact predictably with each operator -- see call sites for the derivation
// specific to each op. Do not add a new caller without re-deriving the fixup: this shortcut
// does not hold for every operator (e.g. multiply, shifts).
typedef enum {
  tagNone,   // AND, OR: tag bits combine to intTg on their own
  tagSubOne, // ADD: tagged sum has tag value 2*intTg; subtract intTg to correct
  tagAddOne, // SUB: tagged difference has tag value 0; add intTg to correct
  tagOrOne   // XOR: tag bits cancel to 0; or intTg back in
} tagFixup;

static void binaryIntOpTagged(codeGenPo state, int32 pc, int32 nextPc, int32 dstOx, int32 leftOx, int32 rightOx,
                              aluBinOpFn op, tagFixup fixup);
static void binaryIntCompare(codeGenPo state, int32 pc, int32 dstOx, int32 leftOx, int32 rightOx, armCond cond);
static void binaryFloatOp(codeGenPo state, int32 pc, int32 nextPc, int32 dstOx, int32 leftOx, int32 rightOx,
                          fpBinOpFn op);
static void binaryFloatCompare(codeGenPo state, int32 pc, int32 dstOx, int32 leftOx, int32 rightOx, armCond cond);

static retCode handleBreakTable(codeGenPo state, ssaInsPo code, blockPo block, int32 pc, int32 limit);
static void mkFloat(codeGenPo state, int32 pc, int32 livePc, fpReg dx);
static void populateLocals(codeGenPo state, int32 arity, registerMap registerArgs);
static int32 operand(codeGenPo state, int32 pc, int32 ox);
static FlexOp localFlex(codeGenPo state, int32 pc, int32 vrNo);
static FlexOp sourceOperandFlex(codeGenPo state, int32 pc, int32 ax);
static localVarPo operandVar(codeGenPo state, int32 pc, int32 ax);
static int32 loadArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity);
static int32 loadLambdaArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity, armReg lamReg);
static int32 loadEscapeArguments(codeGenPo state, int32 pc, int32 livePc, int32 arity, int32 argBase);
static void dropArguments(codeGenPo state, int32 pc);
// `tmpReg` is the register shuffleVars may use to break cycles in the argument
// permutation; it must not hold anything live across the call. sTCall passes X16, but
// sTOCall keeps the closure there (emitTOCallInvoke asserts lamReg != X17, so the closure
// cannot live in X17 instead) and passes X17, which is only used later by the dispatch.
static int32 overrideArguments(codeGenPo state, registerMap argRegs, int32 argPc, int32 arity, armReg tmpReg);
static void adjustAG(codeGenPo state, int32 pc, int32 tgtOff);
static localVarPo findPhiVariable(codeGenPo state, int32 pc, int32 vrNo);
static void storeVar(codeGenPo state, int32 pc, FlexOp val, localVarPo var);
static FlexOp varSrc(codeGenPo state, int32 pc, localVarPo var);

static void retireExpiredVars(codeGenPo state, int32 pc);
static logical registerInUse(codeGenPo state, FlexOp src);

#define opand(ox) operand(state, pc, (ox))

retCode jitInstructions(jitCompPo jit, methodPo mtd, registerMap argRegisters, char* errMsg, integer msgLen) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method", mtd);
  }
#endif

  AnalysisRecord analysis;

  retCode ret;
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

    if (lineDebugging != noTracing) {
      assemCtxPo ctx = assemCtx(jit);
      str(LR, OF(FP, OffsetOf(StackFrame, link)));
    }
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

      armReg vl = findARegister(state, pc);
      loadRegister(state, vl, src);
      getIntVal(jit, vl);
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)star_exit, 1, (FlexOp[]){RG(vl)}, False, 0,
                      (FlexOp[]){});
      pc += insSize;
      continue;
    }
    case sAbort: {
      // abort with message
      int32 insSize = 3;
      armReg loc = findARegister(state, pc);
      adr(loc, here());
      str(loc, OF(STK,OffsetOf(StackRecord,pc)));
      loadConstant(jit, opand(1), loc);
      armReg vl = findARegister(state, pc);
      loadRegister(state, vl, sourceOperandFlex(state, pc, 2));
      getIntVal(jit, vl);
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)abort_star, 3, (FlexOp[]){RG(PR), RG(loc), RG(vl)},
                      False, 0, (FlexOp[]){});
      releaseReg(jit, loc);
      releaseReg(jit, vl);
      pc += insSize;
      continue;
    }
    case sCall: {
      int32 insSize = opand(2) + 3;
      int32 key = opand(1);
      int32 arity = lblArity(C_LBL(getConstant(key)));
      int32 nextPc = pc + insSize;

      int32 argPnt = loadArguments(state, nextPc, pc + 3, arity);
      emitCallInvoke(state, pc, nextPc, key, argPnt);
      pc = nextPc;
      continue;
    }
    case sOCall: {
      int32 numArgs = opand(2);
      int32 insSize = numArgs + 3;
      int32 nextPc = pc + insSize;
      FlexOp lam = sourceOperandFlex(state, pc, 1); // Pick up the closure
      armReg lamReg = X17;
      loadRegister(state, lamReg, lam);
      int32 argPnt = loadLambdaArguments(state, nextPc, pc + 3, numArgs + 1, lamReg);
      emitOCallInvoke(state, pc, nextPc, lamReg, argPnt);
      pc = nextPc;
      continue;
    }
    case sTCall: {
      int32 insSize = opand(2) + 3;
      int32 nextPc = pc + insSize;
      int32 key = opand(1);
      labelPo tgt = C_LBL(getConstant(key));
      int32 arity = lblArity(tgt);

      int32 argPc = pc + 3;
      int32 tgtOff = overrideArguments(state, defaultArgRegs(), argPc, arity, X16);

      emitTCallInvoke(state, pc, key, tgtOff);
      pc = nextPc;
      continue;
    }
    case sTOCall: {
      int32 insSize = opand(2) + 3;
      int32 numArgs = opand(2);
      int32 argPc = pc + 3;
      int32 nextPc = pc + insSize;
      armReg lamReg = X16;
      FlexOp lam = sourceOperandFlex(state, pc, 1); // Pick up the closure
      loadRegister(state, lamReg, lam);
      int32 tgtOff = overrideArguments(state, lambdaArgRegs(), argPc, numArgs, X17) - 1;

      ldr(X0, OF(lamReg, OffsetOf(ClosureRecord, free)));
      if (lineDebugging != noTracing) {
        str(X0, OF(AG,tgtOff*pointerSize));
      }
      emitTOCallInvoke(state, pc, lamReg, tgtOff);
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
      mov(X16, IM((integer) escapeCode(esc)));
      blr(X16);
      mov(RTS, RG(X0));
      mov(RTV, RG(X1));
      restoreRegisters(ctx, saveMap);
      loadCGlobal(ctx, CO, &constAnts);
      unstashEngineState(state->jit);
      dropArguments(state, pc + insSize);

      pc = nextPc;
      continue;
    }
    case sEntry: {
      int32 nextPc = pc + 3;
      if (lineDebugging == noTracing) {
        str(LR, OF(FP, OffsetOf(StackFrame, link)));
      }
      flushArguments(state, nextPc);
      stackCheck(state, pc, opand(1), opand(2));
      if (mtdHasName(state->mtd, "star.ideal@patchVec")) {
        installBkPt(state, pc);
      }
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
      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock!=Null && tgtBlock->phiCnt==1);

      storeVar(state, pc,RG(RTV), tgtBlock->phiVars[0]);
      b(breakLabel(tgtBlock));
      bind(rsltOk);

      localVarPo tgt = localTarget(state, pc, opand(2));
      storeVar(state, pc,RG(RTV), tgt);
      if (!registerInUse(state, RG(RTV))) {
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
        FlexOp src = localSource(state, pc, opand(ax+3))->src; // result arg
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
      armReg ix = findARegister(state, pc);
      int32 skip = opand(2);
      localVarPo govVr = operandVar(state, pc, 1);
      assert(govVr->inUse);
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
      assert(govVr->inUse);
      armReg ix = findARegister(state, pc);
      invokeIntrinsic(state, pc, pc, (runtimeFn)hashTerm, 1, (FlexOp[]){govVr->src}, True, 1,
                      (FlexOp[]){RG(ix)});
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
      assert(govVr->inUse);
      loadRegister(state, ix, govVr->src);
      ldrw(ix, OF(ix, OffsetOf(TermHead,lblIndex))); // pick up the label index
      armReg labels = findARegister(state, pc);
      mov(labels, IM((uinteger)labelConstructorIndex));
      ldrw(ix, EX2(labels, ix, U_XTX, 2));
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
      if (is12bit(lit->labelIndex))
        cmp_w(tmp, IM(lit->labelIndex));
      else {
        mov_w(tmp2, IM(lit->labelIndex));
        cmp_w(tmp, RG(tmp2));
      }
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

      integer lit = (integer)getConstant(key);
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
      invokeIntrinsic(state, pc, pc, (runtimeFn)sameTerm, 2, (FlexOp[]){vl, constantFlex(key)}, True,
                      1, (FlexOp[]){RG(RTV)});
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
      int32 nextPc = pc + insSize;
      int32 key = opand(1);
      armReg glb = findFreeReg(jit);
      globalPo glbVr = findGlobalVar(key);
      mov(glb, IM((integer) glbVr));
      // Check if global is set
      ldr(RTV, OF(glb, OffsetOf(GlobalRecord, content)));
      codeLblPo haveContent = newLabel(ctx);
      mov(RTS, IM(0));
      cbnz(RTV, haveContent);

      labelPo glbLbl = declareLbl(globalVarName(glbVr), 0, 0);
      if (glbLbl == Null)
        return jitError(jit, "no label definition for global %s", globalVarName(glbVr));

      int32 lblKey = defineConstantLiteral((termPo)glbLbl);
      loadConstant(jit, lblKey, X16);

      // pick up the pointer to the method
      ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

      codeLblPo haveMtd = newLabel(ctx);
      cbnz(X17, haveMtd);

      bailOut(state, pc, undefinedCode);

      bind(haveMtd);

      int32 minOffset = stashLiveLocals(state, nextPc, True); // save vars that will be live after the call
      voidOutFrameLocals(state, nextPc, minOffset);           // void out gaps in the locals map
      pushFrme(state, pc, minOffset);

      // Pick up the jit code itself
      ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));

      blr(X16);
      dropArguments(state, nextPc);
      bind(haveContent);
      releaseReg(jit, glb);
      pc = nextPc;
      continue;
    }
    case sSG: {
      int32 insSize = 3;
      globalPo glbVr = findGlobalVar(opand(1));
      armReg glbReg = findFreeReg(jit);
      mov(glbReg, IM((integer) glbVr)); // Global var names are not subject to GC

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

      armReg savReg = findARegister(state, pc);

      invokeIntrinsic(state, pc, nextPc, (runtimeFn)newSingleVar, 0, (FlexOp[]){}, False, 1, (FlexOp[]){RG(savReg)});

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
      armReg tmp = findARegister(state, pc);
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
      armReg tmp = findARegister(state, pc);
      loadFlex(state, pc, sng,RG(tmp));
      armReg tr = findARegister(state, pc);
      armReg fl = findARegister(state, pc);
      loadConstant(jit, falseIndex, fl);
      loadConstant(jit, trueIndex, tr);
      ldr(tmp, OF(tmp, OffsetOf(SingleRecord, content)));
      tst(tmp, RG(tmp));
      csel(tmp, fl, tr, EQ);
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
      armReg sngReg = findARegister(state, pc);
      loadFlex(state, pc, sng,RG(sngReg));
      localVarPo val = localSource(state, pc, opand(3));
      if (isRegisterOp(val->src)) {
        storeFlex(state, pc, val->src,OF(sngReg, OffsetOf(SingleRecord, content)));
        storeVar(state, pc, val->src, localTarget(state, pc, opand(1)));
      }
      else {
        armReg vlReg = findARegister(state, pc);
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

      armReg cell = findARegister(state, pc);

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
      writeBarrier(state, pc, cel);
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
      armReg tmp = findARegister(state, pc);
      armReg tmp2 = findARegister(state, pc);
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
      binaryIntOpTagged(state, pc, pc + insSize, opand(1), opand(2), opand(3), add_, tagSubOne);
      pc += insSize;
      continue;
    }
    case sISub: {
      // L R --> L-R
      int32 insSize = 4;
      binaryIntOpTagged(state, pc, pc + insSize, opand(1), opand(2), opand(3), sub_, tagAddOne);
      pc += insSize;
      continue;
    }
    case sIMul: {
      // L R --> L*R
      int32 insSize = 4;
      binaryIntOp(state, pc, pc + insSize, opand(1), opand(2), opand(3), mulFlex_);
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
      cbnz(divisor, skip);

      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];

      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));
      bind(skip);

      armReg dividend = findARegister(state, pc);
      loadRegister(state, dividend, left);
      getIntVal(jit, dividend);
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
      cbnz(divisor, skip);
      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];

      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));
      bind(skip);

      armReg dividend = findARegister(state, pc);
      loadRegister(state, dividend, left);
      armReg quotient = findARegister(state, pc);

      getIntVal(jit, dividend);
      sdiv(quotient, dividend, divisor);
      msub(dividend, quotient, divisor, dividend);

      mkIntVal(jit, dividend);
      localVarPo dst = localTarget(state, pc, opand(2));
      storeVar(state, pc, RG(dividend), dst);

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
      binaryIntCompare(state, pc, opand(1), opand(2), opand(3), EQ);
      pc += insSize;
      continue;
    }
    case sCLt:
    case sILt: {
      // L R --> L<R
      int32 insSize = 4;
      binaryIntCompare(state, pc, opand(1), opand(2), opand(3), LT);
      pc += insSize;
      continue;
    }
    case sCGe:
    case sIGe: {
      // L R --> L>=R
      int32 insSize = 4;
      binaryIntCompare(state, pc, opand(1), opand(2), opand(3), GE);
      pc += insSize;
      continue;
    }
    case sBAnd: {
      // L R --> L&R
      int32 insSize = 4;
      binaryIntOpTagged(state, pc, pc + insSize, opand(1), opand(2), opand(3), and_, tagNone);
      pc += insSize;
      continue;
    }
    case sBOr: {
      // L R --> L|R
      int32 insSize = 4;
      binaryIntOpTagged(state, pc, pc + insSize, opand(1), opand(2), opand(3), orr_, tagNone);
      pc += insSize;
      continue;
    }
    case sBXor: {
      // L R --> L^R
      int32 insSize = 4;
      binaryIntOpTagged(state, pc, pc + insSize, opand(1), opand(2), opand(3), eor_, tagOrOne);
      pc += insSize;
      continue;
    }
    case sBLsl: {
      // L R --> L<<R
      int32 insSize = 4;
      binaryIntOp(state, pc, pc + insSize, opand(1), opand(2), opand(3), lsl_);
      pc += insSize;
      continue;
    }
    case sBLsr: {
      // L R --> L>>R
      int32 insSize = 4;
      binaryIntOp(state, pc, pc + insSize, opand(1), opand(2), opand(3), lsr_);
      pc += insSize;
      continue;
    }
    case sBAsr: {
      // L R --> L>>>R
      int32 insSize = 4;
      binaryIntOp(state, pc, pc + insSize, opand(1), opand(2), opand(3), asr_);
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
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      binaryFloatOp(state, pc, nextPc, opand(1), opand(2), opand(3), fadd_);
      pc += insSize;
      continue;
    }
    case sFSub: {
      // L R --> L-R
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      binaryFloatOp(state, pc, nextPc, opand(1), opand(2), opand(3), fsub_);
      pc += insSize;
      continue;
    }
    case sFMul: {
      // L R --> L*R
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      binaryFloatOp(state, pc, nextPc, opand(1), opand(2), opand(3), fmul_);
      pc += insSize;
      continue;
    }
    case sFDiv: {
      // L R --> L/R
      int32 insSize = 5;
      int32 nextPc = pc + insSize;
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

      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];

      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));
      bind(skip);
      fdiv(F0, F0, F1);
      mkFloat(state, pc, nextPc, F0);
      storeVar(state, pc, RG(RTV), dst);
      pc += insSize;
      continue;
    }
    case sFMod: {
      // L R --> L%R
      int32 insSize = 5;
      int32 nextPc = pc + insSize;
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

      blockPo tgtBlock = targetBlock(block, pc + opand(1), sBlock);
      assert(tgtBlock->phiCnt==1);
      localVarPo phiVar = tgtBlock->phiVars[0];

      storeVar(state, pc, constantFlex(divZeroIndex), phiVar);
      b(breakLabel(tgtBlock));
      bind(skip);

      stpf(F0, F1, PRX(SP,-16));
      ldpf(F0, F1, PSX(SP,16));
      fdiv(F0, F0, F1);
      fmsub(F2, F2, F1, F0);
      mkFloat(state, pc, nextPc, F0);

      storeVar(state, pc, RG(RTV), dst);
      releaseReg(jit, divisor);
      releaseReg(jit, a1);
      pc += insSize;
      continue;
    }
    case sFAbs: {
      // L --> abs(L)
      int32 insSize = 3;
      int32 nextPc = pc + insSize;
      FlexOp left = localFlex(state, pc, opand(2));
      localVarPo dst = localTarget(state, pc, opand(1));

      armReg a1 = findARegister(state, pc);
      loadRegister(state, a1, left);
      getFltVal(jit, a1, F0);

      fabs(F0, F0);
      verifyState(state, pc);
      mkFloat(state, pc, nextPc, F0);
      verifyState(state, pc);
      storeVar(state, pc, RG(RTV), dst);
      releaseReg(jit, a1);
      pc = nextPc;
      continue;
    }
    case sFEq: {
      // L R --> L==
      int32 insSize = 4;
      binaryFloatCompare(state, pc, opand(1), opand(2), opand(3), EQ);
      pc += insSize;
      continue;
    }
    case sFLt: {
      // L R --> L<R
      int32 insSize = 4;
      binaryFloatCompare(state, pc, opand(1), opand(2), opand(3), LT);
      pc += insSize;
      continue;
    }
    case sFGe: {
      // L R --> L>=R
      int32 insSize = 4;
      binaryFloatCompare(state, pc, opand(1), opand(2), opand(3), GE);
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
        invokeIntrinsic(state, nextPc, nextPc, (runtimeFn)verifyEngine, 1, (FlexOp[]){RG(PR)}, False, 0, (FlexOp[]){});
      }
#endif

      pc = nextPc;
      continue;
    }
    case sClosure: {
      int32 insSize = 4;
      int32 nextPc = pc + insSize;
      int32 key = opand(1);

      armReg cl = findARegister(state, pc);
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
      armReg tmp = findARegister(state, pc);
      codeLblPo rtn = newLabel(ctx);
      adr(tmp, rtn);
      str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
      // Parallel move, for the same reason as sRetire/sResume: RTV and RTS are both
      // allocatable, so the fiber local may live in one of them and a sequential load of
      // the event/status would clobber it before it is read.
      ArgSpec specs[3] = {
        argSpec(localFlex(state, pc, opand(1)), RG(X1)),
        argSpec(localFlex(state, pc, opand(2)), RG(RTV)),
        argSpec(IM(0), RG(RTS))
      };
      shuffleVars(jit, specs, 3, &jit->freeRegs);
      stp(RTV, RTS, PRX(SP,-16));
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)detachStack, 2, (FlexOp[]){
                        RG(PR), RG(X1)
                      }, True, 0, (FlexOp[]){});
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
      ArgSpec specs[3] = {
        argSpec(localFlex(state, pc, opand(1)), RG(X1)),
        argSpec(localFlex(state, pc, opand(2)), RG(RTV)),
        argSpec(IM(0), RG(RTS))
      };
      shuffleVars(jit, specs, 3, &jit->freeRegs);
      stp(RTV, RTS, PRX(SP,-16));
      invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)attachStack, 2, (FlexOp[]){
                        RG(PR), RG(X1)
                      }, True, 0, (FlexOp[]){});
      ldp(RTV, RTS, PSX(SP,16));
      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      bind(rtn);
      pc += insSize;
      continue;
    }
    case sRetire: {
      // Similar to suspend, except that we trash the suspending stack.
      // The fiber, the event and the 0 status all have to be put in place with a
      // parallel move (same shape as sResume): RTV and RTS are both allocatable, so the
      // fiber local may itself live in one of them, and loading the event/status
      // sequentially would clobber the fiber before it is read -- handing
      // detachDropStack the event value in place of the stack.
      int32 insSize = 3;
      ArgSpec specs[3] = {
        argSpec(localFlex(state, pc, opand(1)), RG(X1)),
        argSpec(localFlex(state, pc, opand(2)), RG(RTV)),
        argSpec(IM(0), RG(RTS))
      };
      shuffleVars(jit, specs, 3, &jit->freeRegs);
      stp(RTV, RTS, PRX(SP,-16));
      invokeIntrinsic(state, pc, pc, (runtimeFn)detachDropStack, 2,
                      (FlexOp[]){RG(PR), RG(X1)}, True, 0, (FlexOp[]){});
      ldp(RTV, RTS, PSX(SP,16));

      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      pc += insSize;
      continue;
    }
    case sUnderflow: {
      int32 insSize = 1;
      // underflow from current stack
      stp(RTV, RTS, PRX(SP,-16));
      invokeIntrinsic(state, pc, pc, (runtimeFn)detachDropStack, 2, (FlexOp[]){RG(PR),RG(STK)}, False, 0,
                      (FlexOp[]){});
      ldp(RTV, RTS, PSX(SP,16));
      ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
      br(X16);
      pc += insSize;
      continue;
    }
    case sLine: {
      int32 insSize = 2;
      if (lineDebugging > generalTracing) {
        int32 locKey = opand(1);
        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)lineDebug, 2, (FlexOp[]){
                          RG(PR), constantFlex(locKey)
                        }, False, 0, (FlexOp[]){});
      }
      pc += insSize;
      continue;
    }
    case sBind: {
      int32 insSize = 4;
      if (lineDebugging > noTracing) {
        FlexOp var = constantFlex(opand(1));
        FlexOp loc = constantFlex(opand(2));
        FlexOp vl = localFlex(state, pc, opand(3));

        invokeIntrinsic(state, pc, pc + insSize, (runtimeFn)bindDebug, 4, (FlexOp[]){
                          RG(PR), var, loc, vl
                        }, False, 0, (FlexOp[]){});
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
                          (FlexOp[]){RG(PR), constantFlex(locKey)}, False, 0, (FlexOp[]){});
          break;
        }
        case sEntry: {
          int32 lblKey = defineConstantLiteral((termPo)mtdLabel(jit->mtd));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)entryDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), constantFlex(lblKey)
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sCall: {
          // Stage the call's arguments before the debug hook fires, so showArgs sees them
          // at their final location instead of the caller's stale locals (see loadArguments).
          int32 key = operand(state, nextPc, 1);
          int32 numArgs = operand(state, nextPc, 2);
          int32 callInsSize = numArgs + 3;
          int32 afterCallPc = nextPc + callInsSize;
          int32 arity = lblArity(C_LBL(getConstant(key)));

          int32 argPnt = loadArguments(state, afterCallPc, nextPc + 3, arity);

          invokeIntrinsic(state, pc, nextPc, (runtimeFn)callDebug, 4, (FlexOp[]){
                            RG(PR), IM(sCall), constantFlex(locKey),
                            constantFlex(key)
                          }, False, 0, (FlexOp[]){});

          emitCallInvoke(state, nextPc, afterCallPc, key, argPnt);
          pc = afterCallPc;
          continue;
        }
        case sTCall: {
          int32 key = operand(state, nextPc, 1);
          int32 numArgs = operand(state, nextPc, 2);
          int32 callInsSize = numArgs + 3;
          int32 afterCallPc = nextPc + callInsSize;
          int32 arity = lblArity(C_LBL(getConstant(key)));
          int32 argPc = nextPc + 3;

          int32 tgtOff = overrideArguments(state, defaultArgRegs(), argPc, arity, X16);

          invokeIntrinsic(state, pc, nextPc, (runtimeFn)tcallDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey),
                            constantFlex(key)
                          }, False, 0, (FlexOp[]){});

          emitTCallInvoke(state, nextPc, key, tgtOff);
          pc = afterCallPc;
          continue;
        }
        case sOCall: {
          int32 numArgs = operand(state, nextPc, 2);
          int32 callInsSize = numArgs + 3;
          int32 afterCallPc = nextPc + callInsSize;
          FlexOp lamSrc = sourceOperandFlex(state, nextPc, 1); // Pick up the closure
          armReg lamReg = X17;
          loadRegister(state, lamReg, lamSrc);
          int32 argPnt = loadLambdaArguments(state, afterCallPc, nextPc + 3, numArgs + 1, lamReg);

          FlexOp lam = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)ocallDebug, 4, (FlexOp[]){
                            RG(PR), IM(sOCall), constantFlex(locKey), lam
                          }, False, 0, (FlexOp[]){});

          // lamReg is scratch and may have been clobbered by the intrinsic call; reload it.
          loadRegister(state, lamReg, lamSrc);
          emitOCallInvoke(state, nextPc, afterCallPc, lamReg, argPnt);
          pc = afterCallPc;
          continue;
        }
        case sTOCall: {
          int32 numArgs = operand(state, nextPc, 2);
          int32 callInsSize = numArgs + 3;
          int32 afterCallPc = nextPc + callInsSize;
          int32 argPc = nextPc + 3;
          armReg lamReg = X16;
          FlexOp lamSrc = sourceOperandFlex(state, nextPc, 1); // Pick up the closure
          loadRegister(state, lamReg, lamSrc);
          int32 tgtOff = overrideArguments(state, lambdaArgRegs(), argPc, numArgs, X17) - 1;

          ldr(X0, OF(lamReg, OffsetOf(ClosureRecord, free)));
          str(X0, OF(AG,tgtOff*pointerSize));

          FlexOp lam = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)tocallDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), lam
                          }, False, 0, (FlexOp[]){});

          // lamReg is scratch and may have been clobbered by the intrinsic call; reload it.
          loadRegister(state, lamReg, lamSrc);
          emitTOCallInvoke(state, nextPc, lamReg, tgtOff);
          pc = afterCallPc;
          continue;
        }
        case sRet: {
          FlexOp vl = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)retDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), IM((uinteger)state->mtd), vl
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sRtn: {
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)rtnDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), IM((uinteger)state->mtd)
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sXRet: {
          FlexOp vl = localFlex(state, pc, operand(state, nextPc, 1));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)xretDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), IM((uinteger)state->mtd), vl
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sAssign: {
          FlexOp dst = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp src = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)assignDebug, 4,
                          (FlexOp[]){RG(PR), constantFlex(locKey), dst, src}, False, 0, (FlexOp[]){});
          break;
        }
        case sLG: {
          int32 glbKey = operand(state, nextPc, 1);
          globalPo glbVr = findGlobalVar(glbKey);
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)glbDebug, 3,
                          (FlexOp[]){RG(PR), constantFlex(locKey), IM((uint64)glbVr)}, False, 0, (FlexOp[]){});
          break;
        }
        case sFiber: {
          FlexOp vl = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)fiberDebug, 3, (FlexOp[]){
                            RG(PR), constantFlex(locKey), vl
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sSuspend: {
          FlexOp con = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp evt = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)suspendDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), con, evt
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sResume: {
          FlexOp con = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp evt = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)resumeDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), con, evt
                          }, False, 0, (FlexOp[]){});
          break;
        }
        case sRetire: {
          FlexOp con = localFlex(state, pc, operand(state, nextPc, 1));
          FlexOp evt = localFlex(state, pc, operand(state, nextPc, 2));
          invokeIntrinsic(state, pc, nextPc, (runtimeFn)retireDebug, 4, (FlexOp[]){
                            RG(PR), constantFlex(locKey), con, evt
                          }, False, 0, (FlexOp[]){});
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

// mul_ takes a register operand rather than a FlexOp; adapt it to the aluBinOpFn shape
// so sIMul can share binaryIntOp with the other binary integer ops.
static void mulFlex_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  assert(isRegisterOp(S2));
  mul_(w, Rd, Rn, S2.reg, ctx);
}

armReg readOperandRegister(codeGenPo state, int32 pc, FlexOp src, logical* allocated) {
  if (isRegisterOp(src)) {
    *allocated = False;
    return src.reg;
  }
  armReg fresh = findARegister(state, pc);
  loadRegister(state, fresh, src);
  *allocated = True;
  return fresh;
}

// See the forward declaration comment for the safety argument. `var` is looked up via
// localSource purely to read its liveness (desc->end); its .src is not used here since
// `src` (already fetched by the caller via localFlex) is the same thing.
armReg operandRegister(codeGenPo state, int32 pc, int32 nextPc, int32 varOx, FlexOp src, armReg avoid) {
  if (isRegisterOp(src) && src.reg != avoid) {
    localVarPo var = localSource(state, pc, varOx);
    if (var != Null && var->desc->end <= nextPc) {
      return src.reg; // last use of this variable -- safe to operate on its register directly
    }
  }
  armReg fresh = findARegister(state, pc);
  loadRegister(state, fresh, src);
  return fresh;
}

// Shared shape for sIMul/sBLsl/sBLsr/sBAsr: load both operands, untag, apply the ALU
// op, retag, store. (sIAdd/sISub/sBAnd/sBOr/sBXor use binaryIntOpTagged instead, which
// skips the untag/retag round-trip.)
void binaryIntOp(codeGenPo state, int32 pc, int32 nextPc, int32 dstOx, int32 leftOx, int32 rightOx, aluBinOpFn op) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  FlexOp left = localFlex(state, pc, leftOx);
  FlexOp right = localFlex(state, pc, rightOx);
  localVarPo dst = localTarget(state, pc, dstOx);

  armReg a1 = operandRegister(state, pc, nextPc, leftOx, left, XZR);
  armReg a2 = operandRegister(state, pc, nextPc, rightOx, right, a1);
  getIntVal(jit, a1);
  getIntVal(jit, a2);

  op(1, a1, a1, RG(a2), ctx);

  mkIntVal(jit, a1);
  storeVar(state, pc, RG(a1), dst);
  releaseReg(jit, a2);
  releaseReg(jit, a1);
}

// Shared shape for sIAdd/sISub/sBAnd/sBOr/sBXor: operate directly on the tagged bit
// patterns instead of untag/op/retag, applying a small constant fixup (see the tagFixup
// derivation above). Each fixup is exact 64-bit modular arithmetic, so this produces
// bit-identical results to the untag/op/retag path in every case, including overflow.
void binaryIntOpTagged(codeGenPo state, int32 pc, int32 nextPc, int32 dstOx, int32 leftOx, int32 rightOx,
                       aluBinOpFn op, tagFixup fixup) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  FlexOp left = localFlex(state, pc, leftOx);
  FlexOp right = localFlex(state, pc, rightOx);
  localVarPo dst = localTarget(state, pc, dstOx);

  armReg a1 = operandRegister(state, pc, nextPc, leftOx, left, XZR);
  armReg a2 = operandRegister(state, pc, nextPc, rightOx, right, a1);

  op(1, a1, a1, RG(a2), ctx);

  switch (fixup) {
  case tagSubOne:
    sub(a1, a1, IM(intTg));
    break;
  case tagAddOne:
    add(a1, a1, IM(intTg));
    break;
  case tagOrOne:
    orr(a1, a1, IM(intTg));
    break;
  case tagNone:
    break;
  }

  storeVar(state, pc, RG(a1), dst);
  releaseReg(jit, a2);
  releaseReg(jit, a1);
}

// Shared shape for sCEq/sIEq, sCLt/sILt, sCGe/sIGe: compare directly on the tagged
// representations and select the true/false constant based on cond. A tagged integer is
// 4v+1; since f(v)=4v+1 is strictly monotonic and injective (for any v that validly fits
// the tagged representation), equality and signed ordering of tagged values exactly match
// equality/ordering of the underlying integers -- no untagging needed at all, and no fixup
// afterward either (unlike the arithmetic ops), since neither `cmp` nor the operand loads
// write through a1/a2. The csel result goes into `tr` (already holding the true constant,
// self-selecting when cond holds) rather than a1, so a1/a2 stay purely read-only and can
// use readOperandRegister instead of the liveness-checked operandRegister.
void binaryIntCompare(codeGenPo state, int32 pc, int32 dstOx, int32 leftOx, int32 rightOx, armCond cond) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  FlexOp left = localFlex(state, pc, leftOx);
  FlexOp right = localFlex(state, pc, rightOx);
  localVarPo dst = localTarget(state, pc, dstOx);

  logical a1Fresh, a2Fresh;
  armReg a1 = readOperandRegister(state, pc, left, &a1Fresh);
  armReg a2 = readOperandRegister(state, pc, right, &a2Fresh);

  armReg fl = findARegister(state, pc);
  armReg tr = findARegister(state, pc);
  loadConstant(jit, trueIndex, tr);
  loadConstant(jit, falseIndex, fl);

  cmp(a1, RG(a2));
  csel(tr, tr, fl, cond);

  storeVar(state, pc, RG(tr), dst);
  if (a2Fresh) releaseReg(jit, a2);
  if (a1Fresh) releaseReg(jit, a1);
  releaseReg(jit, tr);
  releaseReg(jit, fl);
}

// Shared shape for sFAdd/sFSub/sFMul: load both operands as floats, apply the FP op, box
// result. getFltVal only reads through a1/a2 (never writes them), so when an operand is
// already a register, use it directly instead of copying -- no liveness check needed,
// since nothing here mutates the source register.
void binaryFloatOp(codeGenPo state, int32 pc, int32 nextPc, int32 dstOx, int32 leftOx, int32 rightOx,
                   fpBinOpFn op) {
  jitCompPo jit = state->jit;
  FlexOp left = localFlex(state, pc, leftOx);
  FlexOp right = localFlex(state, pc, rightOx);
  localVarPo dst = localTarget(state, pc, dstOx);

  logical a1Fresh, a2Fresh;
  armReg a1 = readOperandRegister(state, pc, left, &a1Fresh);
  armReg a2 = readOperandRegister(state, pc, right, &a2Fresh);

  getFltVal(jit, a1, F0);
  getFltVal(jit, a2, F1);
  if (a1Fresh) releaseReg(jit, a1);
  if (a2Fresh) releaseReg(jit, a2);
  op(Double, F0, F0, F1, assemCtx(jit));
  mkFloat(state, pc, nextPc, F0);
  storeVar(state, pc, RG(RTV), dst);
}

// Shared shape for sFEq/sFLt/sFGe: load both operands as floats, compare, then select
// the true/false constant based on cond. Same reasoning as binaryIntCompare for routing
// the csel result through `tr` instead of a1: keeps a1/a2 purely read-only so they can
// use readOperandRegister.
void binaryFloatCompare(codeGenPo state, int32 pc, int32 dstOx, int32 leftOx, int32 rightOx, armCond cond) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  FlexOp left = localFlex(state, pc, leftOx);
  FlexOp right = localFlex(state, pc, rightOx);
  localVarPo dst = localTarget(state, pc, dstOx);

  logical a1Fresh, a2Fresh;
  armReg a1 = readOperandRegister(state, pc, left, &a1Fresh);
  armReg a2 = readOperandRegister(state, pc, right, &a2Fresh);

  armReg fl = findARegister(state, pc);
  armReg tr = findARegister(state, pc);
  loadConstant(jit, trueIndex, tr);
  loadConstant(jit, falseIndex, fl);

  getFltVal(jit, a1, F0);
  getFltVal(jit, a2, F1);
  if (a1Fresh) releaseReg(jit, a1);
  if (a2Fresh) releaseReg(jit, a2);

  fcmp(F0, F1);
  csel(tr, tr, fl, cond);

  storeVar(state, pc, RG(tr), dst);
  releaseReg(jit, tr);
  releaseReg(jit, fl);
}

void allocSmallStruct(codeGenPo state, int32 pc, int32 livePc, int32 index, integer amnt) {
  invokeIntrinsic(state, pc, livePc, (runtimeFn)allocateObject, 2, (FlexOp[]){
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

void mkFloat(codeGenPo state, int32 pc, int32 livePc, fpReg dx) {
  assemCtxPo ctx = assemCtx(state->jit);
  stpf(dx, F1, PRX(SP,-16));
  allocSmallStruct(state, pc, livePc, floatIndex, FloatCellCount);
  ldpf(dx, F1, PSX(SP,16));
  fstr(F0, OF(RTV,OffsetOf(FloatRecord,dx)));
}

void pushFrme(codeGenPo state, int32 pc, int32 argOffset) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, OffsetOf(StackFrame, args)));
  adjustAG(state, pc, argOffset);
}

// Resolve the callee and branch to it. Arguments must already be staged (see loadArguments).
void emitCallInvoke(codeGenPo state, int32 pc, int32 nextPc, int32 key, int32 argPnt) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  labelPo tgt = C_LBL(getConstant(key));
  methodPo callee = labelMtd(tgt);

  if (callee != Null && hasJitCode(callee)) {
    jittedCode jitted = jitCode(callee);
    mov(X16, IM((uinteger)jitted));
  }
  else {
    loadConstant(jit, key, X16);

    // pick up the pointer to the method
    ldr(X16, OF(X16, OffsetOf(LblRecord, mtd)));
    codeLblPo noMtd = newLabel(ctx);
    cbz(X16, noMtd);
    // Pick up the jit code itself
    ldr(X16, OF(X16, OffsetOf(MethodRec, jit.code)));
    codeLblPo runMtd = newLabel(ctx);
    cbnz(X16, runMtd);

    bind(noMtd);
    bailOut(state, pc, undefinedCode);

    bind(runMtd);
  }

  pushFrme(state, pc, argPnt);
  blr(X16);
  dropArguments(state, nextPc);
}

// Resolve the callee and tail-branch to it. Arguments must already be staged (see overrideArguments).
void emitTCallInvoke(codeGenPo state, int32 pc, int32 key, int32 tgtOff) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  labelPo tgt = C_LBL(getConstant(key));
  methodPo callee = labelMtd(tgt);

  if (callee != Null && hasJitCode(callee)) {
    mov(X16, IM((uinteger)jitCode(callee)));
  }
  else {
    loadConstant(jit, key, X16);

    // pick up the pointer to the method
    ldr(X16, OF(X16, OffsetOf(LblRecord, mtd)));
    codeLblPo noMtd = newLabel(ctx);
    cbz(X16, noMtd);
    // Pick up the jit code itself
    ldr(X16, OF(X16, OffsetOf(MethodRec, jit.code)));
    codeLblPo runMtd = newLabel(ctx);
    cbnz(X16, runMtd);

    bind(noMtd);
    bailOut(state, pc, undefinedCode);

    bind(runMtd);
  }
  adjustAG(state, pc, tgtOff);
  str(AG, OF(STK, OffsetOf(StackRecord,args)));

  // Pick up the old return address
  ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
  br(X16);
}

// Resolve the closure's method and branch to it. lamReg must hold the closure pointer and
// arguments must already be staged (see loadLambdaArguments).
void emitOCallInvoke(codeGenPo state, int32 pc, int32 nextPc, armReg lamReg, int32 argPnt) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
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
  dropArguments(state, nextPc);
}

// Resolve the closure's method and tail-branch to it. lamReg must hold the closure pointer and
// arguments must already be staged (see overrideArguments).
void emitTOCallInvoke(codeGenPo state, int32 pc, armReg lamReg, int32 tgtOff) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  ldr(lamReg, OF(lamReg, OffsetOf(ClosureRecord, lbl))); // Pick up the label
  // pick up the pointer to the method
  ldr(lamReg, OF(lamReg, OffsetOf(LblRecord, mtd)));
  codeLblPo haveMtd = newLabel(ctx);
  codeLblPo noMtd = newLabel(ctx);
  cbz(lamReg, noMtd);
  assert(lamReg!=X17);
  ldr(X17, OF(lamReg, OffsetOf(MethodRec, jit.code)));
  cbnz(X17, haveMtd);
  bind(noMtd);
  bailOut(state, pc, undefinedCode);

  bind(haveMtd);
  adjustAG(state, pc, tgtOff);
  str(AG, OF(STK, OffsetOf(StackRecord,args)));
  // Pick up the old return address
  ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
  br(X17);
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
    if (lineDebugging == noTracing) {
      armReg rg = nxtAvailReg(registerArgs);
      if (rg != XZR) {
        var->src = RG(rg);
        var->stashed = False;
        registerArgs = dropReg(registerArgs, rg);
        reserveReg(state->jit, rg);
        regArgCnt++;
        continue;
      }
    }
    var->stkOff = ax;
    var->stashed = True;
    var->src = varFlex(var->stkOff);
  }

#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    showRegisterMap(logFile, registerArgs);
  }
#endif
}

int32 loadArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity) {
  registerMap argRegs = defaultArgRegs();
  ArgSpec operands[arity];
  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call
  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    if (lineDebugging == noTracing) {
      armReg ax = nxtAvailReg(argRegs);
      if (ax != XZR) {
        argRegs = dropReg(argRegs, ax);
        operands[ix] = argSpec(argSrc, RG(ax));
        continue;
      }
    }
    int32 argSlot = currVarLimit - arity + ix;
    operands[ix] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(state->jit, operands, arity, &tmpMap);
  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit - arity; // return how must space is needed to preserve current locals and arguments.
}

// arity is the lambda's full arity (the free value plus its numArgs proper arguments); argBase
// points at the numArgs proper arguments, and the free value is read from lamReg and placed
// ahead of them (slot 0 / X0), so both flow through the same register-vs-stack-slot handling.
int32 loadLambdaArguments(codeGenPo state, int32 livePc, int32 argBase, int32 arity, armReg lamReg) {
  registerMap argRegs = defaultArgRegs();
  ArgSpec operands[arity];
  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call
  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = ix == 0 ? OF(lamReg, OffsetOf(ClosureRecord, free)) : sourceOperandFlex(state, argBase, ix - 1);
    if (lineDebugging == noTracing) {
      armReg ax = nxtAvailReg(argRegs);
      if (ax != XZR) {
        argRegs = dropReg(argRegs, ax);
        operands[ix] = argSpec(argSrc, RG(ax));
        continue;
      }
    }
    int32 argSlot = currVarLimit - arity + ix;
    operands[ix] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(state->jit, operands, arity, &tmpMap);
  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit - arity; // return how must space is needed to preserve current locals and arguments.
}

int32 loadEscapeArguments(codeGenPo state, int32 pc, int32 livePc, int32 arity, int32 argBase) {
  ArgSpec operands[arity + 1];

  operands[0] = argSpec(RG(PR), RG(X0));
  registerMap argRegs = dropReg(defaultArgRegs(), X0);

  int32 currVarLimit = stashLiveLocals(state, livePc, True); // save vars that will be live after the call

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp argSrc = sourceOperandFlex(state, argBase, ix);
    armReg ax = nxtAvailReg(argRegs);
    int32 argSlot = currVarLimit - arity + ix;
    if (ax != XZR) {
      argRegs = dropReg(argRegs, ax);
      operands[ix + 1] = argSpec(argSrc, RG(ax));
    }
    else {
      operands[ix + 1] = argSpec(argSrc, OF(AG,argSlot*pointerSize));
    }
  }
  registerMap tmpMap = fixedRegSet(X16);
  shuffleVars(state->jit, operands, arity + 1, &tmpMap);

  voidOutFrameLocals(state, livePc, currVarLimit); // void out gaps in the locals map
  return currVarLimit;                             // return how must space is needed to preserve current locals.
}

int32 overrideArguments(codeGenPo state, registerMap argRegs, int32 argPc, int32 arity, armReg tmpReg) {
  ArgSpec operands[arity];

  int32 callerArity = mtdArity(state->jit->mtd);
  int32 tgtOff = callerArity - arity;

  for (int32 ix = 0; ix < arity; ix++) {
    FlexOp arg = sourceOperandFlex(state, argPc, ix);
    if (lineDebugging == noTracing) {
      armReg rx = nxtAvailReg(argRegs);
      if (rx != XZR) {
        argRegs = dropReg(argRegs, rx);
        operands[ix] = argSpec(arg, RG(rx));
        continue;
      }
    }
    int32 argSlot = tgtOff + ix;
    operands[ix] = argSpec(arg, OF(AG,argSlot*pointerSize));
  }
  registerMap tmpMap = fixedRegSet(tmpReg);
  shuffleVars(state->jit, operands, arity, &tmpMap);
  return tgtOff;
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
  }
  else if (delta < 0) {
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
      FlexOp rg = RG(findARegister(state, pc));
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
      FlexOp rg = RG(findARegister(state, pc));
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
    "str X13, [x14,#%c[argsOff]]\n"
    "str x29, [x14,#%c[fpOff]]\n"
    "ldp x17,x19, [sp], #16\n"
    "ldp x12,x13, [sp], #16\n"
    "mov %w[ret], w11\n"
    "mov %[val], x10\n"
    "ldp x10,x11, [sp], #16\n"
    "ldp x8,x9, [sp], #16\n"
    "ldp x29,x30, [sp], #16\n"
    : [ret] "=r"(ret), [val] "=r" (val)
    : [process]"r"(P), [stk] "r"(stk), [code] "r"(code), [ag] "m"(stk->args), [argcount] "r" (argCount),
    [constants] "r"(constAnts),[fp] "m"(stk->fp),
    [argsOff] "i" (OffsetOf(StackRecord, args)), [fpOff] "i" (OffsetOf(StackRecord, fp))
    : "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10","x11", "x12", "x13", "x14", "x15", "x16",
    "memory");

  P->stk->sp = exitSP;
  return (ValueReturn){.value = val, .status = ret};
}
