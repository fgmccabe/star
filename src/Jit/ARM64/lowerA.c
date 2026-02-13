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
#include "errorCodes.h"
#include "abort.h"
#include "arithP.h"
#include "debugP.h"
#include "formioP.h"

/* Lower Star VM code to Arm64 code */

/*
* X0-X7 = argument registers & scratch registers
* X0/X1 = return registers
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

static retCode jitBlock(blockPo block, codeGenPo state, insPo code, int32 from, int32 endPc);
static void pshFrame(blockPo block, armReg mtdRg);
static armReg allocSmallStruct(blockPo block, clssPo class, integer amnt);
static retCode handleBreakTable(blockPo block, insPo code, int32 pc, int32 count);
static retCode testResult(codeGenPo state, int32 pc, blockPo tgtBlock);
static armReg mkFloat(blockPo block);
static void populateLocals(LocalVar *locals, int32 numSlots, analysisPo analysis);
static armReg popValue(codeGenPo state, int32 pc);
static void pushRegister(codeGenPo state, armReg rg, int32 pc);
static void pushConstant(codeGenPo state, int32 pc, int32 constant);
static void pushBlank(codeGenPo state, int32 pc);
static void dumpState(codeGenPo state);
static void invokeInstrinsic(codeGenPo state, runtimeFn fn, int32 pc, int32 arity, ...);
static void invokeEscape(codeGenPo state, runtimeFn fn, int32 arity);
static void loadArguments(codeGenPo state, int32 arity, int32 pc);
static void dropArguments(codeGenPo state, int32 arity, int32 pc);
static varDescPo findPhiVariable(codeGenPo state, int32 pc);
static void storeToPhiVar(codeGenPo state, armReg val, varDescPo phiVar);

retCode jitInstructionsA(jitCompPo jit, methodPo mtd, char *errMsg, integer msgLen) {
  AnalysisRecord analysis;
  if (analyseMethod(mtd, &analysis) == Ok) {
    showAnalysis(logFile, &analysis);
  }

  int32 numSlots = slotCount(&analysis);
  LocalVar locals[numSlots];

  populateLocals(locals, numSlots, &analysis);

  CodeGenState state = {.analysis = &analysis, .locals = locals, .numLocals = numSlots, .jit = jit};

#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method %L\n", mtd);
    reinstallMsgProc('X', showStackSlot);
  }
#endif

  JitBlock block = {
    .startPc = 0, .endPc = codeSize(mtd),
    .breakLbl = Null, .loopLbl = Null,
    .parent = Null,
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

  populateLocals(locals, numSlots, &analysis);

  CodeGenState state = {.analysis = &analysis, .locals = locals, .numLocals = numSlots, .jit = jit};

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
#ifdef TRACEJIT
    if (traceJit >= generalTracing) {
      disass(logFile, Null, jit->mtd, &code[pc]);
      outMsg(logFile, "\n%_");
    }
    if (traceJit >= detailedTracing) {
      dRegisterMap(jit->freeRegs);
      dumpState(state);
    }
#endif
    switch (code[pc].op) {
      case Halt: {
        // Stop execution
        armReg a1 = popValue(state, pc);
        invokeInstrinsic(state, (runtimeFn) star_exit, pc, 1, RG(a1));
        releaseReg(jit, a1);
        continue;
      }
      case Abort: {
        // abort with message
        armReg val = popValue(state, pc);
        armReg loc = findFreeReg(jit);
        loadConstant(jit, code[pc].fst, loc);
        invokeInstrinsic(state, (runtimeFn) abort_star, pc, 3, RG(PR), RG(loc), RG(val));
        releaseReg(jit, val);
        releaseReg(jit, loc);
        continue;
      }
      case Call: {
        int32 key = code[pc].fst;
        int32 arity = lblArity(C_LBL(getConstant(key)));
        loadArguments(state, arity, pc);
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
        dropArguments(state, arity, pc);
        continue;
      }
      case XCall: {
        int32 key = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        int32 arity = lblArity(C_LBL(getConstant(key)));

        loadArguments(state, arity, pc);
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
        blr(X16);
        dropArguments(state, arity, pc);
        testResult(state, pc, breakBlock(block, code, tgt, Valof));
        continue;
      }

      case OCall: {
        int32 arity = code[pc].fst;
        armReg clos = popValue(state, pc); // Pick up the closure
        ldr(X17, OF(clos, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        armReg freeReg = findFreeReg(jit);
        ldr(freeReg, OF(clos, OffsetOf(ClosureRecord, free))); // Pick up the free term
        releaseReg(jit, clos);
        pushRegister(state, freeReg, pc);
        loadArguments(state, arity + 1, pc);
        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArguments(state, arity, pc);
        continue;
      }
      case XOCall: {
        int32 arity = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;

        armReg clos = popValue(state, pc); // Pick up the closure
        ldr(X17, OF(clos, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        armReg freeReg = findFreeReg(jit);
        ldr(freeReg, OF(clos, OffsetOf(ClosureRecord, free))); // Pick up the free term
        releaseReg(jit, clos);
        pushRegister(state, freeReg, pc);
        loadArguments(state, arity + 1, pc);

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bailOut(jit, undefinedCode);

        bind(haveMtd);
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        blr(X16);
        dropArguments(state, arity, pc);
        testResult(state, pc, breakBlock(block, code, tgt, Valof));
        continue;
      }
      case TCall: {
        // TCall <prog>
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
        frameOverride(block, arity);
        str(AG, OF(STK, OffsetOf(StackRecord,args)));

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));
        ldr(LR, OF(FP, OffsetOf(StackFrame, link)));
        br(X16);

        return ret;
      }
      case TOCall: {
        int32 arity = code[pc].fst;

        // Tail Call closure
        armReg clos = popValue(state, pc); // Pick up the closure
        ldr(X17, OF(clos, OffsetOf(ClosureRecord, lbl))); // Pick up the label
        // pick up the pointer to the method
        ldr(X17, OF(X17, OffsetOf(LblRecord, mtd)));
        // Update current frame
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program

        armReg freeReg = findFreeReg(jit);
        ldr(freeReg, OF(clos, OffsetOf(ClosureRecord, free))); // Pick up the free term
        releaseReg(jit, clos);
        pushRegister(state, freeReg, pc); // The free term is the first argument

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
        loadArguments(state, arity, pc);
        invokeEscape(state, (runtimeFn) escapeFun(esc), arity);
        dropArguments(state, arity, pc);
        continue;
      }
      case XEscape: {
        int32 escNo = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;

        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);
        loadArguments(state, arity, pc);
        invokeEscape(state, (runtimeFn) escapeFun(esc), arity);
        dropArguments(state, arity, pc);
        testResult(state, pc, breakBlock(block, code, tgt, Valof));
        continue;
      }
      case Entry: {
        // locals definition
        continue;
      }
      case Ret: {
        armReg vl = popValue(state, pc);
        // Put return value at top of args on stack
        storeVarble(jit, vl, mtdArity(jit->mtd) - 1);
        releaseReg(jit, vl);

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
        mov(X0, IM(Normal));
        br(X16);

        return ret;
      }
      case XRet: {
        // exception return
        armReg vl = popValue(state, pc);

        // Put exception value at top of args on stack
        storeVarble(jit, vl, mtdArity(jit->mtd) - 1);
        releaseReg(jit, vl);

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
        mov(X0, IM(Abnormal));
        br(X16);

        return ret;
      }
      case Valof: {
        // vlof block of instructions
        int32 blockLen = code[pc].alt;
        codeLblPo brkLbl = newLabel(ctx);

        JitBlock subBlock = {
          .startPc = pc,
          .endPc = pc + blockLen + 1,
          .breakLbl = brkLbl,
          .loopLbl = here(),
          .parent = block,
          .phiVar = findPhiVariable(state, pc)
        };

        ret = jitBlock(&subBlock, state, code, pc + 1, pc + blockLen + 1);
        pc += blockLen; // Skip over the block
        bind(brkLbl);

        continue;
      }
      case Block: {
        // block of instructions
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
        bind(brkLbl);

        continue;
      }
      case Break: {
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);
        return breakOut(block, tgtBlock);
      }
      case Result: {
        // return value out of block
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = breakBlock(block, code, tgt, Valof);
        blockPo parent = tgtBlock->parent;
        varDescPo phiVar = parent->phiVar;

        armReg val = popValue(state, pc);
        storeToPhiVar(state, val, phiVar);

        return breakOut(block, tgtBlock);
      }
      case Loop: {
        // jump back to start of block
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);
        codeLblPo loop = loopLabel(tgtBlock);
        assert(loop != Null);
        b(loop);
        return ret;
      }
      case Drop: {
        // nothing to do anymore
        continue;
      }
      case Rot: {
        // Nothing to do here
        // Pull up nth element of stack
        continue;
      }
      case Rst: {
        // reset stack height to a fixed height
        continue;
      }
      case Fiber: {
        armReg lamReg = popValue(state, pc);
        invokeInstrinsic(state, (runtimeFn) newStack, pc, 3, RG(PR), IM(True), RG(lamReg));
        pushRegister(state, X0, pc);
        releaseReg(jit, lamReg);
        continue;
      }
      case Suspend: {
        armReg stk = popValue(state, pc);
        armReg evt = popValue(state, pc);
        armReg tmp = findFreeReg(jit);
        codeLblPo rtn = newLabel(ctx);
        adr(tmp, rtn);
        str(tmp, OF(STK, OffsetOf(StackRecord, pc)));
        invokeInstrinsic(state, (runtimeFn) detachStack, pc, 3, RG(PR), RG(stk), RG(evt));
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        releaseReg(jit, tmp);
        releaseReg(jit, evt);
        releaseReg(jit, stk);
        pushBlank(state, pc);
        continue;
      }
      case Resume: {
        armReg stk = popValue(state, pc);
        armReg evt = popValue(state, pc);
        codeLblPo rtn = newLabel(ctx);
        adr(X16, rtn);
        str(X16, OF(STK, OffsetOf(StackRecord, pc)));
        invokeInstrinsic(state, (runtimeFn) attachStack, pc, 3, RG(PR), RG(stk), RG(evt));
        ldr(X16, OF(STK, OffsetOf(StackRecord, pc)));
        br(X16);
        bind(rtn);
        releaseReg(jit, stk);
        releaseReg(jit, evt);
        pushBlank(state, pc);
        continue;
      }
      case Retire: {
        // Similar to suspend, except that we trash the suspending stack
        armReg stk = popValue(state, pc);
        armReg evt = popValue(state, pc);
        invokeInstrinsic(state, (runtimeFn) detachDropStack(), pc, 3, RG(PR), RG(stk), RG(evt));
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
        pushConstant(state, pc, voidIndex);
        continue;
      }
      case LdC: {
        // load literal from constant pool
        pushConstant(state, pc, code[pc].fst);
        continue;
      }
      case Ld: {
        // load stack from args[xx]
        if (!haveFreeReg(jit))
          spillStack(stack, jit);
        int32 varNo = code[pc].fst;
        pushValue(stack, (LocalEntry) {
          .
          kind = isLocal,
          .
          stkOff = varNo,
          .
          inited = True
        }
        )
        ;

        continue;
      }
      case St: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        armReg vl = popValue(stack, jit);
        storeVarble(jit, vl, lclNo);
        setLocal(stack, lclNo, (LocalEntry) {
          .
          kind = isLocal,
          .
          stkOff = lclNo,
          .
          inited = True
        }
        )
        ;
        releaseReg(jit, vl);
        continue;
      }
      case StV: {
        // clear a local to void
        int32 lclNo = code[pc].fst;
        armReg vd = findFreeReg(jit);
        loadConstant(jit, voidIndex, vd);
        storeVarble(jit, vd, lclNo);
        setLocal(stack, lclNo, (LocalEntry) {
          .
          kind = isLocal,
          .
          stkOff = lclNo,
          .
          inited = True
        }
        )
        ;
        releaseReg(jit, vd);
        continue;
      }
      case Tee: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        armReg vl = popValue(stack, jit);
        storeVarble(jit, vl, lclNo);
        setLocal(stack, lclNo, (LocalEntry) {
          .
          kind = isLocal,
          .
          stkOff = lclNo,
          .
          inited = True
        }
        )
        ;
        pushRegister(stack, vl);
        continue;
      }
      case LdG: {
        // load a global variable
        spillStack(stack, jit); // We spill because we may have to call the global function
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
        pshFrame(block, X17);

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit.code)));

        codeLblPo returnPc = newLabel(ctx);
        adr(LR, returnPc);
        br(X16);

        pushBlank(stack); // This one is from the call to load global

        bind(haveContent);
        storeStack(jit, content, stack->vTop);
        bind(returnPc);
        releaseReg(jit, glb);
        releaseReg(jit, content);
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
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);
        armReg sng = popValue(stack, jit);

        ldr(sng, OF(sng, OffsetOf(SingleRecord, content)));
        codeLblPo skip = newLabel(ctx);
        cbnz(sng, skip);
        tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
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

        ands(tmp, vl, IM(0b11));
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
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);
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
          tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
          bne(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgt);
        continue;
      }
      case CLit: {
        // T,lit --> test for a literal value, break if not
        int32 key = code[pc].fst;
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);

        armReg vl = popValue(stack, jit);

        spillStack(stack, jit);

        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) sameTerm, 2, RG(vl), OF(CO, key*pointerSize));
        unstash(jit);
        tst(X0, RG(X0));

        valueStackPo tgtStack = &tgtBlock->stack;
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(tgtStack, jit, tgtBlock->exitHeight);
          tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
          beq(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgt);

        releaseReg(jit, vl);
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
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);
        armReg vl = popValue(stack, jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, trueIndex, tr);
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        setStackDepth(stack, jit, tgtBlock->exitHeight);
        tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
        ret = breakOutEq(block, code, tgt);
        continue;
      }
      case IfNot: {
        // break if false
        int32 tgt = pc + code[pc].alt + 1;
        blockPo tgtBlock = breakBlock(block, code, tgt, Block);
        armReg vl = popValue(stack, jit);
        armReg tr = findFreeReg(jit);
        loadConstant(jit, trueIndex, tr);
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        setStackDepth(stack, jit, tgtBlock->exitHeight);
        tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
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
        tryRet(handleBreakTable(block, code, pc + 1, tableSize));
        return ret;
      }
      case Case: {
        // T --> T, case <Max>
        int32 tableSize = code[pc].fst;
        armReg vl = popValue(stack, jit);
        armReg ix = findFreeReg(jit);
        spillStack(stack, jit);
        stash(block);
        callIntrinsic(ctx, criticalRegs(), (runtimeFn) hashTerm, 1, RG(vl));
        mov(ix, RG(X0));
        releaseReg(jit, vl);
        unstash(jit);
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(tableSize));
        armReg quotient = findFreeReg(jit);
        udiv(quotient, ix, divisor);
        msub(ix, divisor, quotient, ix);
        releaseReg(jit, divisor);
        armReg tgt = findFreeReg(jit);
        codeLblPo jmpTbl = newLabel(ctx);
        adr(tgt, jmpTbl);
        add(tgt, tgt, LS(ix, 2));
        br(tgt);
        releaseReg(jit, tgt);
        releaseReg(jit, quotient);
        releaseReg(jit, ix);
        bind(jmpTbl);
        tryRet(handleBreakTable(block, code, pc + 1, tableSize));
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
        tryRet(handleBreakTable(block, code, pc + 1, tableSize));
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

        blockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1, Valof);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(&tgtBlock->parent->stack, jit, tgtBlock->exitHeight - 1);
          pushConstant(jit, &tgtBlock->parent->stack, divZeroIndex);
          //tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        sdiv(a1, a1, a2);
        mkIntVal(jit, a1);
        // setStackDepth(stack, jit, tgtBlock->exitHeight - 1);
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

        blockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1, Valof);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(&tgtBlock->parent->stack, jit, tgtBlock->exitHeight - 1);
          pushConstant(jit, &tgtBlock->parent->stack, divZeroIndex);
          //tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);

        armReg quotient = findFreeReg(jit);
        sdiv(quotient, a1, divisor);
        msub(a1, divisor, quotient, a1);

        mkIntVal(jit, a1);
        // setStackDepth(stack, jit, tgtBlock->exitHeight - 1);
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
        armReg reslt = mkFloat(block); // We create it first
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);

        fadd(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));

        pushRegister(stack, reslt);
        continue;
      }
      case FSub: {
        // L R --> L-R
        armReg reslt = mkFloat(block);
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);

        fsub(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushRegister(stack, reslt);
        continue;
      }
      case FMul: {
        // L R --> L*R
        armReg reslt = mkFloat(block);
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);

        fmul(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushRegister(stack, reslt);
        continue;
      }
      case FDiv: {
        // L R --> L/R
        blockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1, Valof);
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);

        codeLblPo skip = newLabel(ctx);

        fmov(FP(F2), RG(XZR));
        fcmp(F1, F2);

        bne(skip);

        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(&tgtBlock->parent->stack, jit, tgtBlock->exitHeight - 1);
          pushConstant(jit, &tgtBlock->parent->stack, divZeroIndex);
          //tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        stpf(F0, F1, PRX(SP,-16));
        armReg reslt = mkFloat(block);
        ldpf(F0, F1, PSX(SP,16));
        fdiv(F0, F0, F1);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushRegister(stack, reslt);
        continue;
      }
      case FMod: {
        // L R --> L%R
        blockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1, Valof);
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);
        releaseReg(jit, a1);
        releaseReg(jit, a2);

        codeLblPo skip = newLabel(ctx);

        fmov(FP(F2), RG(XZR));
        fcmp(F1, F2);
        bne(skip);

        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          setStackDepth(&tgtBlock->parent->stack, jit, tgtBlock->exitHeight - 1);
          pushConstant(jit, &tgtBlock->parent->stack, divZeroIndex);
          //tryRet(propagateStack(jit, stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        bind(skip);
        stpf(F0, F1, PRX(SP,-16));
        armReg reslt = mkFloat(block);
        ldpf(F0, F1, PSX(SP,16));
        fdiv(F2, F0, F1);
        fmsub(F2, F2, F1, F0);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushRegister(stack, reslt);
        continue;
      }
      case FAbs: {
        // L --> abs(L)
        armReg reslt = mkFloat(block);
        armReg a1 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        releaseReg(jit, a1);

        fabs(F0, F0);
        fstr(F0, OF(reslt, OffsetOf(FloatRecord, dx)));
        pushRegister(stack, reslt);
        continue;
      }
      case FEq: {
        // L R --> L==
        armReg a1 = popValue(stack, jit);
        armReg a2 = popValue(stack, jit);
        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);

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

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);

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

        getFltVal(jit, a1, F0);
        getFltVal(jit, a2, F1);

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
        // enter the line
        if (lineDebugging) {
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
            case TOCall: {
              armReg lbl = topValue(stack, jit);
              ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) tocallDebug, 4, RG(PR), IM(code[npc].op), RG(loc),
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
        }
        continue;
      }
      case Line: {
        if (lineDebugging) {
          int32 stackLevel = trueStackDepth(stack);

#ifdef TRACEJIT
          if (traceJit >= detailedTracing)
            outMsg(logFile, "True stack depth: %d\n%_", stackLevel);
#endif

          spillStack(stack, jit);
          int32 locKey = code[pc].fst;
          armReg loc = findFreeReg(jit);
          loadConstant(jit, locKey, loc);

          stash(block);
          ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) lineDebug, 2, RG(PR), RG(loc));
          unstash(jit);
          releaseReg(jit, loc);
        }
        continue;
      }
      case Bind: {
        if (lineDebugging) {
          spillStack(stack, jit);
          int32 varKey = code[pc].fst;
          armReg var = findFreeReg(jit);
          loadConstant(jit, varKey, var);

          stash(block);
          ret = callIntrinsic(ctx, criticalRegs(), (runtimeFn) bindDebug, 3, RG(PR), RG(var), IM(code[pc].alt));
          unstash(jit);
          releaseReg(jit, var);
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

  // We only come here if the block does not have a breaking
  setStackDepth(stack, jit, block->exitHeight);
  if (block->parent != Null)
    tryRet(propagateStack(jit, stack, &block->parent->stack, block->exitHeight));

  return ret;
}

armReg allocSmallStruct(blockPo block, clssPo class, integer amnt) {
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

armReg ¯mkFloat(blockPo block) {
  valueStackPo stack = &block->stack;
  jitCompPo jit = block->jit;

  spillStack(stack, jit);
  return allocSmallStruct(block, floatClass, FloatCellCount);
}

void pshFrame(blockPo block, armReg mtdRg) {
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

retCode handleBreakTable(blockPo block, insPo code, int32 pc, int32 count) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  for (int ix = 0; ix < count; ix++, pc++) {
    check(code[pc].op==Break, "Expecting a Break instruction");
    blockPo tgtBlock = breakBlock(block, code, pc + code[pc].alt + 1, Block);
    setStackDepth(&tgtBlock->parent->stack, jit, tgtBlock->exitHeight);
    tryRet(propagateStack(jit, &block->stack, &tgtBlock->parent->stack, tgtBlock->exitHeight));
    codeLblPo lbl = breakLabel(tgtBlock);
    b(lbl);
  }
  return Ok;
}

retCode testResult(blockPo block, blockPo tgtBlock) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo skip = newLabel(ctx);
  cmp(X0, IM(Normal));
  beq(skip);
  armReg er = topValue(&block->stack, jit);
  valueStackPo tgtStack = &tgtBlock->parent->stack;
  valueStackPo srcStack = &block->stack;
  propagateStack(jit, srcStack, tgtStack, tgtBlock->exitHeight - 1);
  forcePush(jit, tgtStack, er);
  retCode ret = breakOut(block, tgtBlock);
  bind(skip);
  return ret;
}
