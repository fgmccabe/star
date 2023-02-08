/*
 * Run-time evaluation for STAR programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, a JIT
 * process will generate native instructions from STAR instructions.
 */

#include "config.h"
#include <globals.h>
#include <turm.h>
#include <arithP.h>
#include "engineP.h"
#include "debugP.h"
#include <math.h>
#include "thunk.h"
#include "continuation.h"
#include "cellP.h"
#include "jit.h"
#include "thunkP.h"
#include "continuationP.h"

#define collectI32(pc) (hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<(unsigned)16)|lo32))
#define collectOff(pc) (hi32 = collectI32(pc), (pc)+(signed)hi32)

#define checkAlloc(Count) STMT_WRAP({\
  if (reserveSpace(H, Count) != Ok) {\
    saveRegisters();\
    retCode ret = gcCollect(H, Count);\
    if (ret != Ok)\
      return ret;\
    restoreRegisters();   \
    check(reserveSpace(H,Count)==Ok,"could not reserve space");\
  }\
})

#define pop() (*SP++)
#define top() (*SP)
#define peek(Ix) (SP[Ix])
#define push(T) STMT_WRAP({*--SP=(termPo)(T);})
#define local(off) (CSP[-off])
#define arg(off) (CSP[off])
#define stackRoom(amnt) ((SP-(amnt)) > ((ptrPo)(FP+1)))
#define saveRegisters() STMT_WRAP({ FP->pc = PC; STK->sp = SP; STK->fp = FP; P->stk = STK;})
#define restoreRegisters() STMT_WRAP({ STK = P->stk; FP = STK->fp; PC = FP->pc;  CSP=FP->csp; SP=STK->sp; LITS=codeLits(FP->prog);})
#define pushFrme(mtd) STMT_WRAP({FP++; FP->prog=mtd; PC = FP->pc = entryPoint(mtd); FP->csp=CSP = SP;})
#define prevFrme() STMT_WRAP({assert(FP >= baseFrame(STK) && ((ptrPo) FP + 1) < SP); FP--; CSP = FP->csp; PC = FP->pc; assert(SP <= CSP);LITS = codeLits(FP->prog);})
#define bail() STMT_WRAP({\
  saveRegisters();\
  stackTrace(P, logFile, STK,displayDepth,showLocalVars);\
  return Error;\
  })

#define stackGrow(Amnt, SaveArity) STMT_WRAP({\
  saveRegisters();\
  P->stk = glueOnStack(H, STK, maximum(stackHwm(STK),(STK->sze * 3) / 2 + (Amnt)),SaveArity); \
  restoreRegisters();\
  if (!stackRoom(Amnt)) {\
    logMsg(logFile, "cannot extend stack sufficiently");\
    bail();\
  }\
  })

/*
 * Execute program on a given process/thread structure
 */
retCode run(processPo P) {
  heapPo H = P->heap;
  stackPo STK = P->stk;
  framePo FP = STK->fp;
  register insPo PC = FP->pc;    /* Program counter */
  register normalPo LITS = codeLits(FP->prog); /* pool of literals */
  register ptrPo SP = STK->sp;         /* Current 'top' of stack (grows down) */
  register ptrPo CSP = FP->csp;

  register uint32 hi32, lo32;    /* Temporary registers */

  currentProcess = P;

#ifdef TRACEMEM
  if (traceMemory)
    verifyProc(P, H);
#endif

  for (;;) {
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    if (insDebugging) {
      saveRegisters();
      insDebug(P);
      restoreRegisters();
    }
#endif

    switch ((OpCode) (*PC++)) {
      case Halt: {
        int32 exitCode = collectI32(PC);

        if (insDebugging || lineDebugging) {
          logMsg(logFile, "Halt %d", exitCode);
        }
        return (retCode) exitCode;
      }
      case Nop: {
        continue;
      }
      case Abort: {
        termPo lc = pop();
        termPo msg = pop();

        logMsg(logFile, "Abort %T at %L", msg, lc);
        saveRegisters();
        verifyProc(P, H);
        stackTrace(P, logFile, P->stk, displayDepth, showArguments);

        return Error;
      }

      case Call: {
        labelPo nProg = C_LBL(nthElem(LITS, collectI32(PC)));
        methodPo mtd = labelCode(nProg);    // Which program do we want?

        if (mtd == Null) {
          logMsg(logFile, "label %L not defined", nProg);
          bail();
        }

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          int root = gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);
          assert(stackRoom(stackDelta(mtd) + STACKFRAME_SIZE));
#ifdef TRACESTACK
          if (traceStack)
            verifyStack(STK, H);
#endif
        }

        assert(isPcOfMtd(FP->prog, PC));
        FP->pc = PC;

        if (hasJit(mtd)) {
#ifdef TRACEJIT
          if (traceJit) {
            logMsg(logFile, "entering jitted code %T", mtd);
          }
#endif
          saveRegisters();
          termPo res = invokeJitMethod(mtd, H, STK);
          restoreRegisters();
          push(res);
        } else {
          pushFrme(mtd);
          LITS = codeLits(mtd);
          incEntryCount(mtd);              // Increment number of times program called
        }
        continue;
      }

      case OCall: {        /* Call tos a1 .. an -->   */
        int arity = collectI32(PC);
        normalPo obj = C_NORMAL(pop());
        labelPo oLbl = objLabel(termLbl(obj), arity);

        if (oLbl == Null) {
          logMsg(logFile, "label %s/%d not defined", labelName(termLbl(obj)), arity);
          bail();
        }

        methodPo mtd = labelCode(oLbl);       /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", oLbl);
          bail();
        }

        push(nthElem(obj, 0));                     // Put the free term back on the stack

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          int root = gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);

#ifdef TRACESTACK
          if (traceStack)
            verifyStack(STK, H);
#endif
        }

        assert(isPcOfMtd(FP->prog, PC));
        FP->pc = PC;
        pushFrme(mtd);
        LITS = codeLits(mtd);
        incEntryCount(mtd);              // Increment number of times program called
        continue;
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */

#ifdef TRACEEXEC
        recordEscape(escNo);
#endif

        escapePo esc = getEscape(escNo);
        saveRegisters();
        assert(H->topRoot == 0);
        ReturnStatus ret;

        switch (esc->arity) {
          case 0:
            ret = ((escFun0) (esc->fun))(H);
            break;
          case 1:
            ret = ((escFun1) (esc->fun))(H, top());
            break;
          case 2:
            ret = ((escFun2) (esc->fun))(H, top(), peek(1));
            break;
          case 3:
            ret = ((escFun3) (esc->fun))(H, top(), peek(1), peek(2));
            break;
          case 4:
            ret = ((escFun4) (esc->fun))(H, top(), peek(1), peek(2), peek(3));
            break;
          case 5:
            ret = ((escFun5) (esc->fun))(H, top(), peek(1), peek(2), peek(3), peek(4));
            break;
          case 6:
            ret = ((escFun6) (esc->fun))(H, top(), peek(1), peek(2), peek(3), peek(4), peek(5));
            break;
          case 7:
            ret = ((escFun7) (esc->fun))(H, top(), peek(1), peek(2), peek(3), peek(4), peek(5), peek(6));
            break;
          case 8:
            ret = ((escFun8) (esc->fun))(H, top(), peek(1), peek(2), peek(3), peek(4), peek(5), peek(6), peek(7));
            break;
          default:
            logMsg(logFile, "invalid arity for escape %s", escapeName(esc));
            bail();
        }
        restoreRegisters();
        assert(H->topRoot == 0);

        switch (ret.ret) {
          case Ok:
            SP += esc->arity;
            if (ret.result != Null)
              push(ret.result);
            continue;
          case Error:
            SP += esc->arity;
            if (ret.result != Null)
              push(ret.result);
            else
              push(unitEnum);
            //PC = exit; Fix me, pass in a continuation to escapes that can fail
            continue;
          case Fail:
            bail();
          case Switch:
            continue;
          default:
            continue;
        }
      }

      case TCall: {       /* Tail call of explicit program */
        termPo nProg = nthElem(LITS, collectI32(PC));
        labelPo lbl = C_LBL(nProg);
        integer arity = labelArity(lbl);

        methodPo mtd = labelCode(lbl);
        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lbl);
          bail();
        }

        if (!stackRoom(stackDelta(mtd))) {
          int root = gcAddRoot(H, (ptrPo) &mtd);

          stackPo prevStack = STK;

          gcAddRoot(H, (ptrPo) &prevStack);

          saveRegisters();
          STK = P->stk = glueOnStack(H, STK, (STK->sze * 3) / 2 + stackDelta(mtd), arity);

          SP = CSP = STK->sp;
          FP = STK->fp;
          pushFrme(mtd);

          // drop old frame on old stack
          framePo prevFrame = prevStack->fp;
          prevStack->sp = stackArg(prevStack, prevFrame, argCount(prevFrame->prog));
          prevStack->fp--;
          gcReleaseRoot(H, root);

#ifdef TRACEEXEC
          saveRegisters();
          verifyStack(STK, H);
#endif
        } else {
          // Overwrite existing arguments and locals
          ptrPo tgt = &arg(argCount(FP->prog));
          ptrPo src = SP + arity;                  /* base of argument vector */

          for (int ix = 0; ix < arity; ix++)
            *--tgt = *--src;    /* copy the argument vector */
          FP->csp = CSP = SP = tgt;
          FP->pc = PC = entryPoint(mtd);
          FP->prog = mtd;
        }

        incEntryCount(mtd);              // Increment number of times program called
        LITS = codeLits(mtd);
        continue;       /* Were done */
      }

      case TOCall: {       /* Tail call */
        int arity = collectI32(PC);
        normalPo obj = C_NORMAL(pop());

        push(nthElem(obj, 0));                     // Put the free term back on the stack
        labelPo lbl = objLabel(termLbl(obj), arity);
        if (lbl == Null) {
          logMsg(logFile, "label %s/%d not defined", labelName(termLbl(obj)), arity);
          bail();
        }
        methodPo mtd = labelCode(lbl);
        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lbl);
          bail();
        }

        if (!stackRoom(stackDelta(mtd))) {
          int root = gcAddRoot(H, (ptrPo) &mtd);

          stackPo prevStack = STK;

          gcAddRoot(H, (ptrPo) &prevStack);

          saveRegisters();
          STK = P->stk = glueOnStack(H, STK, (STK->sze * 3) / 2 + stackDelta(mtd), arity);

          SP = CSP = STK->sp;
          FP = STK->fp;
          pushFrme(mtd);

          // drop old frame on old stack
          framePo prevFrame = prevStack->fp;
          prevStack->sp = stackArg(prevStack, prevFrame, argCount(prevFrame->prog));
          prevStack->fp--;

          gcReleaseRoot(H, root);

#ifdef TRACEEXEC
          saveRegisters();
          verifyStack(STK, H);
#endif
        } else {
          // Overwrite existing arguments and locals
          ptrPo tgt = &arg(argCount(FP->prog));
          ptrPo src = SP + arity;                  /* base of argument vector */

          for (int ix = 0; ix < arity; ix++)
            *--tgt = *--src;    /* copy the argument vector */
          FP->csp = CSP = SP = tgt;
          FP->pc = PC = entryPoint(mtd);
          FP->prog = mtd;
        }

        LITS = codeLits(mtd);
        incEntryCount(mtd);              // Increment number of times program called
        continue;       /* Were done */
      }

      case Locals: {
        int32 height = collectI32(PC);
        assert(height >= 0);
        CSP = FP->csp = SP;
        SP -= height;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < height; ix++)
          SP[ix] = voidEnum;
#endif
        assert(SP == CSP - lclCount(FP->prog));
        continue;
      };

      case RtG: {
        H = processHeap(P);
        // Fall through
      }

      case Ret: {        /* return from function */
        termPo retVal = *SP;     /* return value */

        SP = &arg(argCount(FP->prog)); // Just above arguments to current call
        prevFrme();
        push(retVal);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Jmp:       /* jump to local offset */
        PC = collectOff(PC);
        assert(validPC(FP->prog, PC));
        continue;

      case Drop: {
        SP++;       /* drop tos */
        continue;
      }

      case Dup: {        /* duplicate tos */
        termPo tos = *SP;
        *--SP = tos;
        continue;
      }

      case Rot: {       // Pull up nth element of stack
        integer cnt = collectI32(PC);
        termPo tmp = SP[0];

        for (integer ix = 0; ix < cnt; ix++) {
          SP[ix] = SP[ix + 1];
        }
        SP[cnt] = tmp;
        continue;
      }

      case Rst: {
        int32 height = collectI32(PC);
        assert(height >= 0);
        SP = &FP->csp[-lclCount(FP->prog) - height];
        continue;
      }

      case Fiber: {
        // The top of a stack should be a binary lambda
        termPo fiberLambda = pop();
        saveRegisters();
        stackPo child = newStack(P, fiberLambda);
        restoreRegisters();
        push(child);                                                 // We return the new stack
        continue;
      }

      case Spawn: {
        // The top of a stack should be a unary lambda
        termPo lambda = pop();
        saveRegisters();
        stackPo child = splitStack(P, lambda);

        P->stk = attachStack(P->stk, child);
        verifyStack(P->stk, P->heap);
        restoreRegisters();
        continue;
      }

      case Suspend: { // Suspend identified fiber.
        stackPo stack = C_STACK(pop());
        termPo event = pop();

        if (stackState(stack) != active) {
          logMsg(logFile, "tried to suspend %s fiber %T", stackStateName(stackState(stack)), stack);
          bail();
        } else {
          saveRegisters();
          P->stk = detachStack(STK, stack);
          restoreRegisters();
          push(event);
          continue;
        }
      }
      case Resume: {
        stackPo stack = C_STACK(pop());
        termPo event = pop();

        if (stackState(stack) != suspended) {
          logMsg(logFile, "tried to resume %s stack %T", stackStateName(stackState(stack)), stack);
          bail();
        } else {
          saveRegisters();
          P->stk = attachStack(STK, stack);
          restoreRegisters();
          push(event);
          continue;
        }
      }
      case Retire: { // Similar to a suspend, except that we trash the susending stack
        stackPo fiber = C_STACK(pop());
        termPo event = pop();

        if (stackState(fiber) != active) {
          logMsg(logFile, "tried to retire a non-active stack %T", fiber);
          bail();
        } else {
          saveRegisters();
          P->stk = detachStack(STK, fiber);
          dropStack(fiber);
          restoreRegisters();
#ifdef TRACESTACK
          if (traceStack)
            verifyStack(STK, H);
#endif
          push(event);
          continue;
        }
      }
      case Release: { // Trash a fiber
        stackPo fiber = C_STACK(pop());

        if (stackState(fiber) != suspended) {
          logMsg(logFile, "tried to release a %s fiber %T", stackStateName(stackState(fiber)), fiber);
          bail();
        } else {
          saveRegisters();
          stackPo parent = detachStack(STK, fiber);
          dropStack(fiber);
          restoreRegisters();
          continue;
        }
      }
      case Underflow: {
        termPo val = pop();
        saveRegisters();  // Seal off the current stack
        assert(stackState(STK) == active);
        P->stk = dropStack(STK);
        restoreRegisters();
        push(val);
        continue;
      }
      case Try: {
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        saveRegisters();
        continuationPo cont = allocateContinuation(H, STK, SP, FP, exit);
        restoreRegisters();
        push(cont);
        continue;
      }
      case Throw: {
        continuationPo cont = C_CONTINUATION(pop());
        termPo val = pop();
        assert(continIsValid(cont));

        stackPo stk = contStack(cont);

        assert(isAttachedStack(STK, stk) && validFP(stk, contFP(cont)));

        saveRegisters();
        STK = P->stk = dropUntil(STK, stk);
        STK->fp = contFP(cont);
        STK->fp->pc = contPC(cont);
        STK->sp = contSP(cont);
        invalidateCont(cont);
        restoreRegisters();
        push(val);
        continue;
      }

      case Invoke: { // Invoke a continuation, passing arguments ...
        int arity = collectI32(PC);
        continuationPo cont = C_CONTINUATION(pop());

        assert(continIsValid(cont));

        stackPo stk = contStack(cont);

        if (stackState(stk) != suspended) {
          logMsg(logFile, "tried to resume non-suspended stack %T", stk);
          bail();
        } else if (currFrame(stk) != contFP(cont) || stackSP(stk) != contSP(cont)) {
          logMsg(logFile, "tried to resume non-valid continuation %T", cont);
          bail();
        } else {
          saveRegisters();
          for (integer ix = arity; ix > 0; ix--)
            pushStack(stk, arg(ix));
          P->stk = attachStack(STK, stk);
          P->stk->fp->pc = contPC(cont);
          invalidateCont(cont);
          restoreRegisters();
          continue;
        }
      }

      case TEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (C_STACK(Lhs) == C_STACK(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case LdV: {
        push(voidEnum);     /* load void */
        continue;
      }

      case LdC:     /* load literal value from pool */
        push(nthElem(LITS, collectI32(PC)));
        continue;

      case LdA: {
        int32 offset = collectI32(PC);
        push(arg(offset));    /* load argument */
        continue;
      }

      case LdL: {
        int32 offset = collectI32(PC);
        push(local(offset));      /* load local */
        continue;
      }

      case LdG: {
        int32 glbNo = collectI32(PC);

        globalPo glb = findGlobalVar(glbNo);

        if (glbIsSet(glb)) {
          termPo vr = getGlobal(glb);

          check(vr != Null, "undefined global");
          check(SP > (ptrPo) (FP + 1), "not enough room");

          push(vr);     /* load a global variable */
        } else {
          labelPo glbLbl = findLbl(globalVarName(glb), 0);
          if (glbLbl == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }
          methodPo glbThnk = labelCode(glbLbl);       /* set up for object call */

          if (glbThnk == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }

          if (!stackRoom(stackDelta(glbThnk) + STACKFRAME_SIZE)) {
            int root = gcAddRoot(H, (ptrPo) &glbThnk);
            stackGrow(stackDelta(glbThnk) + STACKFRAME_SIZE, codeArity(glbThnk));
            gcReleaseRoot(H, root);
            assert(stackRoom(stackDelta(glbThnk) + STACKFRAME_SIZE));

#ifdef TRACESTACK
            if (traceStack)
              verifyStack(STK, H);
#endif
          }
          FP->pc = PC;
          pushFrme(glbThnk);

          LITS = codeLits(glbThnk);
          H = globalHeap;
        }
        continue;
      }

      case Thunk: {  // Create a new thunk
        normalPo thLam = C_NORMAL(pop());

        if (reserveSpace(H, ThunkCellCount) != Ok) {
          saveRegisters();
          retCode ret = gcCollect(H, ThunkCellCount);
          if (ret != Ok)
            return ret;
          restoreRegisters();
        }
        thunkPo thnk = thunkVar(H, thLam);
        push(thnk);       /* put the structure back on the stack */
        continue;
      }

      case LdTh: {
        thunkPo thVr = C_THUNK(pop());

        if (thunkIsSet(thVr)) {
          termPo vr = thunkVal(thVr);

          check(vr != Null, "undefined thunk value");
          check(SP > (ptrPo) (FP + 1), "not enough room");

          push(vr);     /* load a global variable */
        } else {
          normalPo thLambda = thunkLam(thVr);

          labelPo oLbl = objLabel(termLbl(thLambda), 2); // Two arguments: the thunk and the free vector

          if (oLbl == Null) {
            logMsg(logFile, "label %s/%d not defined", labelName(termLbl(thLambda)), 2);
            bail();
          }

          methodPo mtd = labelCode(oLbl);       /* set up for object call */

          if (mtd == Null) {
            logMsg(logFile, "no definition for %T", oLbl);
            bail();
          }

          push(nthElem(thLambda, 0));                     // Put the free term back on the stack
          push(thVr);

          if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
            int root = gcAddRoot(H, (ptrPo) &mtd);
            stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
            gcReleaseRoot(H, root);

#ifdef TRACESTACK
            if (traceStack)
              verifyStack(STK, H);
#endif
          }

          assert(isPcOfMtd(FP->prog, PC));
          FP->pc = PC;
          pushFrme(mtd);
          LITS = codeLits(mtd);
          incEntryCount(mtd);              // Increment program count
        }
        continue;
      }

      case StTh: {                           // Store into thunk
        thunkPo thnk = C_THUNK(pop());
        termPo val = pop();

        if (thunkIsSet(thnk)) {
          logMsg(logFile, "thunk %T already set", thnk);
          bail();
        }

        setThunk(thnk, val);      // Update the thunk variable
        continue;
      }

      case TTh: {                        // Set thunk and carry on
        thunkPo thnk = C_THUNK(pop());
        termPo val = top();

        if (thunkIsSet(thnk)) {
          logMsg(logFile, "thunk %T already set", thnk);
          bail();
        }

        setThunk(thnk, val);      // Update the thunk variable
        continue;
      }

      case CLbl: {
        labelPo l = C_LBL(nthElem(LITS, collectI32(PC)));
        termPo t = top();
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        if (isNormalPo(t)) {
          normalPo cl = C_NORMAL(t);
          if (sameLabel(l, termLbl(cl)))
            continue;
        }
        PC = exit;
        continue;
      }

      case Nth: {
        int32 ix = collectI32(PC);  /* which element */
        termPo t = pop();
        check(isNormalPo(t), "tried to access non term");

        normalPo cl = C_NORMAL(t);  /* which term? */
        push(nthArg(cl, ix));

        continue;
      }

      case StL: {
        int32 offset = collectI32(PC);
        ptrPo dest = &local(offset);
        *dest = pop();
        continue;
      }

      case StV: {
        int32 offset = collectI32(PC);
        ptrPo dest = &local(offset);
        *dest = voidEnum;
        continue;
      }
      case TL: {
        int32 offset = collectI32(PC);
        ptrPo dest = &local(offset);
        *dest = top();
        continue;
      }

      case StA: {
        int32 offset = collectI32(PC);
        ptrPo dest = &arg(offset);
        *dest = pop();     /* store as argument */
        continue;
      }

      case StNth: {      /* store into a closure */
        int32 ix = collectI32(PC);
        termPo tos = pop();
        normalPo cl = C_NORMAL(pop());
        cl->args[ix] = tos;
        continue;
      }

      case StG: {
        int32 glbNo = collectI32(PC);
        termPo val = pop();
        globalPo glb = findGlobalVar(glbNo);
        setGlobalVar(glb, val);      // Update the global variable
        continue;
      }

      case TG: {
        int32 glbNo = collectI32(PC);
        termPo val = top();
        globalPo glb = findGlobalVar(glbNo);
        setGlobalVar(glb, val);      // Update the global variable
        continue;
      }

      case Cell: {
        checkAlloc(CellCellCount);
        cellPo cell = newCell(H, pop());
        push(cell);
        continue;
      }

      case Get: {
        cellPo cell = C_CELL(pop());
        push(getCell(cell));
        continue;
      }

      case Assign: {
        cellPo cell = C_CELL(pop());
        termPo vl = pop();
        setCell(cell, vl);
        continue;
      }

      case IAdd: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs + Rhs);
        push(Rs);
        continue;
      }

      case ISub: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs - Rhs);
        push(Rs);
        continue;
      }
      case IMul: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs * Rhs);
        push(Rs);
        continue;
      }
      case IDiv: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());
        termPo Rs = makeInteger(Lhs / Rhs);
        push(Rs);
        continue;
      }
      case IMod: {
        integer denom = integerVal(pop());
        integer numerator = integerVal(pop());

        integer reslt = denom % numerator;

        termPo Rs = (termPo) makeInteger(reslt);

        push(Rs);
        continue;
      }
      case IAbs: {
        termPo Trm = pop();
        integer Arg = integerVal(Trm);

        termPo Rs = (Arg < 0 ? makeInteger(-Arg) : Trm);
        push(Rs);
        continue;
      }
      case IEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) == integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case ILt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) < integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case IGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) >= integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case ICmp: {
        termPo i = pop();
        termPo j = pop();
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        if (integerVal(i) != integerVal(j))
          PC = exit;
        continue;
      }
      case BAnd: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs & (uinteger) Rhs));
        push(Rs);
        continue;
      }
      case BOr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger((integer) ((uinteger) Lhs | (uinteger) Rhs));
        push(Rs);
        continue;
      }
      case BXor: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs ^ (uinteger) Rhs));
        push(Rs);
        continue;
      }
      case BNot: {
        integer Lhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) (~(uinteger) Lhs));
        push(Rs);
        continue;
      }
      case BLsl: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs << (uinteger) Rhs));
        push(Rs);
        continue;
      }
      case BLsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) (((uinteger) Lhs) >> ((uinteger) Rhs)));
        push(Rs);
        continue;
      }
      case BAsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((Lhs) >> Rhs);
        push(Rs);
        continue;
      }
      case FAdd: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs + Rhs);
        push(Rs);
        continue;
      }
      case FSub: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs - Rhs);
        push(Rs);
        continue;
      }
      case FMul: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs * Rhs);
        push(Rs);
        continue;
      }
      case FDiv: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs / Rhs);
        push(Rs);
        continue;
      }
      case FMod: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(fmod(Lhs, Rhs));
        push(Rs);
        continue;
      }
      case FAbs: {
        double Lhs = floatVal(pop());

        termPo Rs = makeFloat(fabs(Lhs));
        push(Rs);
        continue;
      }
      case FEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();
        termPo Eps = pop();

        termPo Rs = (nearlyEqual(floatVal(Lhs), floatVal(Rhs), floatVal(Eps)) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case FLt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (floatVal(Lhs) < floatVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case FGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (floatVal(Lhs) >= floatVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        continue;
      }
      case FCmp: {
        termPo x = pop();
        termPo y = pop();
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        if (floatVal(x) != floatVal(y))
          PC = exit;
        continue;
      }

      case Case: {      /* case instruction */
        int32 mx = collectI32(PC);
        termPo tos = top();
        integer hx = hashTerm(tos) % mx;

        PC = (insPo) ((void *) PC + (sizeof(insWord) * 3) * hx);
        continue;
      }

      case IndxJmp: {    // Branch based on index of constructor term
        int32 mx = collectI32(PC);
        normalPo top = C_NORMAL(top());
        labelPo lbl = termLbl(top);
        integer hx = labelIndex(lbl);

        PC = (insPo) ((void *) PC + (sizeof(insWord) * 3) * hx);
        continue;
      }

      case Unpack: {
        labelPo l = C_LBL(nthElem(LITS, collectI32(PC)));
        termPo t = pop();
        insPo exit = collectOff(PC);

        assert(validPC(FP->prog, exit));

        normalPo n;
        if (isNormalPo(t) && sameLabel(l, termLbl(n = C_NORMAL(t)))) {
          integer arity = labelArity(l);
          for (integer ix = arity - 1; ix >= 0; ix--)
            push(nthElem(n, ix));
        } else {
          PC = exit;
        }
        continue;
      }

      case Alloc: {      /* heap allocate term */
        labelPo cd = C_LBL(nthElem(LITS, collectI32(PC)));
        if (enoughRoom(H, cd) != Ok) {
          saveRegisters();
          retCode ret = gcCollect(H, NormalCellCount(labelArity(cd)));
          if (ret != Ok)
            return ret;
          restoreRegisters();
        }
        normalPo cl = allocateStruct(H, cd); /* allocate a closure on the heap */
        for (int ix = 0; ix < cd->arity; ix++)
          cl->args[ix] = pop();   /* fill in free variables by popping from stack */
        push(cl);       /* put the structure back on the stack */
        continue;
      }

      case Cmp: {
        termPo i = pop();
        termPo j = pop();
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        if (!sameTerm(i, j))
          PC = exit;
        continue;
      }

      case If: {
        termPo i = pop();
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        if (sameTerm(i, trueEnum))
          PC = exit;
        continue;
      }

      case IfNot: {
        termPo i = pop();
        insPo exit = collectOff(PC);
        assert(validPC(FP->prog, exit));

        if (!sameTerm(i, trueEnum))
          PC = exit;
        continue;
      }

      case Frame: {
        PC += 2; // ignore frame entity for now
        continue;
      }

      case dBug: {
#ifdef TRACEEXEC
        if (lineDebugging) {
          saveRegisters();
          enterDebug(P);
          restoreRegisters();
        }
#endif
        continue;
      }

      default:
      case illegalOp:
        syserr("Illegal instruction");
    }
  }
}
