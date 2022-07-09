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
#include "utils.h"
#include "thunk.h"
#include "cellP.h"
#include "jit.h"

#define collectI32(pc) (hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<(unsigned)16)|lo32))
#define collectOff(pc) (hi32 = collectI32(pc), (pc)+(signed)hi32)
#define pcBeforeOff(pc) (pc-=2, collectOff(pc))

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

#define bail() STMT_WRAP({\
  saveRegisters();\
  stackTrace(P, logFile, STK, True,displayDepth);\
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

    assert(CSP == FP->csp && SP <= CSP- lclCount(FP->prog));

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
        stackTrace(P, logFile, P->stk, False, displayDepth);

        return Error;
      }

      case Call: {
        termPo nProg = nthElem(LITS, collectI32(PC));
        methodPo mtd = labelCode(C_LBL(nProg));   // Which program do we want?
        insPo exit = collectOff(PC);

        if (mtd == Null) {
          logMsg(logFile, "label %T not defined", nProg);
          bail();
        }

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          int root = gcAddRoot(H, &nProg);
          gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);
          assert(stackRoom(stackDelta(mtd) + STACKFRAME_SIZE));
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
          bumpCallCount(mtd);
          integer lclCnt = lclCount(mtd);  /* How many locals do we have */

          pushFrme(mtd);
          LITS = codeLits(mtd);
          incEntryCount(mtd);              // Increment number of times program called

          SP -= lclCnt;
#ifdef TRACEEXEC
          for (integer ix = 0; ix < lclCnt; ix++)
            SP[ix] = voidEnum;
#endif
        }

        continue;
      }

      case OCall: {        /* Call tos a1 .. an -->   */
        int arity = collectI32(PC);
        normalPo obj = C_NORMAL(pop());
        labelPo oLbl = objLabel(termLbl(obj), arity);
        insPo exit = collectOff(PC);

        if (oLbl == Null) {
          logMsg(logFile, "label %s/%d not defined", labelName(termLbl(obj)), arity);
          bail();
        }

        methodPo mtd = labelCode(oLbl);       /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", oLbl);
          bail();
        }

        bumpCallCount(mtd);

        push(nthElem(obj, 0));                     // Put the free term back on the stack

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          int root = gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);
        }

        assert(isPcOfMtd(FP->prog, PC));
        FP->pc = PC;
        pushFrme(mtd);
        LITS = codeLits(mtd);

        incEntryCount(mtd);              // Increment number of times program called

        integer lclCnt = lclCount(mtd);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */
        insPo exit = collectOff(PC);

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
        assert(H->topRoot == 0);
        restoreRegisters();
        SP += esc->arity;

        switch (ret.ret) {
          case Ok:
            if (ret.result != Null)
              push(ret.result);
            continue;
          case Error:
            if (ret.result != Null)
              push(ret.result);
            else
              push(unitEnum);
            PC = exit;
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

        bumpCallCount(mtd);

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
        integer lclCnt = lclCount(mtd);  /* How many locals do we have */

        SP -= lclCnt;                   // New top of stack

#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;       /* Were done */
      }

      case TOCall: {       /* Tail call */
        int arity = collectI32(PC);
        normalPo obj = C_NORMAL(pop());
        labelPo lbl = objLabel(termLbl(obj), arity);

        push(nthElem(obj, 0));                     // Put the free term back on the stack

        methodPo mtd = labelCode(lbl);
        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lbl);
          bail();
        }

        bumpCallCount(mtd);

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
        integer lclCnt = lclCount(mtd);  /* How many locals do we have */

        SP -= lclCnt;                   // New top of stack

#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;       /* Were done */
      }

      case RtG: {
        H = processHeap(P);
        // Fall through
      }

      case Ret: {        /* return from function */
        termPo retVal = *SP;     /* return value */

        assert(FP > baseFrame(STK) && SP <= CSP);

        SP = &arg(argCount(FP->prog)); // Just above arguments to current call
        FP = previousFrame(STK, FP);
        CSP = FP->csp;
        PC = FP->pc;
        LITS = codeLits(FP->prog);   /* reset pointer to code literals */
        assert(SP <= CSP);
        push(retVal);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case RetX: {        /* return exceptionally from function */
        termPo retVal = *SP;     /* return value */

        assert(FP > baseFrame(STK) && SP <= stackLimit(STK));

        SP = &arg(argCount(FP->prog)); // Just above arguments to current call
        FP = previousFrame(STK, FP);
        CSP = FP->csp;
        PC = FP->pc;
        LITS = codeLits(FP->prog);   /* reset pointer to code literals */
        PC = pcBeforeOff(PC);  // Pick up the exception offset from the previous instruction
        assert(SP <= CSP);

        push(retVal);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Jmp:       /* jump to local offset */
        PC = collectOff(PC);
        assert(validPC(FP->prog, PC));
        continue;

      case Drop:
        SP++;       /* drop tos */
        continue;

      case Dup: {        /* duplicate tos */
        termPo tos = *SP;
        *--SP = tos;
        continue;
      }

      case Rst: {
        int32 height = collectI32(PC);
        assert(height >= 0);
        SP = &FP->csp[-lclCount(FP->prog)-height];
        continue;
      }

      case Task: {
        // The top of a stack should be a binary lambda
        normalPo obj = C_NORMAL(pop());
        labelPo oLbl = objLabel(termLbl(obj),
                                3); // Fixed arity. Arg0 = free vars, arg1 = task ref, arg2 is the first event

        if (oLbl == Null) {
          logMsg(logFile, "program %s/1 not defined", labelName(termLbl(obj)));
          bail();
        }

        methodPo mtd = labelCode(oLbl);   // Which program do we want?

        if (mtd == Null) {
          logMsg(logFile, "program %s/2 not defined", labelName(termLbl(obj)));
          bail();
        }
        bumpCallCount(mtd);

        int root = gcAddRoot(H, (ptrPo) &mtd);
        saveRegisters();
        stackPo child = spinupStack(H, minStackSize);
        restoreRegisters();
        gcReleaseRoot(H, root);

        pushStack(child, (termPo) child);
        pushStack(child, nthElem(obj, 0));            // Put the free term on the new stack
        child->fp = pushFrame(child, mtd, child->fp);

        push(child);                                                 // We return the new stack

        continue;
      }
      case Suspend: { // Suspend identified task.
        termPo event = pop();
        stackPo task = C_TASK(pop());

        if (stackState(task) != active) {
          logMsg(logFile, "tried to suspend non-active task %T", task);
          bail();
        } else {
          saveRegisters();
          P->stk = detachTask(STK, task);
          restoreRegisters();
          push(event);
          continue;
        }
      }
      case Resume: {
        termPo event = pop();
        stackPo task = C_TASK(pop());

        if (stackState(task) != suspended) {
          logMsg(logFile, "tried to resume non-suspended task %T", task);
          bail();
        } else {
          saveRegisters();
          STK = P->stk = attachTask(STK, task);
          restoreRegisters();
          push(event);
          continue;
        }
      }
      case Retire: { // Similar to a suspend, except that we trash the susending stack
        termPo event = pop();
        stackPo task = C_TASK(pop());

        if (stackState(task) != active) {
          logMsg(logFile, "tried to retire a non-active task %T", task);
          bail();
        } else {
          saveRegisters();
          P->stk = detachTask(STK, task);
          dropStack(task);
          restoreRegisters();
          push(event);
          continue;
        }
      }
      case Release: { // Trash a task
        stackPo task = C_TASK(pop());

        if (stackState(task) != suspended) {
          logMsg(logFile, "tried to release a %s task %T", stackStateName(stackState(task)), task);
          bail();
        } else {
          saveRegisters();
          stackPo parent = detachTask(STK, task);
          dropStack(task);
          continue;
        }
      }
      case Underflow: {
        termPo val = pop();
        saveRegisters();  // Seal off the current stack
        assert(stackState(STK) == active);
        STK = P->stk = dropStack(STK);
        restoreRegisters();
        push(val);
        continue;
      }
      case TEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (C_TASK(Lhs) == C_TASK(Rhs) ? trueEnum : falseEnum);
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
        insPo exit = collectOff(PC);

        globalPo glb = findGlobalVar(glbNo);

        if (glbIsSet(glb)) {
          termPo vr = getGlobal(glb);

          check(vr != Null, "undefined global");

          push(vr);     /* load a global variable */
        } else {
          methodPo glbThnk = labelCode(findLbl(globalVarName(glb), 0));       /* set up for object call */

          if (glbThnk == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }

          if (!stackRoom(stackDelta(glbThnk) + STACKFRAME_SIZE)) {
            int root = gcAddRoot(H, (ptrPo) &glbThnk);
            stackGrow(stackDelta(glbThnk) + STACKFRAME_SIZE, codeArity(glbThnk));
            gcReleaseRoot(H, root);
            assert(stackRoom(stackDelta(glbThnk) + STACKFRAME_SIZE));
          }
          FP->pc = PC;
          pushFrme(glbThnk);

          LITS = codeLits(glbThnk);
          H = globalHeap;

          integer lclCnt = lclCount(glbThnk);  /* How many locals do we have */
          SP -= lclCnt;
#ifdef TRACEEXEC
          for (integer ix = 0; ix < lclCnt; ix++)
            SP[ix] = voidEnum;
#endif
        }
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

        checkAlloc(IntegerCellCount);
        termPo Rs = allocateInteger(H, Lhs + Rhs);
        push(Rs);
        continue;
      }

      case ISub: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, Lhs - Rhs);
        push(Rs);
        continue;
      }
      case IMul: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, Lhs * Rhs);
        push(Rs);
        continue;
      }
      case IDiv: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, Lhs / Rhs);
        push(Rs);
        continue;
      }
      case IMod: {
        integer denom = integerVal(pop());
        integer numerator = integerVal(pop());

        integer reslt = denom % numerator;

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, reslt);

        push(Rs);
        continue;
      }
      case IAbs: {
        termPo Trm = pop();
        integer Arg = integerVal(Trm);

        checkAlloc(IntegerCellCount);
        termPo Rs = (Arg < 0 ? (termPo) allocateInteger(H, -Arg) : Trm);
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

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, ((unsigned) Lhs & (unsigned) Rhs));
        push(Rs);
        continue;
      }
      case BOr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, ((unsigned) Lhs | (unsigned) Rhs));
        push(Rs);
        continue;
      }
      case BXor: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, ((unsigned) Lhs ^ (unsigned) Rhs));
        push(Rs);
        continue;
      }
      case BNot: {
        integer Lhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, ~(unsigned) Lhs);
        push(Rs);
        continue;
      }
      case BLsl: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, ((unsigned) Lhs << (unsigned) Rhs));
        push(Rs);
        continue;
      }
      case BLsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, (((unsigned) Lhs) >> ((unsigned) Rhs)));
        push(Rs);
        continue;
      }
      case BAsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        checkAlloc(IntegerCellCount);
        termPo Rs = (termPo) allocateInteger(H, (Lhs) >> Rhs);
        push(Rs);
        continue;
      }
      case FAdd: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs + Rhs);
        push(Rs);
        continue;
      }
      case FSub: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs - Rhs);
        push(Rs);
        continue;
      }
      case FMul: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs * Rhs);
        push(Rs);
        continue;
      }
      case FDiv: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs / Rhs);
        push(Rs);
        continue;
      }
      case FMod: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, fmod(Lhs, Rhs));
        push(Rs);
        continue;
      }
      case FAbs: {
        double Lhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, fabs(Lhs));
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
        normalPo t = C_NORMAL(pop());
        insPo exit = collectOff(PC);

        assert(validPC(FP->prog, exit));

        if (sameLabel(l, termLbl(t))) {
          integer arity = labelArity(l);
          for (integer ix = arity - 1; ix >= 0; ix--)
            push(nthElem(t, ix));
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
