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
#include "char.h"
#include "engineP.h"
#include "debugP.h"
#include "continuationP.h"
#include <math.h>
#include "cellP.h"
#include "closureP.h"
#include "thunkP.h"
#include "errorCodes.h"
#include "ltype.h"

logical collectStats = False;

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
#define push(T) STMT_WRAP({*--SP=(termPo)(T);})
#define local(lcl) (((ptrPo)FP)[-(lcl)])
#define arg(aix) (((ptrPo)(FP+1))[(aix)])
#define stackRoom(amnt) ((SP-(amnt)) > STK->stkMem)
#define saveRegisters() STMT_WRAP({ FP->pc = PC; STK->sp = SP; STK->fp = FP; P->stk = STK;})
#define restoreRegisters() STMT_WRAP({ STK = P->stk; FP = STK->fp; PC = FP->pc;  SP=STK->sp; LITS=FP->pool;})
#define pushFrme(mtd) STMT_WRAP({framePo f = ((framePo)SP)-1; f->fp=FP; PC = f->pc = entryPoint(mtd); f->pool = codeLits(mtd); FP = f; SP = (ptrPo)FP;})
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
  register normalPo LITS = FP->pool; /* pool of literals */
  register ptrPo SP = STK->sp;         /* Current 'top' of stack (grows down) */

  currentProcess = P;

  for (;;) {
    pcCount++;        /* increment total number of executed */

    if (insDebugging) {
      saveRegisters();
      insDebug(P);
      restoreRegisters();
    }

    switch (PC->op) {
      case Halt: {
        int32 exitCode = PC->fst;

        return (retCode) exitCode;
      }
      case Nop: {
        break;
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
        labelPo nProg = C_LBL(nthElem(LITS, PC->fst));
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
        assert(validPC(frameMtd(FP), PC));
        FP->pc = PC + 1;

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
        int arity = PC->fst;
        termPo cl = pop();
        if (!isClosure(cl)) {
          logMsg(logFile, "Calling non-closure %T", cl);
          bail();
        }
        closurePo obj = C_CLOSURE(cl);
        labelPo lb = closureLabel(obj);

        if (labelArity(lb) != arity) {
          logMsg(logFile, "closure %T does not have correct arity %d", obj, arity);
          bail();
        }

        methodPo mtd = labelCode(lb);       /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lb);
          bail();
        }

        push(closureFree(obj));                     // Put the free term back on the stack

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          int root = gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);

#ifdef TRACESTACK
          if (traceStack)
            verifyStack(STK, H);
#endif
        }

        assert(validPC(frameMtd(FP), PC));
        FP->pc = PC + 1;
        pushFrme(mtd);
        LITS = codeLits(mtd);
        incEntryCount(mtd);              // Increment number of times program called
        continue;
      }

      case Escape: {                     /* call escape */
        int32 escNo = PC->fst;           /* escape number */

        if (collectStats)
          recordEscape(escNo);

        escapePo esc = getEscape(escNo);
        saveRegisters();
        assert(H->topRoot == 0);
        ReturnStatus ret;

        switch (esc->arity) {
          case 0:
            ret = ((escFun0) (esc->fun))(H);
            break;
          case 1: {
            termPo a1 = popStack(STK);
            ret = ((escFun1) (esc->fun))(H, a1);
            break;
          }
          case 2: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            ret = ((escFun2) (esc->fun))(H, a1, a2);
            break;
          }
          case 3: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            termPo a3 = popStack(STK);
            ret = ((escFun3) (esc->fun))(H, a1, a2, a3);
            break;
          }
          case 4: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            termPo a3 = popStack(STK);
            termPo a4 = popStack(STK);
            ret = ((escFun4) (esc->fun))(H, a1, a2, a3, a4);
            break;
          }
          case 5: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            termPo a3 = popStack(STK);
            termPo a4 = popStack(STK);
            termPo a5 = popStack(STK);
            ret = ((escFun5) (esc->fun))(H, a1, a2, a3, a4, a5);
            break;
          }
          case 6: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            termPo a3 = popStack(STK);
            termPo a4 = popStack(STK);
            termPo a5 = popStack(STK);
            termPo a6 = popStack(STK);
            ret = ((escFun6) (esc->fun))(H, a1, a2, a3, a4, a5, a6);
            break;
          }
          case 7: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            termPo a3 = popStack(STK);
            termPo a4 = popStack(STK);
            termPo a5 = popStack(STK);
            termPo a6 = popStack(STK);
            termPo a7 = popStack(STK);
            ret = ((escFun7) (esc->fun))(H, a1, a2, a3, a4, a5, a6, a7);
            break;
          }
          case 8: {
            termPo a1 = popStack(STK);
            termPo a2 = popStack(STK);
            termPo a3 = popStack(STK);
            termPo a4 = popStack(STK);
            termPo a5 = popStack(STK);
            termPo a6 = popStack(STK);
            termPo a7 = popStack(STK);
            termPo a8 = popStack(STK);
            ret = ((escFun8) (esc->fun))(H, a1, a2, a3, a4, a5, a6, a7, a8);
            break;
          }
          default:
            logMsg(logFile, "invalid arity for escape %s", escapeName(esc));
            bail();
        }

        restoreRegisters();
        assert(H->topRoot == 0);

        if (ret.ret == Normal) {
          if (ret.result != Null)
            push(ret.result);
          break;
        } else {
          push(ret.result);
          push(ret.cont);
          goto Exception;
        }
      }

      case TCall: {       /* Tail call of explicit program */
        termPo nProg = nthElem(LITS, PC->fst);
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

          SP = STK->sp;
          FP = STK->fp;
          pushFrme(mtd);
          LITS = codeLits(mtd);

          // drop old frame on old stack
          dropFrame(prevStack);
          gcReleaseRoot(H, root);
        } else {
          // Overwrite existing arguments and locals
          ptrPo tgt = &arg(argCount(frameMtd(FP)));
          ptrPo src = SP + arity;                  /* base of argument vector */
          framePo Pfp = FP->fp;

          for (int ix = 0; ix < arity; ix++)
            *--tgt = *--src;    /* copy the argument vector */
          FP = ((framePo) tgt) - 1;      // Frame might have moved
          FP->fp = Pfp;
          FP->pc = PC = entryPoint(mtd);
          FP->pool = LITS = codeLits(mtd);
          SP = (ptrPo) FP;
        }

        incEntryCount(mtd);              // Increment number of times program called
        continue;       /* Were done */
      }

      case TOCall: {       /* Tail call */
        int arity = PC->fst;
        termPo cl = pop();
        if (!isClosure(cl)) {
          logMsg(logFile, "Calling non-closure %T", cl);
          bail();
        }
        closurePo obj = C_CLOSURE(cl);
        labelPo lb = closureLabel(obj);

        if (labelArity(lb) != arity) {
          logMsg(logFile, "closure %T does not have correct arity %d", obj, arity);
          bail();
        }

        push(closureFree(obj));                     // Put the free term back on the stack
        methodPo mtd = labelCode(lb);       /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lb);
          bail();
        }

        if (!stackRoom(stackDelta(mtd))) {
          int root = gcAddRoot(H, (ptrPo) &mtd);

          stackPo prevStack = STK;

          gcAddRoot(H, (ptrPo) &prevStack);

          saveRegisters();
          STK = P->stk = glueOnStack(H, STK, (STK->sze * 3) / 2 + stackDelta(mtd), arity);
          SP = STK->sp;
          FP = STK->fp;
          pushFrme(mtd);
          LITS = codeLits(mtd);

          // drop old frame on old stack
          dropFrame(prevStack);
          gcReleaseRoot(H, root);
        } else {
          // Overwrite existing arguments and locals
          ptrPo tgt = &arg(argCount(frameMtd(FP)));
          ptrPo src = SP + arity;                  /* base of argument vector */
          framePo Pfp = FP->fp;

          for (int ix = 0; ix < arity; ix++)
            *--tgt = *--src;    /* copy the argument vector */
          FP = ((framePo) tgt) - 1;      // Frame might have moved
          FP->fp = Pfp;
          FP->pc = PC = entryPoint(mtd);
          FP->pool = LITS = codeLits(mtd);
        }

        incEntryCount(mtd);              // Increment number of times program called
        continue;       /* Were done */
      }

      case Entry: {
        integer height = lclCount(frameMtd(FP));
        assert(height >= 0);
        SP = ((ptrPo) FP) - height;
        for (integer ix = 0; ix < height; ix++)
          SP[ix] = voidEnum;
        break;
      };

      case Ret: {        /* return from function */
        termPo retVal = *SP;     /* return value */

        assert(FP < baseFrame(STK));
        SP = &arg(argCount(frameMtd(FP))); // Just above arguments to current call
        FP = FP->fp;
        PC = FP->pc;
        LITS = FP->pool;

        push(retVal);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Block: {
        break;
      }

    breakPoint:{
        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        PC += PC->alt + 1;
        continue;
      }

      case Break: {
        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        PC += PC->alt + 1;
        continue;
      }

      case Loop: {
        PC += PC->alt;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        break;
      }

      case Drop: {
        SP++;       /* drop tos */
        break;
      }

      case Dup: {        /* duplicate tos */
        termPo tos = *SP;
        *--SP = tos;
        break;
      }

      case Rot: {       // Pull up nth element of stack
        integer cnt = PC->fst;
        termPo tmp = SP[0];

        for (integer ix = 0; ix < cnt; ix++) {
          SP[ix] = SP[ix + 1];
        }
        SP[cnt] = tmp;
        break;
      }

      case Rst: {
        int32 height = PC->fst;
        assert(height >= 0);
        SP = &local(lclCount(frameMtd(FP)) + height);
        break;
      }

      case Fiber: {
        // The top of a stack should be a binary lambda
        termPo fiberLambda = pop();
        saveRegisters();
        stackPo child = newStack(H, fiberLambda);
        restoreRegisters();
        push(child);              // We return the new stack
        break;
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
      case Underflow: {
        termPo val = pop();
        saveRegisters();  // Seal off the current stack
        assert(stackState(STK) == active);
        P->stk = dropStack(STK);
        restoreRegisters();
        push(val);
        continue;
      }

      case Reset: {  // Start a new delimited computation
        // The top of a stack should be a unary lambda
        termPo lambda = pop();
        if (!isClosure(lambda)) {
          logMsg(logFile, "expecting a closure, not %T", lambda);
          bail();
        }
        saveRegisters();
        stackPo child = splitStack(P, lambda);

        P->stk = attachStack(P->stk, child);
        verifyStack(P->stk, P->heap);
        restoreRegisters();
        break;
      }

      case Shift: { // Suspend current computation, invoke shift function with new continuation.
        termPo cl = pop();
        stackPo stack = C_STACK(pop());

        if (stackState(stack) != active) {
          logMsg(logFile, "tried to suspend %s fiber %T", stackStateName(stackState(stack)), stack);
          bail();
        } else {
          saveRegisters();
          continuationPo cont = allocateContinuation(H, stack);
          P->stk = detachStack(STK, stack);
          restoreRegisters();

          push((termPo) cont);

          if (!isClosure(cl)) {
            logMsg(logFile, "Calling non-closure %T", cl);
            bail();
          }
          closurePo obj = C_CLOSURE(cl);
          labelPo lb = closureLabel(obj);

          if (labelArity(lb) != 2) {
            logMsg(logFile, "closure %T does not have correct arity %d", obj, 2);
            bail();
          }

          methodPo mtd = labelCode(lb);       /* set up for object call */

          if (mtd == Null) {
            logMsg(logFile, "no definition for %T", lb);
            bail();
          }

          push(closureFree(obj));                     // Put the free term back on the stack

          if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
            int root = gcAddRoot(H, (ptrPo) &mtd);
            stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
            gcReleaseRoot(H, root);

#ifdef TRACESTACK
            if (traceStack)
              verifyStack(STK, H);
#endif
          }

          assert(validPC(frameMtd(FP), PC));
          FP->pc = PC + 1;
          pushFrme(mtd);
          LITS = codeLits(mtd);
          incEntryCount(mtd);              // Increment number of times program called
          continue;
        }
      }

      case Invoke: {                        // Invoke a continuation on current top of stack
        termPo event = pop();
        termPo k = pop();
        if (!isContinuation(k)) {
          logMsg(logFile, "tried to invoke non-continuation %T", k);
          bail();
        } else {
          continuationPo cont = C_CONTINUATION(k);
          if (continIsValid(cont)) {
            stackPo stack = contStack(cont);
            invalidateCont(cont);

            saveRegisters();
            P->stk = attachStack(STK, stack);
            restoreRegisters();
            push(event);
            continue;
          } else {
            logMsg(logFile, "continuation %T not in valid state", k);
            bail();
          }
        }
      }

      case Try: {
        assert(validPC(frameMtd(FP), PC + PC->alt));
        check(stackRoom(TryFrameCellCount), "unexpected stack overflow");

        saveRegisters();
        integer tryIndex = pushTryFrame(STK, P, PC + PC->alt, SP, FP);
        restoreRegisters();
        push(makeInteger(tryIndex));
#ifdef TRACESTACK
        if (traceStack)
          logMsg(logFile, "entering try scope %ld (%d)", tryIndex, tryStackSize(P));
#endif
        break;
      }

      case EndTry: {
        integer tryIndex = integerVal(pop());

#ifdef TRACESTACK
        if (traceStack)
          logMsg(logFile, "leaving try scope %ld (%d)", tryIndex, tryStackSize(P));
#endif
        check(STK->try->tryIndex == tryIndex, "misaligned try block");
        tryFramePo try = STK->try;
        check(try->fp == FP, "misaligned try block");
        STK->try = try->try;

        ptrPo tgt = (ptrPo) (try + 1);
        ptrPo src = (ptrPo) try;
        while (src > SP) {
          *--tgt = *--src;
        }
        SP = STK->sp = tgt;
        break;
      }
      case Throw: {
        Exception:
        {
          integer tryIndex = integerVal(pop());

#ifdef TRACESTACK
          if (traceStack)
            logMsg(logFile, "throwing to try scope %ld (%d)", tryIndex, tryStackSize(P));
#endif

          termPo val = pop();

          saveRegisters();
          stackPo stk = popTryFrame(P, tryIndex);
          if (stk == Null) {
            logMsg(logFile, "cannot find catch handler");
            bail();
          } else {
            restoreRegisters();
            push(val);
            continue;
          }
        }
      }

      case TEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (C_STACK(Lhs) == C_STACK(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case LdV: {
        push(voidEnum);     /* load void */
        break;
      }

      case LdC:     /* load literal value from pool */
        push(nthElem(LITS, PC->fst));
        break;

      case LdA: {
        int32 offset = PC->fst;
        push(arg(offset));    /* load argument */
        break;
      }

      case LdL: {
        int32 offset = PC->fst;
        push(local(offset));      /* load local */
        break;
      }

      case LdG: {
        int32 glbNo = PC->fst;

        globalPo glb = findGlobalVar(glbNo);

        if (glbIsSet(glb)) {
          termPo gval = getGlobal(glb);

          check(gval != Null, "undefined global");
          check(stackRoom(1), "unexpected stack overflow");

          push(gval);     /* load a global variable */
          break;
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
          FP->pc = PC + 1;
          pushFrme(glbThnk);

          LITS = codeLits(glbThnk);
          H = globalHeap;
          continue;
        }
      }

      case CLit: {
        termPo l = nthElem(LITS, PC->fst);
        termPo t = top();

        if (!sameTerm(l, t))
          goto breakPoint;
        else
          break;
      }

      case CLbl: {
        labelPo l = C_LBL(nthElem(LITS, PC->fst));
        termPo t = top();

        if (isNormalPo(t)) {
          normalPo cl = C_NORMAL(t);
          if (sameLabel(l, termLbl(cl)))
            break;
        }
        goto breakPoint;
      }

      case Nth: {
        int32 ix = PC->fst;  /* which element */
        termPo t = pop();
        check(isNormalPo(t), "tried to access non term");

        normalPo cl = C_NORMAL(t);  /* which term? */
        push(nthArg(cl, ix));

        break;
      }

      case StL: {
        int32 offset = PC->fst;
        ptrPo dest = &local(offset);
        *dest = pop();
        break;
      }

      case StV: {
        int32 offset = PC->fst;
        ptrPo dest = &local(offset);
        *dest = voidEnum;
        break;
      }
      case TL: {
        int32 offset = PC->fst;
        ptrPo dest = &local(offset);
        *dest = top();
        break;
      }

      case StA: {
        int32 offset = PC->fst;
        ptrPo dest = &arg(offset);
        *dest = pop();     /* store as argument */
        break;
      }

      case StNth: {      /* store into a closure */
        int32 ix = PC->fst;
        termPo tos = pop();
        normalPo cl = C_NORMAL(pop());
        cl->args[ix] = tos;
        break;
      }

      case StG: {
        int32 glbNo = PC->fst;
        termPo val = pop();
        globalPo glb = findGlobalVar(glbNo);
        setGlobalVar(glb, val);      // Update the global variable
        break;
      }

      case TG: {
        int32 glbNo = PC->fst;
        termPo val = top();
        globalPo glb = findGlobalVar(glbNo);
        setGlobalVar(glb, val);      // Update the global variable
        break;
      }

      case Thunk: {  // Create a new thunk
        closurePo thLam = C_CLOSURE(pop());

        if (reserveSpace(H, ThunkCellCount) != Ok) {
          saveRegisters();
          retCode ret = gcCollect(H, ThunkCellCount);
          if (ret != Ok)
            return ret;
          restoreRegisters();
        }
        thunkPo thnk = thunkVar(H, thLam);
        push(thnk);       /* put the structure back on the stack */
        break;
      }

      case LdTh: {
        thunkPo thVr = C_THUNK(pop());

        if (thunkIsSet(thVr)) {
          termPo vl = thunkVal(thVr);

          check(vl != Null, "undefined thunk value");

          push(vl);     /* load thunk variable */
          break;
        } else {
          closurePo thLambda = thunkLam(thVr);

          labelPo lb = closureLabel(thLambda);

          if (lb == Null) {
            logMsg(logFile, "label %T not defined", thLambda);
            bail();
          } else if (labelArity(lb) != 1) {
            logMsg(logFile, "closure %T does not have correct arity %d", thLambda, 1);
            bail();
          }

          methodPo mtd = labelCode(lb);       /* set up for object call */

          if (mtd == Null) {
            logMsg(logFile, "no definition for %T", lb);
            bail();
          }
          push(thVr);                                         // Keep the thunk var
          push(closureFree(thLambda));                     // Put the free term back on the stack

          if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
            int root = gcAddRoot(H, (ptrPo) &mtd);
            stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
            gcReleaseRoot(H, root);

#ifdef TRACESTACK
            if (traceStack)
              verifyStack(STK, H);
#endif
          }

          assert(validPC(frameMtd(FP), PC));
          FP->pc = PC + 1;
          pushFrme(mtd);
          LITS = codeLits(mtd);
          incEntryCount(mtd);              // Increment program count
          continue;
        }
      }

      case StTh: {                           // Store into thunk
        thunkPo thnk = C_THUNK(pop());
        termPo val = pop();

        if (thunkIsSet(thnk)) {
          logMsg(logFile, "thunk %T already set", thnk);
          bail();
        }

        setThunk(thnk, val);      // Update the thunk variable
        break;
      }

      case TTh: {                        // Set thunk and carry on
        thunkPo thnk = C_THUNK(pop());
        termPo val = top();

        if (thunkIsSet(thnk)) {
          logMsg(logFile, "thunk %T already set", thnk);
          bail();
        }

        setThunk(thnk, val);      // Update the thunk variable
        break;
      }

      case Cell: {
        checkAlloc(CellCellCount);
        cellPo cell = newCell(H, pop());
        push(cell);
        break;
      }

      case Get: {
        cellPo cell = C_CELL(pop());
        push(getCell(cell));
        break;
      }

      case Assign: {
        cellPo cell = C_CELL(pop());
        termPo vl = pop();
        setCell(cell, vl);
        break;
      }

      case IAdd: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs + Rhs);
        push(Rs);
        break;
      }

      case ISub: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs - Rhs);
        push(Rs);
        break;
      }
      case IMul: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs * Rhs);
        push(Rs);
        break;
      }
      case IDiv: {
        termPo tryIndex = pop();

        assert(isInteger(tryIndex));

        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());
        if (Rhs == 0) {
          push(divZero);
          push(tryIndex);
          goto Exception;
        } else {
          termPo Rs = makeInteger(Lhs / Rhs);
          push(Rs);
          break;
        }
      }
      case IMod: {
        termPo tryIndex = pop();

        assert(isInteger(tryIndex));

        integer denom = integerVal(pop());
        integer numerator = integerVal(pop());

        if (numerator == 0) {
          push(divZero);
          push(tryIndex);
          goto Exception;
        } else {
          integer reslt = denom % numerator;

          termPo Rs = (termPo) makeInteger(reslt);

          push(Rs);
          break;
        }
      }
      case IAbs: {
        termPo Trm = pop();
        integer Arg = integerVal(Trm);

        termPo Rs = (Arg < 0 ? makeInteger(-Arg) : Trm);
        push(Rs);
        break;
      }
      case IEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) == integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case ILt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) < integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case IGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) >= integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case ICmp: {
        termPo i = pop();
        termPo j = pop();

        if (integerVal(i) != integerVal(j)) {
          goto breakPoint;
        }
        break;
      }
      case CEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (charVal(Lhs) == charVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case CLt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (charVal(Lhs) < charVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case CGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (charVal(Lhs) >= charVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case CCmp: {
        termPo i = pop();
        termPo j = pop();

        if (charVal(i) != charVal(j)) {
          goto breakPoint;
        } else
          break;
      }
      case BAnd: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs & (uinteger) Rhs));
        push(Rs);
        break;
      }
      case BOr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger((integer) ((uinteger) Lhs | (uinteger) Rhs));
        push(Rs);
        break;
      }
      case BXor: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs ^ (uinteger) Rhs));
        push(Rs);
        break;
      }
      case BNot: {
        integer Lhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) (~(uinteger) Lhs));
        push(Rs);
        break;
      }
      case BLsl: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs << (uinteger) Rhs));
        push(Rs);
        break;
      }
      case BLsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) (((uinteger) Lhs) >> ((uinteger) Rhs)));
        push(Rs);
        break;
      }
      case BAsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((Lhs) >> Rhs);
        push(Rs);
        break;
      }
      case FAdd: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs + Rhs);
        push(Rs);
        break;
      }
      case FSub: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs - Rhs);
        push(Rs);
        break;
      }
      case FMul: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs * Rhs);
        push(Rs);
        break;
      }
      case FDiv: {
        termPo tryIndex = pop();

        assert(isInteger(tryIndex));

        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        if (Rhs == 0.0) {
          push(divZero);
          push(tryIndex);
          goto Exception;
        } else {
          termPo Rs = makeFloat(Lhs / Rhs);
          push(Rs);
          break;
        }
      }
      case FMod: {
        termPo tryIndex = pop();
        assert(isInteger(tryIndex));

        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());
        if (Rhs == 0.0) {
          push(divZero);
          push(tryIndex);
          goto Exception;
        } else {
          termPo Rs = makeFloat(fmod(Lhs, Rhs));
          push(Rs);
          break;
        }
      }
      case FAbs: {
        double Lhs = floatVal(pop());

        termPo Rs = makeFloat(fabs(Lhs));
        push(Rs);
        break;
      }
      case FEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (nearlyEqual(floatVal(Lhs), floatVal(Rhs), floatVal(Rhs) / 1.0e20) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case FLt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (floatVal(Lhs) < floatVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case FGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (floatVal(Lhs) >= floatVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }
      case FCmp: {
        termPo x = pop();
        termPo y = pop();

        if (floatVal(x) != floatVal(y)) {
          goto breakPoint;
        } else
          break;
      }

      case Case: {      /* case instruction */
        int32 mx = PC->fst;

        termPo tos = top();
        integer hx = hashTerm(tos) % mx;

        PC = PC + hx + 1;
        continue;
      }

      case IndxJmp: {    // Branch based on index of constructor term
        int32 mx = PC->fst;
        normalPo top = C_NORMAL(top());
        labelPo lbl = termLbl(top);
        integer hx = labelIndex(lbl);

        PC = PC + hx + 1;
        continue;
      }

      case Closure: {      /* heap allocate closure */
        if (reserveSpace(H, ClosureCellCount) != Ok) {
          saveRegisters();
          retCode ret = gcCollect(H, ClosureCellCount);
          if (ret != Ok)
            return ret;
          restoreRegisters();
        }
        labelPo cd = C_LBL(nthElem(LITS, PC->fst));

        if (!labelDefined(cd)) {
          logMsg(logFile, "label %L not defined", cd);
          bail();
        }

        closurePo cl = newClosure(H, cd, pop());

        push(cl);       /* put the closure back on the stack */
        break;
      }

      case Alloc: {      /* heap allocate term */
        labelPo lbl = C_LBL(nthElem(LITS, PC->fst));
        integer arity = labelArity(lbl);

        if (enoughRoom(H, lbl) != Ok) {
          saveRegisters();
          retCode ret = gcCollect(H, NormalCellCount(arity));
          if (ret != Ok)
            return ret;
          restoreRegisters();
        }
        normalPo cl = allocateStruct(H, lbl); /* allocate a closure on the heap */
        for (int ix = 0; ix < arity; ix++)
          cl->args[ix] = pop();   /* fill in free variables by popping from stack */
        push(cl);       /* put the structure back on the stack */
        break;
      }

      case Cmp: {
        termPo i = pop();
        termPo j = pop();

        if (!sameTerm(i, j))
          goto breakPoint;
        else
          break;
      }

      case If: {
        termPo i = pop();

        if (sameTerm(i, trueEnum)) {
          goto breakPoint;
        } else
          break;
      }

      case IfNot: {
        termPo i = pop();

        if (!sameTerm(i, trueEnum)) {
          goto breakPoint;
        } else
          break;
      }

      case Frame: {
#ifdef TRACESTACK
        termPo frame = nthElem(LITS, PC->fst);
        if (stackVerify) {
          int32 frameDepth;
          if (isString(frame)) {
            integer sigLen;
            const char *sig = strVal(frame, &sigLen);
            tryRet(typeSigArity(sig, sigLen, &frameDepth));
          } else
            frameDepth = integerVal(frame);
          if (frameDepth != stackDepth(STK, frameMtd(FP), SP, FP)) {
            logMsg(logFile, "stack depth: %d does not match frame signature %T", stackDepth(STK, frameMtd(FP), SP, FP),
                   frame);
            bail();
          }
        }
#else
        PC += 2; // ignore frame entity for now
#endif
        break;
      }

      case dBug: {
        if (lineDebugging) {
          saveRegisters();
          FP->pc++;                   // We aim to continue at the next instruction
          enterDebug(P);
          restoreRegisters();
        }
        continue;
      }

      default:
      case illegalOp:
        syserr("Illegal instruction");
    }
    PC++;
  }
}
