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
#include <math.h>
#include "cellP.h"
#include "closureP.h"
#include "singleP.h"
#include "errorCodes.h"
#include "ltype.h"

logical collectStats = False;

#ifdef TRACESTACK
#define checkAlloc(Count) STMT_WRAP({  \
  if (reserveSpace(H, Count) != Ok) {  \
    saveRegisters();                   \
    retCode ret = gcCollect(H, Count); \
    if (ret != Ok)                     \
      return ret;                      \
    if (traceStack > noTracing){       \
      verifyStack(P->stk, H);             \
      verifyHeap(H);                   \
    }                                  \
    restoreRegisters();                \
    check(reserveSpace(H,Count)==Ok,"could not reserve space");\
  }                                    \
})
#else
#define checkAlloc(Count) STMT_WRAP({  \
  if (reserveSpace(H, Count) != Ok) {  \
    saveRegisters();                   \
    retCode ret = gcCollect(H, Count); \
    if (ret != Ok)                     \
      return ret;                      \
    restoreRegisters();                \
    check(reserveSpace(H,Count)==Ok,"could not reserve space");\
  }                                    \
})
#endif

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

#ifndef NDEBUG
    if (collectStats)
      countOp(PC->op);
#endif

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

        PC++;

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          int root = gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);
          assert(stackRoom(stackDelta(mtd) + STACKFRAME_SIZE));
#ifdef TRACESTACK
          if (traceStack > noTracing)
            verifyStack(STK, H);
#endif
        }
        assert(validPC(frameMtd(FP), PC));
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
        int32 arity = PC->fst;
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

        PC++;

        push(closureFree(obj));                     // Put the free term back on the stack

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
#ifdef TRACESTACK
          if (traceStack >= detailedTracing)
            logMsg(logFile, "growing stack due to overflow in OCall");
#endif
          int root = gcAddRoot(H, (ptrPo) &mtd);
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));
          gcReleaseRoot(H, root);

#ifdef TRACESTACK
          if (traceStack > noTracing)
            verifyStack(STK, H);
#endif
        }

        assert(validPC(frameMtd(FP), PC));
        FP->pc = PC;
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
        int32 arity = labelArity(lbl);

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
        int32 arity = PC->fst;
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
        integer height = PC->fst;
        assert(height >= 0);
        assert(height==lclCount(frameMtd(FP)));
        SP = ((ptrPo) FP) - height;
        for (integer ix = 0; ix < height; ix++)
          SP[ix] = voidEnum;
        break;
      };

      case Ret: {        /* return from function */
        termPo retVal = *SP;     /* return value */

        assert(FP < baseFrame(STK));

        ptrPo tgtSp = &arg(argCount(frameMtd(FP)));
        tryFramePo try = STK->try;

        if ((ptrPo) try >= SP && (ptrPo) try < tgtSp) {
          while ((ptrPo) try < tgtSp) {
            check(try->fp == FP, "misaligned try block");
            try = try->try;
          }
          STK->try = try;
        }

        SP = tgtSp; // Just above arguments to current call
        FP = FP->fp;
        PC = FP->pc;
        LITS = FP->pool;

        push(retVal);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Block: {
        break;
      }

      case Break: {
        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        PC += PC->alt + 1;
        continue;
      }

      case Loop: {
        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        break;
      }

      case Result: { /* return a value from a block */
        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        PC += PC->alt + 1;

        continue;       /* and carry after reset block */
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
        int32 cnt = PC->fst;
        termPo tmp = SP[0];

        for (int32 ix = 0; ix < cnt; ix++) {
          SP[ix] = SP[ix + 1];
        }
        SP[cnt] = tmp;
        break;
      }

      case Pick: {       // Reset stack, keeping top elements
        int32 depth = PC->fst;
        int32 keep = PC->alt;

        ptrPo src = &SP[keep];
        ptrPo tgt = &SP[depth + keep];

        for (int32 ix = 0; ix < keep; ix++) {
          *--tgt = *--src;
        }
        SP = &SP[depth];
        assert(SP == tgt);
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
        closurePo lambda = C_CLOSURE(pop());
        saveRegisters();
        stackPo child = splitStack(P, lambda);

        P->stk = attachStack(P->stk, child);
#ifdef TRACESTACK
        if (traceStack > noTracing)
          verifyStack(P->stk, H);
#endif
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
          PC++;
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
          PC++;
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
          if (traceStack > noTracing)
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

      case Try: {
        assert(validPC(frameMtd(FP), PC + PC->alt + 1));
        check(stackRoom(TryFrameCellCount), "unexpected stack overflow");

        saveRegisters();
        integer tryIndex = pushTryFrame(STK, P, PC + PC->alt + 1, SP, FP);
        restoreRegisters();
        push(makeInteger(tryIndex));
#ifdef TRACESTACK
        if (traceStack >= detailedTracing)
          logMsg(logFile, "%ld: entering try scope %ld (%d)", pcCount, tryIndex, tryStackSize(P));
#endif
        break;
      }

      case EndTry: {
        integer tryIndex = integerVal(pop());

#ifdef TRACESTACK
        if (traceStack >= detailedTracing)
          logMsg(logFile, "%ld: leaving try scope %ld (%d)", pcCount, tryIndex, tryStackSize(P));
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
        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block || PC->op == Try);
        PC += PC->alt + 1;
        continue;
      }

      case TryRslt: {
        integer tryIndex = integerVal(pop());
        termPo val = pop();

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
        push(val);

        PC += PC->alt + 1;
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block || PC->op == Try);
        PC += PC->alt + 1;
        continue;
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

      case LdS: {                /* duplicate a stack element */
        termPo tos = SP[PC->fst];
        *--SP = tos;
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
        termPo t = pop();

        if (!sameTerm(l, t)) {
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
        } else
          break;
      }

      case CLbl: {
        labelPo l = C_LBL(nthElem(LITS, PC->fst));
        termPo t = pop();

        if (isNormalPo(t)) {
          normalPo cl = C_NORMAL(t);
          if (sameLabel(l, termLbl(cl)))
            break;
        }
        PC += PC->alt + 1;  // First jump to the block
        assert(validPC(frameMtd(FP), PC));
        assert(PC->op == Block);
        PC += PC->alt + 1;
        continue;
      }

      case Nth: {
        int32 ix = PC->fst;  /* which element */
        termPo t = pop();

        normalPo cl = C_NORMAL(t);  /* which term? */
        push(nthElem(cl, ix));

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

      case StNth: {      /* store into a closure */
        int32 ix = PC->fst;
        normalPo cl = C_NORMAL(pop());
        termPo tos = pop();
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

      case Sav: {  // Create a new single assignment variable
        checkAlloc(SingleCellCount);
        singlePo sav = singleVar(H);
        push(sav);       /* put the structure back on the stack */
        break;
      }

      case TstSav: {
        singlePo savVr = C_SINGLE(pop());

        termPo Rs = (singleIsSet(savVr) ? trueEnum : falseEnum);
        push(Rs);
        break;
      }

      case LdSav: {
        singlePo savVr = C_SINGLE(pop());

        if (singleIsSet(savVr)) {
          termPo vl = singleVal(savVr);

          check(vl != Null, "undefined single assignment value");

          push(vl);     /* load single variable */
          PC++;
          continue;
        } else {
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
        }
      }

      case StSav: {                           // Store into single
        singlePo sav = C_SINGLE(pop());
        termPo val = pop();

        if (singleIsSet(sav)) {
          logMsg(logFile, "single %T already set", sav);
          bail();
        }

        setSingle(sav, val);      // Update the single variable
        break;
      }

      case TSav: {                        // Set single and carry on
        singlePo sav = C_SINGLE(pop());
        termPo val = top();

        if (singleIsSet(sav)) {
          logMsg(logFile, "single %T already set", sav);
          bail();
        }

        setSingle(sav, val);      // Update the single variable
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
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
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
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
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
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
        } else
          break;
      }

      case Case: {      /* case instruction */
        int32 mx = PC->fst;

        termPo tos = pop();
        integer hx = hashTerm(tos) % mx;

        PC = PC + hx + 1;
        continue;
      }

      case IndxJmp: {    // Branch based on index of constructor term
        int32 mx = PC->fst;
        normalPo top = C_NORMAL(pop());
        labelPo lbl = termLbl(top);
        integer hx = labelIndex(lbl);

        PC = PC + hx + 1;
        continue;
      }

      case Closure: {      /* heap allocate closure */
        checkAlloc(ClosureCellCount);
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
        int32 arity = labelArity(lbl);

        checkAlloc(NormalCellCount(arity));
        normalPo cl = allocateStruct(H, lbl); /* allocate a closure on the heap */
        for (int32 ix = 0; ix < arity; ix++)
          cl->args[ix] = pop();   /* fill in free variables by popping from stack */
        push(cl);       /* put the structure back on the stack */
        break;
      }

      case Cmp: {
        termPo i = pop();
        termPo j = pop();

        if (!sameTerm(i, j)) {
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
        } else
          break;
      }

      case If: {
        termPo i = pop();

        if (sameTerm(i, trueEnum)) {
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
        } else
          break;
      }

      case IfNot: {
        termPo i = pop();

        if (!sameTerm(i, trueEnum)) {
          PC += PC->alt + 1;
          assert(validPC(frameMtd(FP), PC));
          assert(PC->op == Block);
          PC += PC->alt + 1;
          continue;
        } else
          break;
      }

      case Frame: {
#ifdef TRACESTACK
        if (stackVerify) {
          termPo frame = nthElem(LITS, PC->fst);
          int32 frameDepth;
          if (isString(frame)) {
            integer sigLen;
            const char *sig = strVal(frame, &sigLen);
            tryRet(typeSigArity(sig, sigLen, &frameDepth));
          } else
            frameDepth = (int32) integerVal(frame);
          if (frameDepth != stackDepth(STK, frameMtd(FP), SP, FP)) {
            logMsg(logFile, "stack depth: %d does not match frame signature %T",
                   stackDepth(STK, frameMtd(FP), SP, FP),
                   frame);
            bail();
          }
        }
#endif
        break;
      }

      case dBug: {
        if (lineDebugging) {
          saveRegisters();
          FP->pc++;                   // We aim to continue at the next instruction
          enterDebug(P);
          restoreRegisters();
          continue;
        } else
          break;
      }

      default:
      case illegalOp:
        syserr("Illegal instruction");
    }
    PC++;
  }
}
