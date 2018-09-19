/*
 * Run-time evaluation for STAR programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, a JIT
 * process will generate native instructions from STAR instructions.
 */

#include "config.h"

#include <debug.h>
#include <globals.h>
#include <turm.h>
#include "engineP.h"

#define collectI32(pc) (hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<(unsigned)16)|lo32))
#define collectOff(pc) (hi32 = collectI32(pc), (pc)+(signed)hi32)

static inline ptrPo checkStack(processPo P, ptrPo SP) {
  assert(SP > (ptrPo) P->stackBase);
  return SP;
}

#define push(X) *checkStack(P,--SP) = ((termPo)(X))
#define pop() (*SP++)
#define top() (*SP)

#define local(off) (((ptrPo)FP)-(off))
#define arg(off) (((ptrPo)(FP+1))+(off))

#define saveRegisters(P, SP) { (P)->pc = PC; (P)->fp = FP; (P)->prog = PROG; (P)->sp = (SP);}
#define restoreRegisters(P) { PC = (P)->pc; FP = (P)->fp; PROG=(P)->prog; SP=(P)->sp; LITS=codeLits(PROG);}

/*
 * Execute program on a given process/thread structure
 */
retCode run(processPo P) {
  heapPo heap = P->heap;
  register insPo PC = P->pc;    /* Program counter */
  register framePo FP = P->fp;    /* Current locals + = arguments, - = locals */
  register methodPo PROG = P->prog; /* Current executing closure */
  register normalPo LITS = codeLits(PROG); /* pool of literals */

  register ptrPo SP = P->sp;         /* Current 'top' of stack (grows down) */

  // Set up a call to the entry point


  register uint32 hi32, lo32;    /* Temporary registers */

  for (;;) {
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    countIns(*PC);
    if (insDebugging) {
      saveRegisters(P, SP);
      insDebug(P, pcCount, *PC);
      restoreRegisters(P);
    }
#endif

    switch ((OpCode) (*PC++)) {
      case Halt:
        return Ok;

      case Abort: {
        termPo msg = pop();

        logMsg(logFile, "Abort %T", msg);
        saveRegisters(P,SP);
        dumpStackTrace(P, logFile);
        return Error;
      }

      case Call: {
        termPo nProg = nthArg(LITS, collectI32(PC));

        push(PROG);
        PROG = labelCode(C_LBL(nProg));   // Which program do we want?
        push(PC);       // Set up for a return
        PC = entryPoint(PROG);
        LITS = codeLits(PROG);

        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        integer lclCnt = lclCount(PROG);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif
        if (SP - stackDelta(PROG) <= (ptrPo) P->stackBase) {
          saveRegisters(P, SP);
          extendStack(P, 2);
          restoreRegisters(P);
        }
        assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);
        continue;
      }

      case OCall: {        /* Call tos a1 .. an -->   */
        int arity = collectI32(PC);
        termPo nProg = SP[0];

        push(PROG);
        push(PC);       /* build up the frame. */
        labelPo oLbl = isNormalPo(nProg) ? termLbl(C_TERM(nProg)) : C_LBL(nProg);
        PROG = labelCode(objLabel(oLbl, arity));       /* set up for object call */
        PC = entryPoint(PROG);
        LITS = codeLits(PROG);

        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        integer lclCnt = lclCount(PROG);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif
        if (SP - stackDelta(PROG) <= (ptrPo) P->stackBase) {
          saveRegisters(P, SP);
          extendStack(P, 2);
          restoreRegisters(P);
        }
        assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);
        continue;
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */
        escapePo esc = getEscape(escNo);
        saveRegisters(P, SP + esc->arity);
        ReturnStatus ret = esc->fun(P, SP);  /* invoke the escape */
        restoreRegisters(P);
        switch (ret.ret) {
          case Ok:
            if (ret.rslt != Null)
              *--SP = ret.rslt;
            continue;
          case Error:
            return Error;
          default:
            continue;
        }
      }

      case Tail: {       /* Tail call of explicit program */
        termPo nProg = nthArg(LITS, collectI32(PC));

        // Pick up existing frame
        framePo oldFp = FP->fp;
        insPo oldRtn = FP->rtn;
        methodPo oldPROG = FP->prog;
        integer oldArgCnt = argCount(PROG);

        labelPo prog = C_LBL(nProg);

        PROG = labelCode(prog);   // Which program do we want?

        assert(PROG != Null);

        // slide new arguments over old frame
        integer argCnt = labelArity(prog);  /* prepare to slide arguments over caller */

        ptrPo tgt = &FP->args[oldArgCnt];
        ptrPo src = SP + argCnt;                  /* base of argument vector */

        for (int ix = 0; ix < argCnt; ix++)
          *--tgt = *--src;    /* copy the argument vector */

        FP = oldFp;
        SP = tgt;

        // set up new frame
        push(oldPROG);
        push(oldRtn);            /* make sure we return where the caller returns */

        PC = entryPoint(PROG);
        LITS = codeLits(PROG);

        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        integer lclCnt = lclCount(PROG);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif
        continue;       /* Were done */
      }

      case OTail: {       /* Tail call */
        int arity = collectI32(PC);
        termPo nProg = SP[0];

        // Pick up existing frame
        framePo oldFp = FP->fp;
        insPo oldRtn = FP->rtn;
        methodPo oldPROG = FP->prog;
        integer oldArgCnt = argCount(PROG);

        labelPo oLbl = isNormalPo(nProg) ? termLbl(C_TERM(nProg)) : C_LBL(nProg);
        PROG = labelCode(objLabel(oLbl, arity));       /* set up for object call */

        // slide new arguments over old frame
        integer argCnt = argCount(PROG);  /* prepare to slide arguments over caller */

        ptrPo tgt = &FP->args[oldArgCnt];
        ptrPo src = SP + argCnt;                  /* base of argument vector */

        for (int ix = 0; ix < argCnt; ix++)
          *--tgt = *--src;    /* copy the argument vector */

        FP = oldFp;
        SP = tgt;

        // set up new frame
        push(oldPROG);
        push(oldRtn);            /* make sure we return where the caller returns */

        PC = entryPoint(PROG);
        LITS = codeLits(PROG);

        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        integer lclCnt = lclCount(PROG);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif
        continue;       /* Were done */
      }

      case Ret: {        /* return from function */
        termPo ret = *SP;     /* return value */

        int64 argCnt = argCount(PROG);

        SP = (ptrPo) FP;     /* reset stack */

        FP = (framePo) (*SP++);
        PC = (insPo) (*SP++);
        PROG = (methodPo) (*SP++);

        LITS = codeLits(PROG);   /* reset pointer to code literals */

        SP += argCnt;

        push(ret);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Jmp:       /* jump to local offset */
        PC = collectOff(PC);
        assert(validPC(PROG, PC));
        continue;

      case Drop:
        SP++;       /* drop tos */
        continue;

      case Dup: {        /* duplicate tos */
        termPo tos = *SP;
        *--SP = tos;
        continue;
      }

      case Pull: {
        int32 offset = collectI32(PC);    // How far down to reach stack
        termPo se = SP[offset];
        *--SP = se;
        continue;
      }

      case Rot: {
        int32 offset = collectI32(PC);    // How far down to reach into the stack
        termPo se = SP[offset - 1];
        for (int32 ix = offset - 1; ix > 0; ix--)
          SP[ix] = SP[ix - 1];
        *SP = se;
        continue;
      }

      case Rst: {
        int32 offset = collectI32(PC);
        assert(offset >= 0);
        SP = (ptrPo) FP - lclCount(PROG) - offset;
        continue;
      }

      case LdC:     /* load literal value from pool */
        push(nthArg(LITS, collectI32(PC)));
        continue;

      case LdA: {
        int32 offset = collectI32(PC);
        ptrPo src = arg(offset);
        push(*src);     /* load argument */
        continue;
      }

      case LdL: {
        int32 offset = collectI32(PC);
        ptrPo src = local(offset);
        push(*src);     /* load local */
        continue;
      }

      case LdG: {
        int32 glbNo = collectI32(PC);
        globalPo glb = getGlobalVar(glbNo);
        push(getGlobal(glb));     /* load a global variable */
        continue;
      }

      case CLbl: {
        termPo l = pop();
        termPo t = pop();
        insPo exit = collectOff(PC);
        assert(validPC(PROG, exit));

        if (isNormalPo(t)) {
          normalPo cl = C_TERM(t);
          if (sameTerm(l, (termPo) termLbl(cl)))
            PC = exit;
        } else if (sameTerm(t, l))
          PC = exit;
        continue;
      }

      case Nth: {
        int32 ix = collectI32(PC);  /* which element */
        normalPo cl = C_TERM(pop());  /* which term? */
        push(cl->args[ix]);
        continue;
      }

      case StL: {
        int32 offset = collectI32(PC);
        ptrPo dest = local(offset);
        *dest = pop();
        continue;
      }

      case TL: {
        int32 offset = collectI32(PC);
        ptrPo dest = local(offset);
        *dest = top();
        continue;
      }

      case StA: {
        int32 offset = collectI32(PC);
        ptrPo dest = arg(offset);
        *dest = (termPo) pop();     /* store as argument */
        continue;
      }

      case StNth: {      /* store into a closure */
        int32 ix = collectI32(PC);
        termPo tos = pop();
        normalPo cl = C_TERM(pop());
        cl->args[ix] = tos;
        continue;
      }

      case StG: {
        int32 glbNo = collectI32(PC);
        termPo val = pop();
        globalPo glb = getGlobalVar(glbNo);
        setGlobalVar(glb, val);      // Update the global variable
        continue;
      }

      case Case: {      /* case instruction */
        int32 mx = collectI32(PC);
        termPo tos = top();
        integer hx = hashTermLbl(tos) % mx + 1;

        PC = (insPo) ((void *) PC + (sizeof(insWord) * 3) * hx);
        continue;
      }

      case Alloc: {      /* heap allocate term */
        labelPo cd = C_LBL(nthArg(LITS, collectI32(PC)));
        if (enoughRoom(heap, cd) != Ok) {
          saveRegisters(P, SP);
          retCode ret = gcCollect(heap, NormalCellCount(cd->arity));
          if (ret != Ok)
            return ret;
          restoreRegisters(P);
        }
        normalPo cl = allocateStruct(heap, cd); /* allocate a closure on the heap */
        for (int ix = 0; ix < cd->arity; ix++)
          cl->args[ix] = pop();   /* fill in free variables by popping from stack */
        push(cl);       /* put the closure back on the stack */
        continue;
      }

      case Cmp: {
        termPo i = pop();
        termPo j = pop();
        insPo exit = collectOff(PC);
        assert(validPC(PROG, exit));

        if (!sameTerm(i, j))
          PC = exit;
        continue;
      }

      case Bf: {       /* Branch on false */
        termPo i = pop();
        insPo exit = collectOff(PC);
        assert(validPC(PROG, exit));

        if (i == falseEnum)
          PC = exit;
        continue;
      }

      case Bt: {        /* Branch on true */
        termPo i = pop();
        insPo exit = collectOff(PC);
        assert(validPC(PROG, exit));

        if (i == trueEnum)
          PC = exit;
        continue;
      }

      case Frame:
        PC += 2;
        continue;

      case dLine: {
#ifdef TRACEEXEC
        if (lineDebugging) {
          termPo line = nthArg(LITS, collectI32(PC));
          saveRegisters(P, SP);
          lineDebug(P, line);
          restoreRegisters(P);
        } else
#endif
          PC += 2;
        continue;
      }

      case dCall: {
#ifdef TRACEEXEC
        if (lineDebugging) {
          termPo callee = nthArg(LITS, collectI32(PC));
          saveRegisters(P, SP);
          callDebug(P, callee);
          restoreRegisters(P);
        } else
#endif
          PC += 2;
        continue;
      }

      case dOCall:
#ifdef TRACEEXEC
        if (lineDebugging) {
          termPo callee = getLbl(SP[0], collectI32(PC));

          saveRegisters(P, SP);
          callDebug(P, callee);
          restoreRegisters(P);
        } else
#endif
          PC += 2;
        continue;

      case dTail: {
#ifdef TRACEEXEC
        if (lineDebugging) {
          termPo callee = nthArg(LITS, collectI32(PC));
          saveRegisters(P, SP);
          tailDebug(P, callee);
          restoreRegisters(P);
        } else
#endif
          PC += 2;
        continue;
      }

      case dOTail:
#ifdef TRACEEXEC
        if (lineDebugging) {
          termPo callee = getLbl(SP[0], collectI32(PC));
          saveRegisters(P, SP);
          tailDebug(P, callee);
          restoreRegisters(P);
        } else
#endif
          PC += 2;
        continue;

      case dRet:
#ifdef TRACEEXEC
        if (lineDebugging) {
          saveRegisters(P, SP);
          retDebug(P, SP[0]);
          restoreRegisters(P);
        }
#endif
        continue;

      default:
      case illegalOp:
        syserr("Illegal instruction");
    }
  }
}
