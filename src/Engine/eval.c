/*
 * Run-time evaluation for CAFE programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, a JIT
 * process will generate native instructions from CAFE instructions.
 */

#include "config.h"

#include <debug.h>
#include <globals.h>
#include "engineP.h"
#include "arithP.h"

#define collectI32(pc) (hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32))
#define collectOff(pc) (hi32 = collectI32(pc), (pc)+hi32)

#define push(X) *--SP = ((termPo)(X))
#define pop() (*SP++)
#define top() (*SP)

#define local(off) (((ptrPo)FP)-(off))
#define arg(off) (((ptrPo)(FP+1))+(off)-1)

#define saveRegisters(P) { (P)->pc = PC; (P)->fp = FP; (P)->prog = PROG; (P)->sp = SP;}
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

  register uint32 hi32, lo32;    /* Temporary registers */

#ifdef TRACEEXEC
  integer pcCount = 0;       /* How many instructions executed so far? */
#endif

  for (;;) {
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    countIns(*PC);
    if (insDebugging) {
      saveRegisters(P);
      insDebug(pcCount, P);
      restoreRegisters(P);
    } else if (lineDebugging) {
      switch (*PC) {
        case Call: saveRegisters(P);
          callDebug(P, insLit(P));
          restoreRegisters(P);
          break;
        case OCall: saveRegisters(P);
          callDebug(P, SP[1]);
          restoreRegisters(P);
          break;
        case Tail: saveRegisters(P);
          tailDebug(P, insLit(P));
          restoreRegisters(P);
          break;
        case OTail: saveRegisters(P);
          tailDebug(P, SP[1]);
          restoreRegisters(P);
          break;
        case Ret: saveRegisters(P);
          retDebug(P, top());
          restoreRegisters(P);
          break;
        case Line: saveRegisters(P);
          lineDebug(P, insLit(P));
          restoreRegisters(P);
          break;
        default:;
      }
    }
#endif

    switch ((OpCode) (*PC++)) {
      case Halt:
        return Ok;

      case Call: {
        termPo nProg = nthArg(LITS, collectI32(PC));

        push(PROG);
        PROG = labelCode(C_LBL(nProg));   // Which program do we want?
        push(PC);       // Set up for a return
        PC = entryPoint(PROG);
        LITS = codeLits(PROG);
#ifdef TRACEEXEC
        P->hasEnter = False;
#endif
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
#ifdef TRACEEXEC
        P->hasEnter = False;
#endif
        continue;
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */
        escapePo esc = getEscape(escNo);
        ReturnStatus ret = esc->fun(P, SP);  /* invoke the escape */
        SP += esc->arity;     /* drop arguments */
        switch (ret.ret) {
          case Ok:
            if (ret.rslt != Null)
              *--SP = ret.rslt;
            continue;
          case Error:
            goto raiseError;
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
#ifdef TRACEEXEC
        P->hasEnter = False;
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
#ifdef TRACEEXEC
        P->hasEnter = False;
#endif
        continue;       /* Were done */
      }

      case Enter: {      /* set up the local env of locals */
        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        int32 lclCnt = collectI32(PC);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        P->hasEnter = True;
        if (insDebugging)
          for (integer ix = 0; ix < lclCnt; ix++)
            SP[0] = voidEnum;
#endif
        continue;
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

        if (isNormalPo(t)) {
          normalPo cl = C_TERM(t);
          if (!sameTerm(l, (termPo) termLbl(cl)))
            PC = exit;
        } else if (!sameTerm(t, l))
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
          saveRegisters(P);
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
        if (!sameTerm(i, j))
          PC = exit;
        continue;
      }

      case Bf: {       /* Branch on false */
        termPo i = pop();
        insPo exit = collectOff(PC);
        if (i == falseEnum)
          PC = exit;
        continue;
      }

      case Bt: {        /* Branch on true */
        termPo i = pop();
        insPo exit = collectOff(PC);

        if (i == trueEnum)
          PC = exit;
        continue;
      }

      case Rais:
      raiseError:
        syserr("problem");

      case Frame:
        PC += 2;
        continue;

      case Line: {
        PC += 2;

        continue;
      }

      default:
      case illegalOp:
        syserr("Illegal instruction");
    }
  }
}

ptrPo localVar(framePo fp, int64 off) {
  return &(((ptrPo) fp)[-off]);
}
