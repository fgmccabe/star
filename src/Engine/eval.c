/*
 * Run-time evaluation for CAFE programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, a JIT
 * process will generate native instructions from CAFE instructions.
 */

#include "config.h"

#include <debug.h>
#include "engineP.h"
#include "escape.h"      /* escape call handling */
#include "arithP.h"

#define collectI32(pc) (hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32))

#define collectOff(pc) (hi32 = collectI32(pc)/sizeof(uint16), (pc)+(int32)hi32)

#define push(X) *--SP = ((termPo)(X))
#define pop() (*SP++)

#define local(off) &(((ptrPo)FP)[-(off)-1])
#define arg(off) (((ptrPo)(FP+1))+(off))
#define free(off) (ENV->free[off])

/*
 * Execute program on a given process/thread structure
 */
retCode run(processPo P, heapPo heap) {
  register insPo PC = P->pc;    /* Program counter */
  register framePo FP = P->fp;    /* Current locals + = arguments, - = locals */
  register methodPo PROG = P->prog; /* Current executing closure */
  register constantPo LITS = codeLiterals(PROG); /* pool of literals */

  register ptrPo SP = P->sp;         /* Current 'top' of stack (grows down) */

  register uint32 hi32, lo32;    /* Temporary registers */

#ifdef TRACEEXEC
  integer pcCount = 0;       /* How many instructions executed so far? */
#endif

  for (;;) {
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    if (tracing)
      debug_stop(pcCount, P, PROG, PC, FP, SP);
#endif

    switch (*PC++) {
      case Halt:
        return Ok;

      case Call: {
        PROG = C_MTD(LITS[collectI32(PC)].data);   // Which program do we want?
        push(PC);       // Set up for a return
        PC = entryPoint(PROG);
        LITS = codeLiterals(PROG);
        continue;
      }

      case OCall: {        /* Call tos a1 .. an -->   */
        PROG = C_MTD(*SP++);       /* set up for callee */
        push(PC);       /* build up the frame. */
        PC = entryPoint(PROG);
        LITS = codeLiterals(PROG);
        continue;
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */
        escapePo esc = (escapePo) LITS[escNo].data;
        termPo ret = (termPo) esc->fun(P, SP);  /* invoke the escape */
        SP += esc->arity;     /* drop arguments */
        *--SP = ret;
        continue;
      }

      case Tail: {       /* Tail call of explicit program */
        // Pick up existing frame
        framePo oldFp = FP->fp;
        insPo oldPc = FP->rtn;
        int64 argCnt = argCount(PROG); /* How many arguments in caller? */

        PROG = C_MTD(LITS[collectI32(PC)].data);   // Which program do we want?

        // slide new arguments over old frame
        int64 nArgCnt = argCount(PROG);  /* prepare to slide arguments over caller */

        ptrPo tgt = arg(argCnt);
        ptrPo src = SP + nArgCnt + 1;   /* base of argument vector */

        FP = oldFp;

        for (int ix = 0; ix < nArgCnt; ix++)
          *--tgt = *--src;    /* copy the argument vector */
        SP = tgt;

        // set up new frame
        *--SP = (termPo) PROG;
        *--SP = (termPo) oldPc;  /* make sure we return where the caller returns */

        PC = entryPoint(PROG);
        LITS = codeLiterals(PROG);
        continue;       /* Were done */
      }

      case OTail: {       /* Tail call */
        // Pick up existing frame
        framePo oldFp = FP->fp;
        insPo oldPc = FP->rtn;
        int64 argCnt = argCount(PROG); /* How many arguments in caller? */

        PROG = C_MTD(*SP++);       /* set up for callee */

        // slide new arguments over old frame
        int64 nArgCnt = argCount(PROG);  /* prepare to slide arguments over caller */

        ptrPo tgt = arg(argCnt);
        ptrPo src = SP + nArgCnt + 1;   /* base of argument vector */

        FP = oldFp;

        for (int ix = 0; ix < nArgCnt; ix++)
          *--tgt = *--src;    /* copy the argument vector */
        SP = tgt;

        // set up new frame
        *--SP = (termPo) PROG;
        *--SP = (termPo) oldPc;  /* make sure we return where the caller returns */

        PC = entryPoint(PROG);
        LITS = codeLiterals(PROG);
        continue;       /* Were done */
      }

      case Enter: {      /* set up the local env of locals */
        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        int32 lclCnt = collectI32(PC);  /* How many locals do we have */
        SP -= lclCnt;
        continue;
      }

      case Ret: {        /* return from function */
        int64 argCnt = argCount(PROG);
        termPo ret = *SP;     /* and return value */

        SP = arg(argCnt);     /* reset stack */

        PC = FP->rtn;     /* and return address */
        FP = FP->fp;      /* and old frame pointer */

        PROG = FP->prog;      /* pick up parent code */
        LITS = codeLiterals(PROG);   /* reset pointer to code literals */

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

      case LdC:     /* load literal value from pool */
        push(LITS[collectI32(PC)].data);
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

      case Nth: {
        int32 ix = collectI32(PC);  /* which element */
        normalPo cl = C_TERM(pop());  /* which term? */
        push(cl->args[ix]);
        continue;
      }

      case StL: {
        int32 offset = collectI32(PC);
        ptrPo dest = local(offset);
        *dest = (termPo) pop();
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

      case Case: {      /* case instruction */
        int32 mx = collectI32(PC);
        int32 ix = (int32) pop();    /* tos has an index */

        if (ix > 0 && ix < mx)     /* size of a jump instruction */
          PC = (insPo) ((void *) PC + (sizeof(insPo *) + sizeof(int32)));
        /* otherwise default to next instruction */

        continue;
      }

      case Alloc: {      /* heap allocate term */
        labelPo cd = C_LBL(LITS[collectI32(PC)].data);
        normalPo cl = allocateStruct(heap, cd); /* allocate a closure on the heap */
        for (int ix = 0; ix < cd->arity; ix++)
          cl->args[ix] = pop();   /* fill in free variables by popping from stack */
        push(cl);       /* put the closure back on the stack */
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

      case Cas: {        /* compare and swap, branch if not zero */
        integer nw = integerVal(pop());     /* new value */
        integer old = integerVal(pop());    /* compare value */
        normalPo p = C_TERM(pop()); /* lock */
        insPo exit = collectOff(PC);

        if (!compare_and_swap(p, old, nw))
          PC = exit;
        continue;
      }

      default:
      case illegalOp:
        syserr("Illegal instruction");
    }
  }
}

logical compare_and_swap(normalPo cl, int64 old, int64 nw) {
  integer *check = &(C_INT(cl->args[0]))->ix;
  return __sync_bool_compare_and_swap(check, old, nw);
}

termPo localVar(framePo fp, int64 off) {
  return &(((termPo) fp)[-off - 1]);
}
