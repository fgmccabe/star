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



#define collectI32(pc) (hi32 = (uint32)(*pc++), lo32 = *pc++, ((hi32<<16)|lo32))

#define collectOff(pc) (hi32 = collectI32(pc)/sizeof(uint16), pc+(int32)hi32)

#define push(X) *--SP = ((integer)(X))
#define pop() ((integer)(*SP++))

#define local(off) &(((ptrPo)FP)[-off-1])
#define arg(off) (((ptrPo)(FP+1))+off)
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

      case OCall:        /* Call tos a1 .. an -->   */
        PROG = C_MTD(*SP++);       /* set up for callee */
        push(PC);       /* build up the frame. */
        PC = entryPoint(PROG);
        LITS = codeLiterals(PROG);
        continue;

      case Enter: {      /* set up the local env of locals */
        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */
        int32 lclCnt = collectI32(PC);  /* How many locals do we have */
        SP -= lclCnt;
        continue;
      }

      case Tail: {       /* Tail call */
        // Pick up existing frame
        framePo oldFp = FP->fp;
        insPo oldPc = FP->rtn;
        int64 argCnt = argCount(ENV); /* How many arguments in caller? */
        ENV = (closurePo) *SP;   /* Our new closure is at top of stack */

        // slide new arguments over old frame
        int64 nArgCnt = argCount(ENV);  /* prepare to slide arguments over caller */

        ptrPo tgt = arg(argCnt);
        ptrPo src = SP + nArgCnt + 1;   /* base of argument vector */

        FP = oldFp;

        for (int ix = 0; ix < nArgCnt; ix++)
          *--tgt = *--src;    /* copy the argument vector */
        SP = tgt;

        // set up new frame
        *--SP = (termPo) ENV;
        *--SP = (termPo) oldPc;  /* make sure we return where the caller returns */

        PC = entryPoint(ENV);
        LITS = codeLiterals(ENV);
        continue;       /* Were done */
      }

      case Ret: {        /* return from function */
        int64 argCnt = argCount(ENV);
        termPo ret = *SP;     /* and return value */

        SP = arg(argCnt);     /* reset stack */

        PC = FP->rtn;     /* and return address */
        FP = FP->fp;      /* and old frame pointer */

        ENV = FP->env;      /* pick up parent code */
        LITS = codeLiterals(ENV);   /* reset pointer to code literals */

        push(ret);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */
        escapePo esc = (escapePo) LITS[escNo].data;
        termPo ret = (termPo) esc->esc(SP);  /* invoke the escape */
        SP += esc->arity;     /* drop arguments */
        *--SP = ret;
        continue;
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
        termPo se = SP[offset-1];
        for(int32 ix=offset-1;ix>0;ix--)
          SP[ix] = SP[ix-1];
        *SP = se;
        continue;
      }

      case LdI:       /* load integer literal */
        push(collectI32(PC));
        continue;

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
        closurePo cl = (closurePo) pop();  /* which closure? */
        push(cl->free[ix]);
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
        integer tos = pop();
        closurePo cl = (closurePo) pop();
        cl->free[ix] = tos;
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

      case Alloc: {      /* heap allocate closure */
        methodPo cd = (methodPo) (codeLiterals(ENV)[collectI32(PC)].data);
        closurePo cl = allocate(heap, cd); /* allocate a closure on the heap */
        for (int ix = 0; ix < cd->freeCount; ix++)
          cl->free[ix] = pop();   /* fill in free variables by popping from stack */
        push(cl);       /* put the closure back on the stack */
        continue;
      }

      case I2f:       /* convert from int to float */
        *(double *) SP = (double) (*(int64 *) SP);
        continue;

      case F2i:       /* convert from float to int */
        *(int64 *) SP = (int64) (*(double *) SP);
        continue;

      case AddI: {        /* tos:=tos+tos-1, integer addition */
        int64 i = *(int64 *) SP++;
        *(int64 *) SP = i + (*(int64 *) SP);
        collectOff(PC);     /* ignore failure case for now */
        continue;
      }
      case AddF: {       /* tos:=tos+tos-1  floating point addition */
        double i = (double) pop();
        double j = (double) pop();
        collectOff(PC);     /* ignore failure case for now */
        push(i + j);
        continue;
      }

      case IncI: {        /* tos:=tos+1, integer increment */
        collectOff(PC);     /* ignore failure case for now */
        int64 i = *(int64 *) SP;
        *(int64 *) SP = i + 1;
        continue;
      }

      case SubI: {        /* tos:=tos-tos-1, integer subtract */
        collectOff(PC);     /* ignore failure case for now */
        int64 i = *(int64 *) SP++;
        *(int64 *) SP = (*(int64 *) SP) - i;
        continue;
      }

      case SubF: {       /* tos:=tos-tos-1  floating point subtract */
        double j = (double) pop();
        double i = (double) pop();
        push(i - j);
        collectOff(PC);     /* ignore failure case for now */
        continue;
      }

      case DecI: {        /* tos:=tos-1, integer decrement */
        collectOff(PC);     /* ignore failure case for now */
        int64 i = *(int64 *) SP;
        *(int64 *) SP = i - 1;
        continue;
      }

      case MulI: {        /* tos:=tos*tos-1, integer multiply */
        collectOff(PC);     /* ignore failure case for now */
        int64 i = pop();
        int64 j = pop();
        push(i * j);
        continue;
      }

      case MulF: {       /* tos:=tos*tos-1  floating point multiple */
        collectOff(PC);     /* ignore failure case for now */
        double i = (double) pop();
        double j = (double) pop();
        push(i * j);
        continue;
      }

      case DivI: {        /* tos:=tos/tos-1, integer divide */
        collectOff(PC);     /* ignore failure case for now */
        int64 j = (int64) pop();
        int64 i = (int64) pop();
        push(i / j);
        continue;
      }

      case DivF: {       /* tos:=tos/tos-1  floating point divide */
        collectOff(PC);     /* ignore failure case for now */
        double j = (double) pop();
        double i = (double) pop();
        push(i / j);
        continue;
      }

      case RemI: {        /* tos:=tos%tos-1, 32bit width */
        collectOff(PC);     /* ignore failure case for now */
        int64 j = (int64) pop();
        int64 i = (int64) pop();
        push(i % j);
        continue;
      }

      case Lft: {       /* Shift left */
        int64 j = (int64) pop();
        int64 i = (int64) pop();
        push(i << j);
        continue;
      }

      case Asr: {        /* Arithmetic shift right */
        int64 j = (int64) pop();
        int64 i = (int64) pop();
        push(i >> j);
        continue;
      }

      case Rgt: {      /* Logical shift right */
        integer j = (integer) pop();
        int64 i = (int64) pop();
        push(i >> j);
        continue;
      }

      case CmpI: {        /* compare integer, leave tos with -1,0,1 */
        int64 j = (int64) pop();
        int64 i = (int64) pop();
        push(i > j ? 1 : i < j ? -1 : 0);
        continue;
      }

      case CmpF: {       /* compare float, leave tos with -1,0,1 */
        double j = (double) pop();
        double i = (double) pop();
        push(i > j ? 1 : i < j ? -1 : 0);
        continue;
      }

      case Bz: {       /* Branch on zero */
        int64 i = pop();
        insPo exit = collectOff(PC);
        if (i == 0)
          PC = exit;
        continue;
      }

      case Bnz: {        /* Branch on non-zero */
        int64 i = pop();
        insPo exit = collectOff(PC);

        if (i != 0)
          PC = exit;
        continue;
      }

      case Blt: {        /* branch on less than zero */
        int64 i = pop();
        insPo exit = collectOff(PC);

        if (i < 0)
          PC = exit;
        continue;
      }

      case Ble: {        /* branch on less or equal to zero */
        int64 i = pop();
        insPo exit = collectOff(PC);

        if (i <= 0)
          PC = exit;
        continue;
      }

      case Bge: {        /* branch on greater or equal to zero */
        int64 i = pop();
        insPo exit = collectOff(PC);

        if (i >= 0)
          PC = exit;
        continue;
      }
      case Bgt: {        /* branch on greater than zero */
        int64 i = pop();
        insPo exit = collectOff(PC);

        if (i > 0)
          PC = exit;

        continue;
      }

      case Cas: {        /* compare and swap, branch if not zero */
        int64 nw = pop();     /* new value */
        int64 old = pop();    /* compare value */
        closurePo p = (closurePo) pop(); /* lock */
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

logical compare_and_swap(termPo cl, int64 old, int64 nw) {
  int
  integer *check = &cl->free[0];
  return __sync_bool_compare_and_swap(check, old, nw);
}

termPo localVar(framePo fp, int64 off) {
  return &(((termPo) fp)[-off - 1]);
}
