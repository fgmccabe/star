/*
 * Run-time evaluation for CAFE programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, a JIT
 * process will generate native instructions from CAFE instructions.
 */

#include "config.h"

#include <stdlib.h>
#include <assert.h>

#include "options.h"      /* run-time options */
#include "engine.h"     /* access engine definitions */
#include "escapes.h"      /* escape call handling */
#include "codeP.h"      /* We need access to code structures */
#include "opcodes.h"

#ifdef TRACEEXEC
extern logical tracing;     /* true if debugging instructions */
#endif

#define collectI32(pc) (hi32 = (uint32)(*pc++), lo32 = *pc++, ((hi32<<16)|lo32))

#define collectOff(pc) (hi32 = collectI32(pc)/sizeof(uint16), pc+(int32)hi32)

#define push(X) *--SP = ((uint64)(X))
#define pop() (*SP++)

#define local(off) &(((ptrPo)FP)[-off-1])
#define arg(off) (((ptrPo)(FP+1))+off)
#define free(off) (ENV->free[off])

/*
 * Execute program on a given process/thread structure
 */
uint64 run(processPo P,heapPo heap)
{
  register insPo PC = P->pc;    /* Program counter */
  register framePo FP = P->fp;    /* Current locals + = arguments, - = locals */
  register closurePo ENV = P->prog; /* Current executing closure */
  register constantPo LITS = codeLiterals(ENV); /* pool of literals */

  register ptrPo SP = P->sp;         /* Current 'top' of stack (grows down) */

  register uint32 hi32,lo32;    /* Temporary registers */

#ifdef TRACEEXEC
  long pcCount = 0;       /* How many instructions executed so far? */
#endif

  for(;;){
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    if(tracing)
      debug_stop(pcCount,P,ENV,PC,FP,SP);
#endif

    switch(*PC++){
    case Halt:
      return *SP;

    case Call:        /* Call tos a1 .. an -->   */
      ENV = (closurePo)*SP;   /* set up env for callee */
      push(PC);       /* build up the frame. */
      PC = entryPoint(ENV);
      LITS = codeLiterals(ENV);
      continue;

    case Enter:{      /* set up the local env of locals */
      push(FP);
      FP = (framePo)SP;     /* set the new frame pointer */
      int32 lclCnt = collectI32(PC);  /* How many locals do we have */
      SP -= lclCnt;
      continue;
    }

    case Tail:{       /* Tail call */
      // Pick up existing frame
      framePo oldFp = FP->fp;
      insPo oldPc = FP->rtn;
      int32 argCnt = argCount(ENV); /* How many arguments in caller? */
      ENV = (closurePo)*SP;   /* Our new closure is at top of stack */

      // slide new arguments over old frame
      int32 nArgCnt = argCount(ENV);  /* prepare to slide arguments over caller */

      ptrPo tgt = arg(argCnt);
      ptrPo src = SP+nArgCnt+1;   /* base of argument vector */

      FP = oldFp;

      for(int ix=0;ix<nArgCnt;ix++)
        *--tgt = *--src;    /* copy the argument vector */
      SP = tgt;

      // set up new frame
      *--SP = (uint64)ENV;
      *--SP = (uint64)oldPc;  /* make sure we return where the caller returns */
      
      PC = entryPoint(ENV);
      LITS = codeLiterals(ENV);
      continue;       /* Were done */
    }

    case Ret:{        /* return from function */
      int32 argCnt = argCount(ENV);
      uint64 ret = *SP;     /* and return value */

      SP = arg(argCnt);     /* reset stack */

      PC = FP->rtn;     /* and return address */
      FP = FP->fp;      /* and old frame pointer */

      ENV = FP->env;      /* pick up parent code */
      LITS = codeLiterals(ENV);   /* reset pointer to code literals */

      push(ret);      /* push return value */
      continue;       /* and carry on regardless */
    }

    case Escape:{     /* call escape */
      int32 escNo = collectI32(PC); /* escape number */
      escapePo esc = (escapePo)LITS[escNo].data;
      uint64 ret = esc->esc(SP);  /* invoke the escape */
      SP += esc->arity;     /* drop arguments */
      *--SP = ret;
      continue;
    }

    case Jmp:       /* jump to local offset */
      PC = collectOff(PC);
      continue;

    case Pop:
      SP++;       /* drop tos */
      continue;

    case Dup:{        /* duplicate tos */
      int64 tos = *SP;
      *--SP = tos;
      continue;
    }

    case Pull:{
      int32 offset = collectI32(PC);    // How far down to reach stack
      int64 se = SP[offset];
      *--SP = se;
      continue;
    }

    case Swap:{
      int64 xx = *SP;     /* swap top two elements of stack */
      *SP = *(SP-1);
      *(SP-1) = xx;
      continue;
    }

    case LdInt:       /* load integer literal */
      push(collectI32(PC));
      continue;

    case LdConst:     /* load literal value from pool */
      push(LITS[collectI32(PC)].data);
      continue;

    case LdArg:{
      int32 offset = collectI32(PC);
      ptrPo src = arg(offset);
      push(*src);     /* load argument */
      continue;
    }

    case LdLocal:{
      int32 offset = collectI32(PC);
      ptrPo src = local(offset);
      push(*src);     /* load local */
      continue;
    }

    case LdEnv:
      push(free(collectI32(PC))); /* load from free variables */
      continue;

    case Nth:{
      int32 ix = collectI32(PC);  /* which element */
      closurePo cl = (closurePo)pop();  /* which closure? */
      push(cl->free[ix]);
      continue;
    }

    case StLocal:{
      int32 offset = collectI32(PC);
      ptrPo dest = local(offset);
      *dest = pop();
      continue;
    }

    case StArg:{
      int32 offset = collectI32(PC);
      ptrPo dest = arg(offset);
      *dest = pop();     /* store as argument */
      continue;
    }

    case StEnv:          /* store into free. Should not normally happen */
      free(collectI32(PC)) = pop();
      continue;

    case StNth:{      /* store into a closure */
      int32 ix = collectI32(PC);
      uint64 tos = pop();
      closurePo cl = (closurePo)pop();
      cl->free[ix] = tos;
      continue;
    }

    case Cayse:{      /* case instruction */
      int32 mx = collectI32(PC);
      int32 ix = (int32)pop();    /* tos has an index */

      if(ix>0 && ix<mx)     /* size of a jump instruction */
        PC = (insPo) ((void*)PC+ ( sizeof(insPo*) + sizeof(int32)));
      /* otherwise default to next instruction */

      continue;
    }

    case Alloc:{      /* heap allocate closure */
      methodPo cd = (methodPo)(codeLiterals(ENV)[collectI32(PC)].data);
      closurePo cl = allocate(heap,cd); /* allocate a closure on the heap */
      for(int ix=0;ix<cd->freeCount;ix++)
        cl->free[ix] = pop();   /* fill in free variables by popping from stack */
      push(cl);       /* put the closure back on the stack */
      continue;
    }      

    case I2f:       /* convert from int to float */
      *(double*)SP = (double)(*(int64*)SP);
      continue;

    case F2i:       /* convert from float to int */
      *(int64*)SP = (int64)(*(double*)SP);
      continue;

    case Add:{        /* tos:=tos+tos-1, integer addition */
      int64 i = *(int64*)SP++;
      *(int64*)SP = i+(*(int64*)SP);
      collectOff(PC);     /* ignore failure case for now */
      continue;
    }
    case Addf:{       /* tos:=tos+tos-1  floating point addition */
      double i = (double)pop();
      double j = (double)pop();
      collectOff(PC);     /* ignore failure case for now */
      push(i+j);
      continue;
    }

    case Inc:{        /* tos:=tos+1, integer increment */
      collectOff(PC);     /* ignore failure case for now */
      int64 i = *(int64*)SP;
      *(int64*)SP = i+1;
      continue;
    }

    case Sub:{        /* tos:=tos-tos-1, integer subtract */
      collectOff(PC);     /* ignore failure case for now */
      int64 i = *(int64*)SP++;
      *(int64*)SP = (*(int64*)SP)-i;
      continue;
    }

    case Subf:{       /* tos:=tos-tos-1  floating point subtract */
      double j = (double)pop();
      double i = (double)pop();
      push(i-j);
      collectOff(PC);     /* ignore failure case for now */
      continue;
    }

    case Dec:{        /* tos:=tos-1, integer decrement */
      collectOff(PC);     /* ignore failure case for now */
      int64 i = *(int64*)SP;
      *(int64*)SP = i-1;
      continue;
    }

    case Mul:{        /* tos:=tos*tos-1, integer multiply */
      collectOff(PC);     /* ignore failure case for now */
      int64 i = pop();
      int64 j = pop();
      push(i*j);
      continue;
    }

    case Mulf:{       /* tos:=tos*tos-1  floating point multiple */
      collectOff(PC);     /* ignore failure case for now */
      double i = (double)pop();
      double j = (double)pop();
      push(i*j);
      continue;
    }

    case Div:{        /* tos:=tos/tos-1, integer divide */
      collectOff(PC);     /* ignore failure case for now */
      int64 j = (int64)pop();
      int64 i = (int64)pop();
      push(i/j);
      continue;
    }

    case Divf:{       /* tos:=tos/tos-1  floating point divide */
      collectOff(PC);     /* ignore failure case for now */
      double j = (double)pop();
      double i = (double)pop();
      push(i/j);
      continue;
    }

    case Rem:{        /* tos:=tos%tos-1, 32bit width */
      collectOff(PC);     /* ignore failure case for now */
      int64 j = (int64)pop();
      int64 i = (int64)pop();
      push(i%j);
      continue;
    }

    case Left:{       /* Shift left */
      int64 j = (int64)pop();
      int64 i = (int64)pop();
      push(i<<j);
      continue;
    }

    case Asr:{        /* Arithmetic shift right */
      int64 j = (int64)pop();
      int64 i = (int64)pop();
      push(i>>j);
      continue;
    }

    case Right:{      /* Logical shift right */
      uint64 j = (uint64)pop();
      int64 i = (int64)pop();
      push(i>>j);
      continue;
    }

    case Cmp:{        /* compare integer, leave tos with -1,0,1 */
      int64 j = (int64)pop();
      int64 i = (int64)pop();
      push(i>j?1:i<j?-1:0);
      continue;
    }

    case Cmpf:{       /* compare float, leave tos with -1,0,1 */
      double j = (double)pop();
      double i = (double)pop();
      push(i>j?1:i<j?-1:0);
      continue;
    }

    case Bz:{       /* Branch on zero */
      int64 i = pop();
      insPo exit = collectOff(PC);
      if(i==0)
        PC = exit;
      continue;
    }

    case Bnz:{        /* Branch on non-zero */
      int64 i = pop();
      insPo exit = collectOff(PC);

      if(i!=0)
        PC = exit;
      continue;
    }

    case Blt:{        /* branch on less than zero */
      int64 i = pop();
      insPo exit = collectOff(PC);

      if(i<0)
        PC = exit;
      continue;
    }

    case Ble:{        /* branch on less or equal to zero */
      int64 i = pop();
      insPo exit = collectOff(PC);

      if(i<=0)
        PC = exit;
      continue;
    }

    case Bge:{        /* branch on greater or equal to zero */
      int64 i = pop();
      insPo exit = collectOff(PC);

      if(i>=0)
        PC = exit;
      continue;
    }
    case Bgt:{        /* branch on greater than zero */
      int64 i = pop();
      insPo exit = collectOff(PC);

      if(i>0)
        PC = exit;

      continue;
    }

    case Cas:{        /* compare and swap, branch if not zero */
      int64 nw = pop();     /* new value */
      int64 old = pop();    /* compare value */
      closurePo p = (closurePo)pop(); /* lock */
      insPo exit = collectOff(PC);

      if(!compare_and_swap(p, old, nw)) 
        PC = exit;
      continue;
    }

    default:
    case illegalOp:
      syserr("Illegal instruction");
    }
  }
}

logical compare_and_swap(closurePo cl,int64 old,int64 nw)
{
  uint64* check = &cl->free[0];
  return __sync_bool_compare_and_swap(check,old,nw);
}

ptrPo localVar(framePo fp,int off)
{
  return &(((ptrPo)fp)[-off-1]);
}
