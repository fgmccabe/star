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

#define saveRegisters(P, SP) STMT_WRAP({ (P)->pc = PC; (P)->fp = FP; (P)->prog = PROG; (P)->sp = (SP);})
#define restoreRegisters(P) STMT_WRAP({ PC = (P)->pc; FP = (P)->fp; PROG=(P)->prog; SP=(P)->sp; LITS=codeLits(PROG);})

#define bail() {\
  saveRegisters(P, SP);\
  dumpStackTrace(P, logFile);\
  return Error;\
  }

#define check(Cond, Msg) { if(!(Cond)) { logMsg(logFile,(Msg)); bail(); } }

/*
 * Execute program on a given process/thread structure
 */
retCode run(processPo P) {
  heapPo H = P->heap;
  register insPo PC = P->pc;    /* Program counter */
  register framePo FP = P->fp;    /* Current locals + = arguments, - = locals */
  register methodPo PROG = P->prog; /* Current executing closure */
  register normalPo LITS = codeLits(PROG); /* pool of literals */

  register ptrPo SP = P->sp;         /* Current 'top' of stack (grows down) */

  register uint32 hi32, lo32;    /* Temporary registers */

#ifdef TRACEMEM
  if (traceMemory)
    verifyProc(P, H);
#endif

  for (;;) {
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    countIns(*PC);
    if (insDebugging) {
      saveRegisters(P, SP);
      insDebug(P, *PC);
      restoreRegisters(P);
    }
#endif

    switch ((OpCode) (*PC++)) {
      case Halt: {
        if (insDebugging || lineDebugging) {
          logMsg(logFile, "Halt %T", pop());
          bail();
        }
        return Ok;
      }
      case Call: {
        termPo nProg = nthArg(LITS, collectI32(PC));
        methodPo NPROG = labelCode(C_LBL(nProg));   // Which program do we want?

        if (SP - stackDelta(NPROG) <= (ptrPo) P->stackBase) {
          saveRegisters(P, SP);
          if (extendStack(P, 2, stackDelta(NPROG)) != Ok) {
            logMsg(logFile, "cannot extend stack");
            bail();
          }
          restoreRegisters(P);
        }
        assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);

        push(PROG);
        PROG = NPROG;
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

        continue;
      }

      case OCall: {        /* Call tos a1 .. an -->   */
        int arity = collectI32(PC);
        normalPo nProg = C_TERM(pop());
        push(nthArg(nProg, 0));                     // Put the free term back on the stack

        push(PROG);
        push(PC);       /* build up the frame. */
        labelPo oLbl = termLbl(nProg);
        PROG = labelCode(objLabel(oLbl, arity));       /* set up for object call */
        PC = entryPoint(PROG);
        LITS = codeLits(PROG);

        push(FP);
        FP = (framePo) SP;     /* set the new frame pointer */

        if (SP - stackDelta(PROG) <= (ptrPo) P->stackBase) {
          saveRegisters(P, SP);
          if (extendStack(P, 2, stackDelta(PROG)) != Ok) {
            logMsg(logFile, "cannot extend stack");
            bail();
          }
          restoreRegisters(P);
        }
        assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);

        integer lclCnt = lclCount(PROG);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;
      }

      case Escape: {     /* call escape */
        int32 escNo = collectI32(PC); /* escape number */

#ifdef TRACEEXEC
        recordEscape(escNo);
#endif

        escapePo esc = getEscape(escNo);
        saveRegisters(P, SP + esc->arity);
        assert(H->topRoot == 0);
        ReturnStatus ret = esc->fun(P, SP);  /* invoke the escape */
        assert(H->topRoot == 0);
        restoreRegisters(P);
        switch (ret.ret) {
          case Ok:
            if (ret.result != Null)
              *--SP = ret.result;
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

        if (SP - stackDelta(PROG) <= (ptrPo) P->stackBase) {
          saveRegisters(P, SP);
          if (extendStack(P, 2, 0) != Ok) {
            logMsg(logFile, "cannot extend stack");
            bail();
          }
          restoreRegisters(P);
        }
        assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);

        continue;       /* Were done */
      }

      case OTail: {       /* Tail call */
        int arity = collectI32(PC);
        normalPo nProg = C_TERM(pop());
        push(nthArg(nProg, 0));                     // Put the free term back on the stack

        // Pick up existing frame
        framePo oldFp = FP->fp;
        insPo oldRtn = FP->rtn;
        methodPo oldPROG = FP->prog;
        integer oldArgCnt = argCount(PROG);

        labelPo oLbl = termLbl(nProg);
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

        if (SP - stackDelta(PROG) <= (ptrPo) P->stackBase) {
          saveRegisters(P, SP);
          if (extendStack(P, 2, 0) != Ok) {
            logMsg(logFile, "cannot extend stack");
            bail();
          }
          restoreRegisters(P);
        }
        assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);

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

      case Rst: {
        int32 offset = collectI32(PC);
        assert(offset >= 0);
        SP = (ptrPo) FP - lclCount(PROG) - offset;
        continue;
      }

      case LdV: {
        push(voidEnum);     /* load void */
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
        globalPo glb = findGlobalVar(glbNo);

        if (glbIsSet(glb)) {
          termPo vr = getGlobal(glb);

          check(vr != Null, "undefined global");

          push(vr);     /* load a global variable */
        } else if (glbHasProvider(glb)) {
          termPo prov = getProvider(glb);  // Set up an OCall to the provider

          normalPo nProg = C_TERM(prov);

          push(PROG);
          push(PC);       /* build up the frame. */
          labelPo oLbl = termLbl(nProg);
          PROG = labelCode(objLabel(oLbl, 0));       /* set up for object call */
          PC = entryPoint(PROG);
          LITS = codeLits(PROG);

          push(FP);
          FP = (framePo) SP;     /* set the new frame pointer */

          if (SP - stackDelta(PROG) <= (ptrPo) P->stackBase) {
            saveRegisters(P, SP);
            if (extendStack(P, 2, 0) != Ok) {
              logMsg(logFile, "cannot extend stack");
              bail();
            }
            restoreRegisters(P);
          }
          assert(SP - stackDelta(PROG) > (ptrPo) P->stackBase);

          integer lclCnt = lclCount(PROG);  /* How many locals do we have */
          SP -= lclCnt;
#ifdef TRACEEXEC
          for (integer ix = 0; ix < lclCnt; ix++)
            SP[ix] = voidEnum;
#endif
        } else {
          logMsg(logFile, "global %s not defined", globalVarName(glb));
          bail();
        }
        continue;
      }

      case Get: {
        labelPo lbl = C_LBL(nthArg(LITS, collectI32(PC)));
        normalPo trm = C_TERM(pop());
        termPo el = getField(trm, lbl);
        if (el != Null)
          push(el);
        else {
          logMsg(logFile, "no field %T in %T", lbl, trm);
          bail();
        }
        continue;
      }

      case CLbl: {
        termPo l = pop();
        termPo t = pop();
        insPo exit = collectOff(PC);
        assert(validPC(PROG, exit));

        if (isNormalPo(t)) {
          normalPo cl = C_TERM(t);
          if (isALabel(l) && sameLabel(C_LBL(l), termLbl(cl)))
            PC = exit;
        } else if (sameTerm(t, l))
          PC = exit;
        continue;
      }

      case Nth: {
        int32 ix = collectI32(PC);  /* which element */
        normalPo cl = C_TERM(pop());  /* which term? */
        push(nthArg(cl, ix));
        continue;
      }

      case StL: {
        int32 offset = collectI32(PC);
        ptrPo dest = local(offset);
        *dest = pop();
        continue;
      }

      case StV: {
        int32 offset = collectI32(PC);
        ptrPo dest = local(offset);
        *dest = voidEnum;
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

      case Set: {
        labelPo lbl = C_LBL(nthArg(LITS, collectI32(PC)));
        termPo val = pop();
        normalPo trm = C_TERM(pop());
        setField(trm, lbl, val);
        continue;
      }

      case IAdd: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, integerVal(Lhs) + integerVal(Rhs));
        push(Rs);
        continue;
      }

      case ISub: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, integerVal(Lhs) - integerVal(Rhs));
        push(Rs);
        continue;
      }
      case IMul: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, integerVal(Lhs) * integerVal(Rhs));
        push(Rs);
        continue;
      }
      case IDiv: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, integerVal(Lhs) / integerVal(Rhs));
        push(Rs);
        continue;
      }
      case IMod: {
        integer denom = integerVal(pop());
        integer numerator = integerVal(pop());

        integer reslt = denom % numerator;

        termPo Rs = (termPo) allocateInteger(H, reslt);

        push(Rs);
        continue;
      }
      case IAbs: {
        termPo Trm = pop();
        integer Arg = integerVal(Trm);

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
        assert(validPC(PROG, exit));

        if (integerVal(i) != integerVal(j))
          PC = exit;
        continue;
      }
      case BAnd: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, ((unsigned) integerVal(Lhs) & (unsigned) integerVal(Rhs)));
        push(Rs);
        continue;
      }
      case BOr: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, ((unsigned) integerVal(Lhs) | (unsigned) integerVal(Rhs)));
        push(Rs);
        continue;
      }
      case BXor: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, ((unsigned) integerVal(Lhs) ^ (unsigned) integerVal(Rhs)));
        push(Rs);
        continue;
      }
      case BNot: {
        termPo Lhs = pop();

        termPo Rs = (termPo) allocateInteger(H, ~(unsigned) integerVal(Lhs));
        push(Rs);
        continue;
      }
      case BLsl: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, ((unsigned) integerVal(Lhs) << (unsigned) integerVal(Rhs)));
        push(Rs);
        continue;
      }
      case BLsr: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, (((unsigned) integerVal(Lhs)) >> ((unsigned) integerVal(Rhs))));
        push(Rs);
        continue;
      }
      case BAsr: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateInteger(H, (integerVal(Lhs) >> integerVal(Rhs)));
        push(Rs);
        continue;
      }
      case FAdd: {
        termPo Rhs = pop();
        termPo Lhs = pop();

        termPo Rs = (termPo) allocateFloat(H, floatVal(Lhs) + floatVal(Rhs));
        push(Rs);
        continue;
      }
      case FSub: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateFloat(H, floatVal(Lhs) - floatVal(Rhs));
        push(Rs);
        continue;
      }
      case FMul: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateFloat(H, floatVal(Lhs) * floatVal(Rhs));
        push(Rs);
        continue;
      }
      case FDiv: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (termPo) allocateFloat(H, floatVal(Lhs) / floatVal(Rhs));
        push(Rs);
        continue;
      }
      case FMod: {
        termPo Lhs = pop();
        termPo Rhs = pop();
        termPo Rs = (termPo) allocateFloat(H, fmod(floatVal(Lhs), floatVal(Rhs)));
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
        assert(validPC(PROG, exit));

        if (floatVal(x) != floatVal(y))
          PC = exit;
        continue;
      }

      case Case: {      /* case instruction */
        int32 mx = collectI32(PC);
        termPo tos = top();
        integer hx = hashTermLbl(tos) % mx;

        PC = (insPo) ((void *) PC + (sizeof(insWord) * 3) * hx);
        continue;
      }

      case Alloc: {      /* heap allocate term */
        labelPo cd = C_LBL(nthArg(LITS, collectI32(PC)));
        if (enoughRoom(H, cd) != Ok) {
          saveRegisters(P, SP);
          retCode ret = gcCollect(H, NormalCellCount(cd->arity));
          if (ret != Ok)
            return ret;
          restoreRegisters(P);
        }
        normalPo cl = allocateStruct(H, cd); /* allocate a closure on the heap */
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

      case Frame: {
        termPo frame = nthArg(LITS, collectI32(PC));
        // ignore frame entity for now
        continue;
      }
      case Throw: {
        termPo item = pop();
        int32 off = collectI32(PC);

        while (off == 0) {
          int64 argCnt = argCount(PROG);

          SP = (ptrPo) FP;     /* reset stack */

          FP = (framePo) (*SP++);
          PC = (insPo) (*SP++);
          PROG = (methodPo) (*SP++);

          LITS = codeLits(PROG);   /* reset pointer to code literals */

          SP += argCnt;
          OpCode nxt = (OpCode) (*PC++);
          assert(nxt == Unwind);
          off = collectI32(PC);     // look at the offset in the next instruction
        }
        PC += off;
        push(item);
        continue;
      }

      case Unwind:
        PC += 2;
        continue;

      case dBug: {
#ifdef TRACEEXEC
        if (lineDebugging) {
          saveRegisters(P, SP);
          enterDebug(P);
          restoreRegisters(P);
        }
#endif
        continue;
      }

      case dLine: {
        termPo line = nthArg(LITS, collectI32(PC));

#ifdef TRACEEXEC
        if (lineDebugging) {
          saveRegisters(P, SP);
          lineDebug(P, line);
          restoreRegisters(P);
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
