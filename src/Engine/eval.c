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
#include "cellP.h"

#define collectI32(pc) (hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<(unsigned)16)|lo32))
#define collectOff(pc) (hi32 = collectI32(pc), (pc)+(signed)hi32)

#define checkAlloc(Count) do{\
  if (reserveSpace(H, Count) != Ok) {\
    saveRegisters();\
    retCode ret = gcCollect(H, Count);\
    if (ret != Ok)\
      return ret;\
    restoreRegisters();   \
    check(reserveSpace(H,Count)==Ok,"could not reserve space");\
  }\
}while(False)

#define stackPtr(offset) ((ptrPo)(&S->stack[offset]))

#define pop() (*SP++)
#define top() (*SP)
#define push(T) STMT_WRAP({*--SP=(termPo)(T);})
#define local(off) stackPtr(F->fp-off)
#define arg(off) stackPtr(F->fp+off)
#define stackRoom(amnt) ((SP-(amnt)) > (ptrPo)(F+1))
#define saveRegisters() STMT_WRAP({ F->pc = PC; S->sp = stackOffset(S,SP); })
#define restoreRegisters() STMT_WRAP({ F = currFrame(S); PC = F->pc;  SP=stackPtr(S->sp); LITS=codeLits(F->prog);})

#define bail() STMT_WRAP({\
  saveRegisters();\
  stackTrace(P, logFile, S);\
  return Error;\
  })

#define stackGrow(Amnt, SaveArity) STMT_WRAP({\
  saveRegisters();\
  stackPo prevStack = S;\
  S = P->stk = glueOnStack(H, S, (S->sze * 3) / 2 + (Amnt));\
  for (integer ix = SaveArity; ix > 0; ix--) {\
    pushStack(S,popStack(prevStack));\
  }\
  restoreRegisters();\
  if (!stackRoom(Amnt)) {\
    logMsg(logFile, "cannot extend stack sufficiently");\
    bail();\
  }\
  })

#define oCall(obj, arity) STMT_WRAP({ \
  labelPo oLbl = objLabel(termLbl(obj), (arity));\
  if (oLbl == Null) {\
    logMsg(logFile, "label %s/%d not defined", labelName(termLbl(obj)), arity);\
    bail();\
  }\
  methodPo mtd = labelCode(oLbl);       /* set up for object call */\
  if (mtd == Null) {\
    logMsg(logFile, "no definition for %T", oLbl);\
    bail();\
  }\
  push(nthElem(obj, 0));                     // Put the free term back on the stack\
  if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE))\
    stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));\
  F->pc = PC;\
  F = pushFrame(S, mtd, stackOffset(S, SP));\
  PC = entryPoint(mtd);\
  LITS = codeLits(mtd);\
  integer lclCnt = lclCount(mtd);  /* How many locals do we have */\
  SP -= lclCnt;\
  for (integer ix = 0; ix < lclCnt; ix++)\
    SP[ix] = voidEnum;\
})

/*
 * Execute program on a given process/thread structure
 */
retCode run(processPo P) {
  heapPo H = P->heap;
  stackPo S = P->stk;
  framePo F = currFrame(S);
  register insPo PC = F->pc;    /* Program counter */
  register normalPo LITS = codeLits(F->prog); /* pool of literals */
  register ptrPo SP = stackPtr(S->sp);         /* Current 'top' of stack (grows down) */

  register uint32 hi32, lo32;    /* Temporary registers */

#ifdef TRACEMEM
  if (traceMemory)
    verifyProc(P, H);
#endif

  for (;;) {
#ifdef TRACEEXEC
    pcCount++;        /* increment total number of executed */

    if (insDebugging) {
      saveRegisters();
      insDebug(P, *PC);
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
      case Abort: {
        termPo lc = pop();
        termPo msg = pop();

        logMsg(logFile, "Abort %T at %L", msg, lc);
        verifyProc(P, processHeap(P));
        stackTrace(P, logFile, P->stk);

        return Error;
      }
      case Call: {
        termPo nProg = nthElem(LITS, collectI32(PC));
        methodPo mtd = labelCode(C_LBL(nProg));   // Which program do we want?

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE)) {
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));

          assert(stackRoom(stackDelta(mtd) + STACKFRAME_SIZE));
        }
        F->pc = PC;
        F = pushFrame(S, mtd, stackOffset(S, SP));
        PC = entryPoint(mtd);
        LITS = codeLits(mtd);

        integer lclCnt = lclCount(mtd);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

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

        if (!stackRoom(stackDelta(mtd) + STACKFRAME_SIZE))
          stackGrow(stackDelta(mtd) + STACKFRAME_SIZE, codeArity(mtd));

        F->pc = PC;
        F = pushFrame(S, mtd, stackOffset(S, SP));
        PC = entryPoint(mtd);
        LITS = codeLits(mtd);

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

#ifdef TRACEEXEC
        recordEscape(escNo);
#endif

        escapePo esc = getEscape(escNo);
        saveRegisters();
        assert(H->topRoot == 0);
        ReturnStatus ret = esc->fun(P, SP);  /* invoke the escape */
        assert(H->topRoot == 0);
        restoreRegisters();
        SP += esc->arity;

        switch (ret.ret) {
          case Ok:
            if (ret.result != Null)
              push(ret.result);
            continue;
          case Error:
            return Error;
          default:
            continue;
        }
      }

      case TCall: {       /* Tail call of explicit program */
        termPo nProg = nthElem(LITS, collectI32(PC));
        labelPo lbl = C_LBL(nProg);
        integer arity = labelArity(lbl);

        // Pick up existing frame
        ptrPo tgt = stackPtr(F->fp + argCount(F->prog));
        ptrPo src = SP + arity;                  /* base of argument vector */

        for (int ix = 0; ix < arity; ix++)
          *--tgt = *--src;    /* copy the argument vector */

        methodPo mtd = F->prog = labelCode(lbl);
        F->pc = PC = entryPoint(mtd);
        SP = tgt;
        F->fp = stackOffset(S, SP);

        LITS = codeLits(mtd);
        integer lclCnt = lclCount(mtd);  /* How many locals do we have */

        if (!stackRoom(lclCnt))
          stackGrow(lclCnt, arity);

        SP -= lclCnt;

#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;       /* Were done */
      }

      case TOCall: {       /* Tail call */
        int arity = collectI32(PC);
        normalPo obj = C_NORMAL(pop());
        labelPo oLbl = objLabel(termLbl(obj), arity);

        push(nthElem(obj, 0));                     // Put the free term back on the stack

        // Pick up existing frame
        ptrPo tgt = stackPtr(F->fp + argCount(F->prog));
        ptrPo src = SP + arity;                  /* base of argument vector */

        for (int ix = 0; ix < arity; ix++)
          *--tgt = *--src;    /* copy the argument vector */

        methodPo mtd = labelCode(oLbl);       /* set up for object call */
        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", oLbl);
          bail();
        }

        F->prog = mtd;
        F->pc = PC = entryPoint(mtd);
        SP = tgt;
        F->fp = stackOffset(S, SP);

        LITS = codeLits(mtd);
        integer lclCnt = lclCount(mtd);  /* How many locals do we have */

        if (!stackRoom(lclCnt))
          stackGrow(lclCnt, arity);

        SP -= lclCnt;

#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;       /* Were done */
      }

      case Ret: {        /* return from function */
        termPo retVal = *SP;     /* return value */

        assert(F->fp > 0);
        assert(stackOffset(S, SP) < F->fp); // We must have a value to return

        SP = stackPtr(F->fp + argCount(F->prog));
        S->fp--;
        F = currFrame(S);
        PC = F->pc;
        LITS = codeLits(F->prog);   /* reset pointer to code literals */

        push(retVal);      /* push return value */
        continue;       /* and carry on regardless */
      }

      case Jmp:       /* jump to local offset */
        PC = collectOff(PC);
        assert(validPC(F->prog, PC));
        continue;

      case Drop:
        SP++;       /* drop tos */
        continue;

      case DropTo: {
        termPo top = pop();
        int32 height = collectI32(PC);
        assert(height >= 0);
        SP = stackPtr(F->fp - lclCount(F->prog) - height);
        push(top);
        continue;
      }

      case Dup: {        /* duplicate tos */
        termPo tos = *SP;
        *--SP = tos;
        continue;
      }

      case Rst: {
        int32 offset = collectI32(PC);
        assert(offset >= 0);
        SP = stackPtr(F->fp - lclCount(F->prog) - offset);
        continue;
      }

      case Swap: {
        termPo t1 = pop();
        termPo t2 = pop();
        push(t1);
        push(t2);
        continue;
      }

      case Tag: {
        labelPo lbl = C_LBL(nthElem(LITS, collectI32(PC)));
        static integer tagCount = 0;

        labelPo nLbl = otherLbl(lbl, tagCount++);
        push(nLbl);
        continue;
      }

      case Prompt: {
        normalPo thunk = C_NORMAL(pop());
        termPo prompt = pop();
        saveRegisters();

        S = P->stk = spinupStack(H, S, initStackSize, prompt);

        restoreRegisters();
        push(nthElem(thunk, 0));                     // Put the free term back on the stack

        labelPo oLbl = termLbl(thunk);
        methodPo thMtd = labelCode(oLbl);       /* set up for object call */

        if (thMtd == Null) {
          logMsg(logFile, "no definition for %T", oLbl);
          bail();
        }

        F = pushFrame(S, thMtd, stackOffset(S, SP));
        PC = entryPoint(thMtd);
        LITS = codeLits(thMtd);

        integer lclCnt = lclCount(thMtd);  /* How many locals do we have */
        SP -= lclCnt;
#ifdef TRACEEXEC
        for (integer ix = 0; ix < lclCnt; ix++)
          SP[ix] = voidEnum;
#endif

        continue;
      }

      case Cut: {
        termPo handler = pop();
        termPo promptLbl = pop();
        saveRegisters();
        stackPo prompt = detachStack(S, promptLbl);

        if (prompt == Null) {
          logMsg(logFile, "cannot find prompt associated with %T", promptLbl);
          bail();
        } else {
          stackPo suspended = S;

          S = P->stk = prompt; // Reset the stack to prompt point
          restoreRegisters();

          push(suspended);

          labelPo oLbl = objLabel(termLbl(handler), 2);  // Enter the cut handler

          methodPo thMtd = labelCode(oLbl);       /* set up for object call */

          if (thMtd == Null) {
            logMsg(logFile, "no definition for %T", oLbl);
            bail();
          }

          push(nthElem(handler, 0));                     // Put the free term back on the stack
          F->pc = PC;
          F = pushFrame(S, thMtd, stackOffset(S, SP));
          PC = entryPoint(thMtd);
          LITS = codeLits(thMtd);

          integer lclCnt = lclCount(thMtd);  /* How many locals do we have */
          SP -= lclCnt;
#ifdef TRACEEXEC
          for (integer ix = 0; ix < lclCnt; ix++)
            SP[ix] = voidEnum;
#endif

        }
        continue;
      }

      case Resume: {
        termPo cont = pop();
        termPo k = pop();
        stackPo stk = C_STACK(cont);
        saveRegisters();
        S = P->stk = attachStack(S, stk);
        restoreRegisters();
        push(k);
        continue;
      }

      case TResume: {                         // Tail resumptive entry to continuation
        termPo cont = pop();
        termPo k = pop();
        stackPo stk = C_STACK(cont);

        assert(F->fp > 0);

        SP = stackPtr(F->fp + argCount(F->prog));
        S->fp--;
        F = currFrame(S);
        PC = F->pc;

        saveRegisters();
        S = P->stk = attachStack(S, stk);
        restoreRegisters();
        push(k);
        continue;
      }

      case Underflow: {
        termPo val = pop();
        saveRegisters();  // Seal off the current stack
        assert(stackState(S) == attached);
        setStackState(S, detached);
        S = P->stk = S->attachment;
        restoreRegisters();
        push(val);
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
        } else {
          methodPo glbThnk = labelCode(findLbl(globalVarName(glb), 0));       /* set up for object call */

          if (glbThnk == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }

          if (!stackRoom(stackDelta(glbThnk) + STACKFRAME_SIZE)) {
            stackGrow(stackDelta(glbThnk) + STACKFRAME_SIZE, codeArity(glbThnk));

            assert(stackRoom(stackDelta(glbThnk) + STACKFRAME_SIZE));
          }
          F->pc = PC;
          F = pushFrame(S, glbThnk, stackOffset(S, SP));
          PC = entryPoint(glbThnk);
          LITS = codeLits(glbThnk);

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
        assert(validPC(F->prog, exit));

        if (isNormalPo(t)) {
          normalPo cl = C_NORMAL(t);
          if (sameLabel(l, termLbl(cl)))
            PC = exit;
        }
        continue;
      }

      case CmpVd: {
        termPo t = pop();
        insPo exit = collectOff(PC);
        assert(validPC(F->prog, exit));

        if (t == voidEnum) {
          PC = exit;
        }
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
        push(unitEnum);
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
        assert(validPC(F->prog, exit));

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
        double Rhs = floatVal(pop());
        double Lhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs + Rhs);
        push(Rs);
        continue;
      }
      case FSub: {
        double Rhs = floatVal(pop());
        double Lhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs - Rhs);
        push(Rs);
        continue;
      }
      case FMul: {
        double Rhs = floatVal(pop());
        double Lhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs * Rhs);
        push(Rs);
        continue;
      }
      case FDiv: {
        double Rhs = floatVal(pop());
        double Lhs = floatVal(pop());

        checkAlloc(FloatCellCount);
        termPo Rs = (termPo) allocateFloat(H, Lhs / Rhs);
        push(Rs);
        continue;
      }
      case FMod: {
        double Rhs = floatVal(pop());
        double Lhs = floatVal(pop());

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
        assert(validPC(F->prog, exit));

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

        assert(validPC(F->prog, exit));

        if (sameLabel(l, termLbl(t))) {
          integer arity = labelArity(l);
          for (integer ix = arity - 1; ix >= 0; ix--)
            push(nthElem(t, ix));
        } else {
          push(t);
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

      case AlTpl: {      /* Allocate new tuple */
        labelPo cd = C_LBL(nthElem(LITS, collectI32(PC)));
        if (enoughRoom(H, cd) != Ok) {
          saveRegisters();
          retCode ret = gcCollect(H, NormalCellCount(cd->arity));
          if (ret != Ok)
            return ret;
          restoreRegisters();
        }
        normalPo cl = allocateStruct(H, cd); /* allocate a closure on the heap */
        for (int ix = 0; ix < cd->arity; ix++)
          cl->args[ix] = voidEnum;   /* fill in free variables with voids */
        push(cl);       /* put the structure back on the stack */
        continue;
      }

      case Cmp: {
        termPo i = pop();
        termPo j = pop();
        insPo exit = collectOff(PC);
        assert(validPC(F->prog, exit));

        if (!sameTerm(i, j))
          PC = exit;
        continue;
      }

      case Comp: {
        termPo i = pop();
        termPo j = pop();
        insPo exit = collectOff(PC);
        assert(validPC(F->prog, exit));

        if (!sameTerm(i, j))
          PC = exit;
        continue;
      }

      case If: {
        termPo i = pop();
        insPo exit = collectOff(PC);
        assert(validPC(F->prog, exit));

        if (sameTerm(i, trueEnum))
          PC = exit;
        continue;
      }

      case IfNot: {
        termPo i = pop();
        insPo exit = collectOff(PC);
        assert(validPC(F->prog, exit));

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

      case dLine: {
        termPo line = nthElem(LITS, collectI32(PC));

#ifdef TRACEEXEC
        if (lineDebugging) {
          saveRegisters();
          lineDebug(P, line);
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
