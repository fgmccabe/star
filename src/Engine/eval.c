/*
 * Run-time evaluation for STAR programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, a JIT
 * process will generate native instructions from STAR instructions.
 */

#include "config.h"
#include <globals.h>
#include "constants.h"
#include "char.h"
#include <math.h>
#include "errorCodes.h"
#include "escapeP.h"
#include "evalP.h"
#include "threds.h"

logical collectStats = False;

/*
 * Execute program on a given process/thread structure
 */
retCode run(processPo P) {
  heapPo H = P->heap;
  stackPo STK = P->stk;
  framePo FP = STK->fp;
  register insPo PC = STK->pc; /* Program counter */
  register ptrPo SP = STK->sp; /* Current 'top' of stack (grows down) */
  register methodPo PROG = STK->prog;
  register ptrPo ARGS = STK->args;

  for (;;) {
#ifndef NDEBUG
    pcCount++; /* increment total number of executed */

    if (insDebugging) {
      saveRegisters();
      insDebug(P);
      restoreRegisters();
    }
#endif

    switch (PC->op) {
      case Halt: {
        int32 exitCode = PC->fst;

        return (retCode) exitCode;
      }
      case Nop: {
        PC++;
        continue;
      }
      case Abort: {
        termPo lc = pop();
        termPo msg = pop();
        saveRegisters();
        abort_star(P, lc, msg);

        return Error;
      }

      case Call:
      case XCall: {
        labelPo nProg = C_LBL(getConstant(PC->fst));
        methodPo mtd = labelCode(nProg); // Which program do we want?

        if (mtd == Null) {
          logMsg(logFile, "label %A not defined", nProg);
          bail();
        }

        FP++;
        FP->prog = PROG;
        FP->link = PC + 1;
        FP->args = ARGS;

        PROG = mtd;
        ARGS = SP;

        if (hasJit(mtd)) {
#ifdef TRACEJIT
          if (traceJit) {
            logMsg(logFile, "entering jitted code %T", mtd);
          }
#endif

          saveRegisters();
          invokeJitMethod(P, mtd);
          restoreRegisters();
          PC++;
        } else {
          PC = entryPoint(mtd);
        }
        continue;
      }

      case OCall:
      case XOCall: {
        /* Call tos a1 .. an -->   */
        int32 arity = PC->fst;
        termPo cl = pop();
        if (!isClosure(cl)) {
          logMsg(logFile, "Calling non-closure %T", cl);
          bail();
        }
        closurePo obj = C_CLOSURE(cl);
        labelPo lb = closureLabel(obj);

        if (lblArity(lb) != arity) {
          logMsg(logFile, "closure %T does not have correct arity %d", obj, arity);
          bail();
        }

        methodPo mtd = labelCode(lb); /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lb);
          bail();
        }

        push(closureFree(obj)); // Put the free term back on the stack

        FP++; // Guaranteed to have room for the frame
        FP->prog = PROG;
        FP->link = PC + 1;
        FP->args = ARGS;

        PROG = mtd;
        ARGS = SP;
        PC = entryPoint(mtd);
        continue;
      }

      case Escape:
      case XEscape: {
        /* call escape */
        int32 escNo = PC->fst; /* escape number */

#ifndef NDEBUG
        if (collectStats)
          recordEscape(escNo);
#endif

        escapePo esc = getEscape(escNo);
        saveRegisters();
        assert(H->topRoot == 0);
        ReturnStatus ret = (esc->fun)(P);
        restoreRegisters();
        assert(H->topRoot == 0);

        if (ret == Normal) {
          PC++;
          continue;
        } else if (PC->op == XEscape) {
          termPo exception = pop();
          breakOut();
          push(exception);
          continue;
        } else {
          logMsg(logFile, "invalid return from escape %s", escapeName(esc));
          bail();
        }
      }

      case TCall: {
        /* Tail call of explicit program */
        labelPo lbl = C_LBL(getConstant(PC->fst));
        int32 arity = lblArity(lbl);
        methodPo mtd = labelCode(lbl);
        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lbl);
          bail();
        }

        // Overwrite existing arguments and locals
        ptrPo tgt = &arg(argCount(PROG));
        ptrPo src = SP + arity; /* base of argument vector */

        for (int ix = 0; ix < arity; ix++)
          *--tgt = *--src; /* copy the argument vector */
        PC = entryPoint(mtd);
        PROG = mtd;
        ARGS = SP = tgt;
        continue; /* Were done */
      }

      case TOCall: {
        /* Tail call */
        int32 arity = PC->fst;
        termPo cl = pop();
        if (!isClosure(cl)) {
          logMsg(logFile, "Calling non-closure %T", cl);
          bail();
        }
        closurePo obj = C_CLOSURE(cl);
        labelPo lb = closureLabel(obj);

        if (lblArity(lb) != arity) {
          logMsg(logFile, "closure %T does not have correct arity %d", obj, arity);
          bail();
        }

        push(closureFree(obj)); // Put the free term back on the stack
        methodPo mtd = labelCode(lb); /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lb);
          bail();
        }

        // Overwrite existing arguments and locals
        ptrPo tgt = &arg(argCount(PROG));
        ptrPo src = SP + arity; /* base of argument vector */

        for (int ix = 0; ix < arity; ix++)
          *--tgt = *--src; /* copy the argument vector */
        PC = entryPoint(mtd);
        PROG = mtd;
        ARGS = SP = tgt;
        continue; /* Were done */
      }

      case Entry: {
        if (!stackRoom(stackDelta(PROG) + FrameCellCount)) {
          saveRegisters();
          integer Amnt = stackDelta(PROG) + FrameCellCount;
          P->stk = glueOnStack(H, STK, maximum(stackHwm(STK), (STK->sze * 3) / 2 + (Amnt)), codeArity(PROG));
          restoreRegisters();
          if (!stackRoom(Amnt)) {
            logMsg(logFile, "cannot extend stack sufficiently");
            bail();
          }

#ifdef TRACESTACK
          if (traceStack > noTracing)
            verifyStack(STK, H);
#endif
        }

        integer height = PC->fst;
        for (int32 ix = 0; ix < height; ix++)
          push(voidEnum);

        incEntryCount(PROG); // Increment number of times program called

        PC++;
        continue;
      };

      case Ret: {
        /* return from function */
        termPo retVal = *SP; /* return value */

        assert(FP > baseFrame(STK));

        SP = &arg(argCount(PROG)); // Just above arguments to current call
        PROG = FP->prog;
        ARGS = FP->args;
        PC = FP->link;
        FP--;

        push(retVal); /* push return value */
        continue; /* and carry on regardless */
      }

      case XRet: {
        /* exceptional return from function */
        termPo retVal = *SP; /* return value */

        assert(FP > baseFrame(STK));

        PROG = FP->prog;
        ARGS = FP->args;
        PC = FP->link - 1;
        FP--;

        PC += PC->alt + 1;

        int32 height = PC->fst;
        SP = &local(lclCount(PROG) + height - 1);
        PC += PC->alt + 1;

        push(retVal); /* push return value */
        continue; /* and carry on regardless */
      }

      case Block: {
        PC++;
        continue;
      }

      case Break: {
        PC += PC->alt + 1;
        int32 height = PC->fst;
        SP = &local(lclCount(PROG) + height);
        PC += PC->alt + 1;
        continue;
      }

      case Loop: {
        PC += PC->alt + 1;
        int32 height = PC->fst;
        SP = &local(lclCount(PROG) + height);
        PC++;
        continue;
      }

      case Result: {
        /* return a value from a block */
        termPo reslt = pop();
        PC += PC->alt + 1;
        int32 height = PC->fst;
        SP = &local(lclCount(PROG) + height - 1);
        PC += PC->alt + 1;
        push(reslt);
        continue; /* and carry after reset block */
      }

      case Drop: {
        SP++; /* drop tos */
        PC++;
        continue;
      }

      case Dup: {
        /* duplicate tos */
        termPo tos = *SP;
        *--SP = tos;
        PC++;
        continue;
      }

      case Rot: {
        // Pull up nth element of stack
        int32 cnt = PC->fst;
        termPo tmp = SP[0];

        for (int32 ix = 0; ix < cnt; ix++) {
          SP[ix] = SP[ix + 1];
        }
        SP[cnt] = tmp;
        PC++;
        continue;
      }

      case Pick: {
        // Reset stack, keeping top elements
        int32 depth = PC->fst;
        int32 keep = PC->alt;

        ptrPo src = &SP[keep];
        ptrPo tgt = &SP[depth + keep];

        for (int32 ix = 0; ix < keep; ix++) {
          *--tgt = *--src;
        }
        SP = tgt;
        PC++;
        continue;
      }

      case Rst: {
        int32 height = PC->fst;
        SP = &local(lclCount(PROG) + height);
        PC++;
        continue;
      }

      case Fiber: {
        // The top of a stack should be a binary lambda
        termPo fiberLambda = pop();
        saveRegisters();
        stackPo child = newStack(H, fiberLambda);
        restoreRegisters();
        push(child); // We return the new stack
        PC++;
        continue;
      }

      case Suspend: {
        // Suspend identified fiber.
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
      case Retire: {
        // Similar to suspend, except that we trash the suspending stack
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
        saveRegisters(); // Seal off the current stack
        assert(stackState(STK) == active);
        P->stk = dropStack(STK);
        restoreRegisters();
        push(val);
        continue;
      }

      case LdV: {
        push(voidEnum); /* load void */
        PC++;
        continue;
      }

      case LdC: /* load constant value */
        push(getConstant(PC->fst));
        PC++;
        continue;

      case LdA: {
        push(arg(PC->fst)); /* load argument */
        PC++;
        continue;
      }

      case LdL: {
        push(local(PC->fst)); /* load local */
        PC++;
        continue;
      }

      case LdG: {
        globalPo glb = findGlobalVar(PC->fst);

        if (glbIsSet(glb)) {
          termPo gval = getGlobal(glb);

          check(gval != Null, "undefined global");
          check(stackRoom(1), "unexpected stack overflow");

          push(gval); /* load a global variable */
          PC++;
          continue;
        } else {
          labelPo glbLbl = findLbl(globalVarName(glb), 0);
          if (glbLbl == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }
          methodPo glbThnk = labelCode(glbLbl); /* set up for object call */

          if (glbThnk == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }

          FP++;
          FP->prog = PROG;
          FP->link = PC + 1;
          FP->args = ARGS;

          PROG = glbThnk;
          ARGS = SP;
          PC = entryPoint(glbThnk);

          continue;
        }
      }

      case CInt:
      case CChar:
      case CFlt: {
        termPo lt = getConstant(PC->fst);
        termPo tx = pop();

        if (lt != tx) {
          breakBlock();
          continue;
        } else {
          PC++;
          continue;
        }
      }

      case CLit: {
        termPo l = getConstant(PC->fst);
        termPo t = pop();

        if (!sameTerm(l, t)) {
          breakBlock();
        } else
          PC++;
        continue;
      }

      case CLbl: {
        labelPo l = C_LBL(getConstant(PC->fst));
        termPo t = pop();

        if (isNormalPo(t)) {
          normalPo cl = C_NORMAL(t);
          if (sameLabel(l, termLbl(cl))) {
            PC++;
            continue;
          }
        }
        breakBlock(); // First jump to the block
        continue;
      }

      case Nth: {
        termPo t = pop();

        normalPo cl = C_NORMAL(t); /* which term? */
        push(nthElem(cl, PC->fst));

        PC++;
        continue;
      }

      case StL: {
        local(PC->fst) = pop();
        PC++;
        continue;
      }

      case StV: {
        local(PC->fst) = voidEnum;
        PC++;
        continue;
      }
      case TL: {
        local(PC->fst) = top();
        PC++;
        continue;
      }

      case StNth: {
        /* store into a closure */
        termPo top = pop();
        normalPo cl = C_NORMAL(top);
        termPo tos = pop();
        cl->args[PC->fst] = tos;
        PC++;
        continue;
      }

      case StG: {
        termPo val = pop();
        globalPo glb = findGlobalVar(PC->fst);
        setGlobalVar(glb, val); // Update the global variable
        PC++;
        continue;
      }

      case TG: {
        termPo val = top();
        globalPo glb = findGlobalVar(PC->fst);
        setGlobalVar(glb, val); // Update the global variable
        PC++;
        continue;
      }

      case Sav: {
        // Create a new single assignment variable
        checkAlloc(SingleCellCount);
        singlePo sav = singleVar(H);
        push(sav); /* put the structure back on the stack */
        PC++;
        continue;
      }

      case TstSav: {
        singlePo savVr = C_SINGLE(pop());

        termPo Rs = (singleIsSet(savVr) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }

      case LdSav: {
        singlePo savVr = C_SINGLE(pop());

        if (singleIsSet(savVr)) {
          termPo vl = singleVal(savVr);

          check(vl != Null, "undefined single assignment value");

          push(vl); /* load single variable */
          PC++;
          continue;
        } else {
          breakBlock();
          continue;
        }
      }

      case StSav: {
        // Store into single
        singlePo sav = C_SINGLE(pop());
        termPo val = pop();

        if (singleIsSet(sav)) {
          logMsg(logFile, "single %T already set", sav);
          bail();
        }

        setSingle(sav, val); // Update the single variable
        PC++;
        continue;
      }

      case TSav: {
        // Set single and carry on
        singlePo sav = C_SINGLE(pop());
        termPo val = top();

        if (singleIsSet(sav)) {
          logMsg(logFile, "single %T already set", sav);
          bail();
        }

        setSingle(sav, val); // Update the single variable
        PC++;
        continue;
      }
      case Cell: {
        checkAlloc(CellCellCount);
        cellPo cell = newCell(H, pop());
        push(cell);
        PC++;
        continue;
      }

      case Get: {
        cellPo cell = C_CELL(pop());
        push(getCell(cell));
        PC++;
        continue;
      }

      case Assign: {
        cellPo cell = C_CELL(pop());
        termPo vl = pop();
        setCell(cell, vl);
        PC++;
        continue;
      }

      case IAdd: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs + Rhs);
        push(Rs);
        PC++;
        continue;
      }

      case ISub: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs - Rhs);
        push(Rs);
        PC++;
        continue;
      }
      case IMul: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger(Lhs * Rhs);
        push(Rs);
        PC++;
        continue;
      }
      case IDiv: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        if (Rhs == 0) {
          breakOut();
          push(divZero);
        } else {
          termPo Rs = makeInteger(Lhs / Rhs);
          push(Rs);
          PC++;
        }
        continue;
      }
      case IMod: {
        integer denom = integerVal(pop());
        integer numerator = integerVal(pop());

        if (numerator == 0) {
          breakOut();
          push(divZero);
        } else {
          integer reslt = denom % numerator;

          termPo Rs = (termPo) makeInteger(reslt);

          push(Rs);
          PC++;
          continue;
        }
      }
      case IAbs: {
        termPo Trm = pop();
        integer Arg = integerVal(Trm);

        termPo Rs = (Arg < 0 ? makeInteger(-Arg) : Trm);
        push(Rs);
        PC++;
        continue;
      }
      case IEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) == integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case ILt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) < integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case IGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (integerVal(Lhs) >= integerVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case ICmp: {
        termPo i = pop();
        termPo j = pop();

        if (integerVal(i) != integerVal(j)) {
          breakBlock();
          continue;
        }
        PC++;
        continue;
      }
      case CEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (charVal(Lhs) == charVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case CLt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (charVal(Lhs) < charVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case CGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (charVal(Lhs) >= charVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case CCmp: {
        termPo i = pop();
        termPo j = pop();

        if (charVal(i) != charVal(j)) {
          PC += PC->alt + 1;
          assert(validPC(PROG, PC));
          PC += PC->alt + 1;
          continue;
        } else {
          PC++;
          continue;
        }
      }
      case BAnd: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs & (uinteger) Rhs));
        push(Rs);
        PC++;
        continue;
      }
      case BOr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger((integer) ((uinteger) Lhs | (uinteger) Rhs));
        push(Rs);
        PC++;
        continue;
      }
      case BXor: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = makeInteger((integer) ((uinteger) Lhs ^ (uinteger) Rhs));
        push(Rs);
        PC++;
        continue;
      }
      case BNot: {
        integer Lhs = integerVal(pop());

        termPo Rs = makeInteger((integer) (~(uinteger) Lhs));
        push(Rs);
        PC++;
        continue;
      }
      case BLsl: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) ((uinteger) Lhs << (uinteger) Rhs));
        push(Rs);
        PC++;
        continue;
      }
      case BLsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((integer) (((uinteger) Lhs) >> ((uinteger) Rhs)));
        push(Rs);
        PC++;
        continue;
      }
      case BAsr: {
        integer Lhs = integerVal(pop());
        integer Rhs = integerVal(pop());

        termPo Rs = (termPo) makeInteger((Lhs) >> Rhs);
        push(Rs);
        PC++;
        continue;
      }
      case FAdd: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs + Rhs);
        push(Rs);
        PC++;
        continue;
      }
      case FSub: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs - Rhs);
        push(Rs);
        PC++;
        continue;
      }
      case FMul: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        termPo Rs = makeFloat(Lhs * Rhs);
        push(Rs);
        PC++;
        continue;
      }
      case FDiv: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        if (Rhs == 0.0) {
          breakOut();
          push(divZero);
        } else {
          termPo Rs = makeFloat(Lhs / Rhs);
          push(Rs);
          PC++;
          continue;
        }
      }
      case FMod: {
        double Lhs = floatVal(pop());
        double Rhs = floatVal(pop());

        if (Rhs == 0.0) {
          breakOut();
          push(divZero);
        } else {
          termPo Rs = makeFloat(fmod(Lhs, Rhs));
          push(Rs);
          PC++;
          continue;
        }
      }
      case FAbs: {
        double Lhs = floatVal(pop());

        termPo Rs = makeFloat(fabs(Lhs));
        push(Rs);
        PC++;
        continue;
      }
      case FEq: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (nearlyEqual(floatVal(Lhs), floatVal(Rhs), floatVal(Rhs) / 1.0e20) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case FLt: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (floatVal(Lhs) < floatVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case FGe: {
        termPo Lhs = pop();
        termPo Rhs = pop();

        termPo Rs = (floatVal(Lhs) >= floatVal(Rhs) ? trueEnum : falseEnum);
        push(Rs);
        PC++;
        continue;
      }
      case FCmp: {
        termPo x = pop();
        termPo y = pop();

        if (floatVal(x) != floatVal(y)) {
          PC += PC->alt + 1;
          assert(validPC(PROG, PC));
          PC += PC->alt + 1;
          continue;
        } else {
          PC++;
          continue;
        }
      }
      case ICase: {
        int32 mx = PC->fst;

        integer hx = hash61(integerVal(pop())) % mx;

        PC = PC + hx + 1;
        continue;
      }

      case Case: {
        /* case instruction */
        int32 mx = PC->fst;

        termPo tos = pop();
        integer hx = hashTerm(tos) % mx;

        PC = PC + hx + 1;
        continue;
      }

      case IxCase: {
        // Branch based on index of constructor term
        int32 mx = PC->fst;
        termPo top = pop();
        assert(isNormalPo(top));
        labelPo lbl = termLbl(C_NORMAL(top));
        integer hx = lblIndex(lbl) % mx;

        PC = PC + hx + 1;
        continue;
      }

      case Closure: {
        /* heap allocate closure */
        checkAlloc(ClosureCellCount);
        labelPo cd = C_LBL(getConstant(PC->fst));

        if (!labelDefined(cd)) {
          logMsg(logFile, "label %A not defined", cd);
          bail();
        }

        closurePo cl = newClosure(H, cd, pop());

        push(cl); /* put the closure back on the stack */
        PC++;
        continue;
      }

      case Alloc: {
        /* heap allocate term */
        labelPo lbl = C_LBL(getConstant(PC->fst));
        int32 arity = lblArity(lbl);

        checkAlloc(NormalCellCount(arity));
        normalPo cl = allocateStruct(H, lbl); /* allocate a closure on the heap */
        for (int32 ix = 0; ix < arity; ix++)
          cl->args[ix] = pop(); /* fill in free variables by popping from stack */
        push(cl); /* put the structure back on the stack */
        PC++;
        continue;
      }

      case Cmp: {
        termPo i = pop();
        termPo j = pop();

        if (!sameTerm(i, j)) {
          PC += PC->alt + 1;
          assert(validPC(PROG, PC));
          PC += PC->alt + 1;
          continue;
        } else {
          PC++;
          continue;
        }
      }

      case If: {
        termPo i = pop();

        if (i == trueEnum) {
          PC += PC->alt + 1;
          assert(validPC(PROG, PC));
          PC += PC->alt + 1;
          continue;
        } else {
          PC++;
          continue;
        }
      }

      case IfNot: {
        termPo i = pop();

        if (i != trueEnum) {
          PC += PC->alt + 1;
          assert(validPC(PROG, PC));
          PC += PC->alt + 1;
          continue;
        } else {
          PC++;
          continue;
        }
      }

      case Frame: {
        assert(SP == &local(lclCount(PROG) + PC->fst));
        PC++;
        continue;
      }

      case dBug: {
        if (lineDebugging) {
          PC++; // We aim to continue at the next instruction
          saveRegisters();
          enterDebug(P);
          restoreRegisters();
          continue;
        } else {
          PC++;
          continue;
        }
      }

      case maxOpCode:
      case illegalOp:
        syserr("Illegal instruction");
    }
    syserr("PC case issues");
  }
}
