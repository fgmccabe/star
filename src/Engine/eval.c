/*
 * Run-time evaluation for STAR programs.
 *
 * It is expected that this is for fall-back purposes only. Normally, code is JITed during package load.
 */

#include "config.h"
#include "globals.h"
#include "constants.h"
#include "char.h"
#include <math.h>

#include "analyse.h"
#include "errorCodes.h"
#include "escapeP.h"
#include "evalP.h"
#include "normalP.h"
#include "threds.h"

logical collectStats = False;

/*
 * Execute program on a given engine structure
 */
int32 run(enginePo P) {
  heapPo H = P->heap;
  stackPo STK = P->stk;
  framePo FP = STK->fp;
  register ssaInsPo PC = STK->pc; /* Program counter */
  register ptrPo SP = STK->sp; /* Current 'top' of stack (grows down) */
  register methodPo PROG = STK->prog;
  register ptrPo ARGS = STK->args;
  ValueReturn RSLT; // Result variable

  for (;;) {
#ifndef NDEBUG
    pcCount++; /* increment total number of executed */

    if (insDebugging) {
      saveRegisters();
      insDebug(P);
      restoreRegisters();
    }
#endif

    switch (PC->op.op) {
      case sHalt:
        return (int32) integerVal(varble(operand(1)));
      case sAbort: {
        termPo lc = getConstant(operand(1));
        termPo msg = varble(operand(2));
        saveRegisters();
        abort_star(P, lc, msg);

        return Abnormal;
      }

      case sCall: {
        labelPo nProg = C_LBL(getConstant(operand(1)));
        methodPo mtd = labelMtd(nProg); // Which program do we want?
        int32 arity = mtdArity(mtd);
        int32 insSize = arity + 3;

        if (mtd == Null) {
          logMsg(logFile, "label %A not defined", nProg);
          bail();
        }

        FP++;
        FP->prog = PROG;
        FP->link = PC + insSize;
        FP->args = ARGS;

        PROG = mtd;

        for (int32 ax = 0; ax < arity; ax++) {
          *--SP = varble(operand(ax+3));
        }
        ARGS = SP;

#ifndef NOJIT
        if (hasJit(mtd)) {
#ifdef TRACEJIT
        if (traceJit) {
          logMsg(logFile, "entering jitted code %T", mtd);
        }
#endif

        insPo link = PC; // Jit code can override this in the frame
        saveRegisters();
        ReturnStatus ret = (enableSSA ? invokeJitMethodA(P, mtd) : invokeJitMethod(P, mtd));
        restoreRegisters();
        PC = link;

        if (ret == Normal) {
          PC++;
        } else if (PC->op == XCall) {
          termPo exception = pop();
          breakOut();
          push(exception);
        } else {
          logMsg(logFile, "invalid return from %L", nProg);
          bail();
        }
        } else
#endif
        PC = entryPoint(mtd);
        continue;
      }

      case sOCall: {
        /* Call tos a1 .. an -->   */
        termPo cl = varble(operand(1));
        check(isClosure(cl), "calling non-closure");

        closurePo lam = C_CLOSURE(cl);
        labelPo lb = closureLabel(lam);

        int32 arity = lblArity(lb);
        int32 insSize = arity + 3;

        if (lblArity(lb) != operand(2) + 1) {
          logMsg(logFile, "closure %T does not have correct arity %d", lam, arity);
          bail();
        }

        methodPo mtd = labelMtd(lb); /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lb);
          bail();
        }

        FP++; // Guaranteed to have room for the frame
        FP->prog = PROG;
        FP->link = PC + insSize;
        FP->args = ARGS;

        for (int32 ax = 0; ax < arity - 1; ax++) {
          *--SP = varble(operand(ax+3));
        }
        *--SP = closureFree(lam); // Put the free term as the first argument

        PROG = mtd;
        ARGS = SP;
        PC = entryPoint(mtd);
        continue;
      }

      case sEscape: {
        /* call escape */
        int32 escNo = operand(1); /* escape number */
        int32 arity = operand(2);
        int32 insSize = arity + 3;

#ifndef NDEBUG
        if (collectStats)
          recordEscape(escNo);
#endif

        escapePo esc = getEscape(escNo);
        saveRegisters();
        assert(H->topRoot == 0);

        switch (arity) {
          case 8:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)),varble(operand(5)),varble(operand(6)),
                                 varble(operand(7)),varble(operand(8)),varble(operand(9)),varble(operand(10)));
            break;
          case 7:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)),varble(operand(5)),varble(operand(6)),
                                 varble(operand(7)),varble(operand(8)),varble(operand(9)));
            break;
          case 6:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)),varble(operand(5)),varble(operand(6)),
                                 varble(operand(7)),varble(operand(8)));
            break;
          case 5:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)),varble(operand(5)),varble(operand(6)),
                                 varble(operand(7)));
            break;
          case 4:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)),varble(operand(5)),varble(operand(6)));
            break;
          case 3:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)),varble(operand(5)));
            break;
          case 2:
            RSLT = (esc->direct)(P,varble(operand(3)),varble(operand(4)));
            break;
          case 1:
            RSLT = (esc->direct)(P,varble(operand(3)));
            break;
          case 0:
            RSLT = (esc->direct)(P);
            break;
          default:
            logMsg(logFile, "unsupported escape arity %d", arity);
            bail();
        }

        restoreRegisters();
        assert(H->topRoot == 0);
        PC += insSize;
        continue;
      }
      case sRSP: {
        // Respond to a call or escape
        if (RSLT.status == Normal) {
          varble(operand(1)) = RSLT.value;
          PC += 2;
        } else {
          logMsg(logFile, "uncaught exception %T", RSLT.value);
          bail();
        }
        continue;
      }
      case sRSX: {
        // Respond to a call or escape
        if (RSLT.status == Normal) {
          varble(operand(2)) = RSLT.value;
          PC += 3;
        } else {
          returnBlock(1, RSLT.value);
        }
        continue;
      }
      case sTCall: {
        /* Tail call of explicit program */
        labelPo lbl = C_LBL(getConstant(operand(1)));
        int32 arity = lblArity(lbl);
        methodPo mtd = labelMtd(lbl);
        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lbl);
          bail();
        }

        // Overwrite existing arguments and locals
        ptrPo tgt = &varble(argCount(PROG));

        for (int ix = 0; ix < arity; ix++)
          *--tgt = varble(operand(arity+3-ix)); /* copy the argument vector */
        PC = entryPoint(mtd);
        PROG = mtd;
        ARGS = SP = tgt;
        continue; /* Were done */
      }
      case sTOCall: {
        /* Tail lambda call */
        termPo cl = varble(operand(1));
        check(isClosure(cl), "calling non-closure");

        closurePo lam = C_CLOSURE(cl);
        labelPo lb = closureLabel(lam);

        int32 arity = lblArity(lb);
        int32 insSize = arity + 3;

        if (lblArity(lb) != operand(2) + 1) {
          logMsg(logFile, "closure %T does not have correct arity %d", lam, arity);
          bail();
        }

        methodPo mtd = labelMtd(lb); /* set up for object call */

        if (mtd == Null) {
          logMsg(logFile, "no definition for %T", lb);
          bail();
        }

        // Overwrite existing arguments and locals
        ptrPo tgt = &varble(argCount(PROG));

        for (int32 ax = 0; ax < arity - 1; ax++) {
          *--tgt = varble(operand(arity+3-ax));
        }
        *--tgt = closureFree(lam); // Put the free term as the first argument

        PC = entryPoint(mtd);
        PROG = mtd;
        ARGS = SP = tgt;
        continue; /* Were done */
      }
      case sEntry: {
        int32 insSize = 3;
        int32 arity = operand(1);
        int32 lclCount = operand(2);
        int32 amnt = (int32) (stackDelta(PROG) + arity + lclCount + FrameCellCount);
        if (!stackRoom(amnt)) {
          saveRegisters();
          glueOnStack(P, False, maximum(stackHwm(STK), (STK->sze * 3) / 2 + (amnt)), mtdArity(PROG));
          restoreRegisters();
          if (!stackRoom(amnt)) {
            logMsg(logFile, "cannot extend stack sufficiently");
            bail();
          }

#ifdef TRACESTACK
          if (traceStack > noTracing)
            verifyStack(STK, H);
#endif
        }
        for (int32 lx = 1; lx <= lclCount; lx++) {
          varble(-lx) = voidEnum;
        }
        PC += insSize;
        continue;
      };
      case sRtn: {
        /* return from procedure */
        RSLT.value = voidEnum; /* no return value */
        RSLT.status = Normal;

        assert(FP > baseFrame(STK));

        SP = &varble(argCount(PROG)); // Just above arguments to current call
        PROG = FP->prog;
        ARGS = FP->args;
        PC = FP->link;
        FP--;

        assert(SP<=ARGS);

        continue; /* and carry on regardless */
      }
      case sRet: {
        /* return from function */
        RSLT.value = varble(operand(1)); /* return value */
        RSLT.status = Normal;

        assert(FP > baseFrame(STK));

        SP = &varble(argCount(PROG)); // Just above arguments to current call
        PROG = FP->prog;
        ARGS = FP->args;
        PC = FP->link;
        FP--;

        assert(SP<=ARGS);

        continue; /* and carry on regardless */
      }

      case sXRet: {
        /* exceptional return from function */
        RSLT.value = varble(operand(1)); /* return value */
        RSLT.status = Abnormal;

        assert(FP > baseFrame(STK));

        SP = &varble(argCount(PROG)); // Just above arguments to current call
        PROG = FP->prog;
        ARGS = FP->args;
        PC = FP->link;
        FP--;

        assert(SP<=ARGS);

        continue; /* and carry on regardless */
      }
      case sBlock: {
        PC += 2; // Skip over the size marker and the Block instruction itself.
      }
      case sValof: {
        PC += 3; // Skip over the size marker, the Valof instruction and the index of the phi var
        continue;
      }
      case sBreak: {
        breakBlock(1);
        continue;
      }
      case sResult: {
        /* return a value from a block */
        termPo reslt = varble(operand(2));
        returnBlock(1, reslt);
        continue; /* and carry after reset block */
      }
      case sLoop: {
        PC += operand(1);
        PC += 2; // Move to start of actual instructions
        continue;
      }
      case sIf: {
        int32 insSize = 3;
        termPo i = varble(operand(2));

        if (i == trueEnum) {
          breakBlock(1);
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      case sIfNot: {
        int32 insSize = 3;
        termPo i = varble(operand(2));

        if (i != trueEnum) {
          breakBlock(1);
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      case sICase: {
        int32 mx = operand(2);

        integer hx = hash61(integerVal(varble(operand(1)))) % mx;

        PC = PC + hx + 3;
        continue;
      }
      case sCase: {
        /* case instruction */
        int32 mx = operand(2);

        integer hx = hashTerm(varble(operand(1))) % mx;

        PC = PC + hx + 3;
        continue;
      }
      case sIxCase: {
        // Branch based on index of constructor term
        int32 mx = operand(2);
        termPo gov = varble(operand(1));
        assert(isNormalPo(gov));
        labelPo lbl = termLbl(C_NORMAL(gov));
        integer hx = lblIndex(lbl) % mx;

        PC = PC + hx + 3;
        continue;
      }
      case sCLbl: {
        labelPo l = C_LBL(getConstant(operand(1)));
        termPo t = varble(operand(3));
        int32 insSize = 4;

        if (isNormalPo(t)) {
          normalPo cl = C_NORMAL(t);
          if (sameLabel(l, termLbl(cl))) {
            PC += insSize;
            continue;
          }
        }
        breakBlock(2); // Jump out of the right block
        continue;
      }
      case sCInt:
      case sCChar: {
        int32 insSize = 4;
        termPo lt = getConstant(operand(1));
        termPo tx = varble(operand(3));

        if (lt != tx) {
          breakBlock(2);
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      case sCFlt:
      case sCLit: {
        int32 insSize = 4;
        termPo lt = getConstant(operand(1));
        termPo tx = varble(operand(3));

        if (lt != tx) {
          breakBlock(2);
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      case sMC: {
        /* load constant value */
        int32 insSize = 3;
        varble(operand(1)) = getConstant(operand(2));
        PC += insSize;
        continue;
      }
      case sMv: {
        int32 insSize = 3;
        varble(operand(1)) = varble(operand(2));
        PC += insSize;
        continue;
      }
      case sLG: {
        globalPo glb = findGlobalVar(operand(1));
        int32 insSize = 2;

        if (glbIsSet(glb)) {
          termPo gval = getGlobal(glb);

          check(gval != Null, "undefined global");
          check(stackRoom(1), "unexpected stack overflow");

          RSLT.value = gval;
          RSLT.status = Normal;
          PC += insSize;
          continue;
        } else {
          labelPo glbLbl = findLbl(globalVarName(glb), 0);
          if (glbLbl == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }
          methodPo glbThnk = labelMtd(glbLbl); /* set up for object call */

          if (glbThnk == Null) {
            logMsg(logFile, "no definition for global %s", globalVarName(glb));
            bail();
          }

          FP++;
          FP->prog = PROG;
          FP->link = PC + insSize;
          FP->args = ARGS;

          PROG = glbThnk;
          ARGS = SP;
          PC = entryPoint(glbThnk);
          continue;
        }
      }
      case sSG: {
        int32 insSize = 3;
        termPo val = varble(operand(2));
        globalPo glb = findGlobalVar(operand(1));
        setGlobalVar(glb, val); // Update the global variable
        PC += insSize;
        continue;
      }
      case sSav: {
        int32 insSize = 2;
        // Create a new single assignment variable
        checkAlloc(SingleCellCount);
        singlePo sav = singleVar(H);
        varble(operand(1)) = (termPo) sav;
        PC += insSize;
        continue;
      }
      case sLdSav: {
        int32 insSize = 4;
        singlePo savVr = C_SINGLE(varble(operand(3)));

        if (singleIsSet(savVr)) {
          termPo vl = singleVal(savVr);

          check(vl != Null, "undefined single assignment value");
          varble(operand(1)) = vl;
          PC += insSize;
          continue;
        } else {
          breakBlock(2);
          continue;
        }
      }
      case sTstSav: {
        int32 insSize = 3;
        singlePo savVr = C_SINGLE(varble(operand(2)));

        varble(operand(1)) = singleIsSet(savVr) ? trueEnum : falseEnum;
        PC += insSize;
        continue;
      }
      case sStSav: {
        // Store into single
        int32 insSize = 3;
        singlePo savVr = C_SINGLE(varble(operand(1)));

        if (singleIsSet(savVr)) {
          logMsg(logFile, "single %T already set", savVr);
          bail();
        }

        setSingle(savVr, varble(operand(2))); // Update the single variable
        PC += insSize;
        continue;
      }
      case sCell: {
        int32 insSize = 3;
        checkAlloc(CellCellCount);
        varble(operand(1)) = (termPo) newCell(H, varble(operand(2)));
        PC += insSize;
        continue;
      }
      case sGet: {
        int32 insSize = 3;
        varble(operand(1)) = getCell(C_CELL(varble(operand(2))));
        PC += insSize;
        continue;
      }
      case sAssign: {
        int32 insSize = 3;
        cellPo cell = C_CELL(getCell(C_CELL(varble(operand(1)))));
        setCell(cell, varble(operand(2)));
        PC += insSize;
        continue;
      }
      case sNth: {
        int32 insSize = 4;
        normalPo t = C_NORMAL(varble(operand(3)));
        varble(operand(1)) = nthElem(t,operand(2));
        PC += insSize;
        continue;
      }
      case sStNth: {
        int32 insSize = 4;
        normalPo t = C_NORMAL(varble(operand(1)));
        setNth(t,operand(2),varble(operand(3)));
        PC += insSize;
        continue;
      }
      case sIAdd: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        termPo Rs = makeInteger(Lhs + Rhs);
        varble(operand(1)) = Rs;
        PC += insSize;
        continue;
      }
      case sISub: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        termPo Rs = makeInteger(Lhs - Rhs);
        varble(operand(1)) = Rs;
        PC += insSize;
        continue;
      }
      case sIMul: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        termPo Rs = makeInteger(Lhs * Rhs);
        varble(operand(1)) = Rs;
        PC += insSize;
        continue;
      }
      case sIDiv: {
        int32 insSize = 5;
        integer Lhs = integerVal(varble(operand(3)));
        integer Rhs = integerVal(varble(operand(4)));

        if (Rhs == 0) {
          returnBlock(1, divZero);
        } else {
          varble(operand(2)) = makeInteger(Lhs / Rhs);
        }
        PC += insSize;
        continue;
      }
      case sIMod: {
        int32 insSize = 5;
        integer Lhs = integerVal(varble(operand(3)));
        integer Rhs = integerVal(varble(operand(4)));

        if (Rhs == 0) {
          returnBlock(1, divZero);
        } else {
          varble(operand(2)) = makeInteger(Lhs % Rhs);
        }
        PC += insSize;
        continue;
      }
      case sIAbs: {
        int32 insSize = 3;
        integer Lhs = integerVal(varble(operand(2)));
        varble(operand(1)) = (Lhs < 0 ? makeInteger(-Lhs) : varble(operand(2)));
        PC += insSize;
        continue;
      }
      case sIEq: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = (Lhs == Rhs ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sILt: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = (Lhs < Rhs ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sIGe: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = (Lhs >= Rhs ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sCEq: {
        int32 insSize = 4;
        termPo Lhs = varble(operand(2));
        termPo Rhs = varble(operand(3));

        varble(operand(1)) = (charVal(Lhs) == charVal(Rhs) ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sCLt: {
        int32 insSize = 4;
        termPo Lhs = varble(operand(2));
        termPo Rhs = varble(operand(3));

        varble(operand(1)) = (charVal(Lhs) < charVal(Rhs) ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sCGe: {
        int32 insSize = 4;
        termPo Lhs = varble(operand(2));
        termPo Rhs = varble(operand(3));

        varble(operand(1)) = (charVal(Lhs) >= charVal(Rhs) ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sBAnd: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = makeInteger((integer) ((uinteger) Lhs & (uinteger) Rhs));
        PC += insSize;
        continue;
      }
      case sBOr: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = makeInteger((integer) ((uinteger) Lhs | (uinteger) Rhs));
        PC += insSize;
        continue;
      }
      case sBXor: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = makeInteger((integer) ((uinteger) Lhs ^ (uinteger) Rhs));
        PC += insSize;
        continue;
      }
      case sBLsl: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = makeInteger((integer) ((uinteger) Lhs << (uinteger) Rhs));
        PC += insSize;
        continue;
      }
      case sBLsr: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = makeInteger((integer) ((uinteger) Lhs >> (uinteger) Rhs));
        PC += insSize;
        continue;
      }
      case sBAsr: {
        int32 insSize = 4;
        integer Lhs = integerVal(varble(operand(2)));
        integer Rhs = integerVal(varble(operand(3)));

        varble(operand(1)) = makeInteger(Lhs >> Rhs);
        PC += insSize;
        continue;
      }
      case sBNot: {
        int32 insSize = 3;
        integer Rhs = integerVal(varble(operand(2)));

        varble(operand(1)) = makeInteger((integer) ~(uinteger) Rhs);
        PC += insSize;
        continue;
      }
      case sFAdd: {
        int32 insSize = 4;
        checkAlloc(FloatCellCount);

        double Lhs = floatVal(varble(operand(2)));
        double Rhs = floatVal(varble(operand(3)));

        varble(operand(1)) = makeFloat(H, Lhs + Rhs);
        PC += insSize;
        continue;
      }
      case sFSub: {
        int32 insSize = 4;
        checkAlloc(FloatCellCount);

        double Lhs = floatVal(varble(operand(2)));
        double Rhs = floatVal(varble(operand(3)));

        varble(operand(1)) = makeFloat(H, Lhs - Rhs);
        PC += insSize;
        continue;
      }
      case sFMul: {
        int32 insSize = 4;
        checkAlloc(FloatCellCount);

        double Lhs = floatVal(varble(operand(2)));
        double Rhs = floatVal(varble(operand(3)));

        varble(operand(1)) = makeFloat(H, Lhs * Rhs);
        PC += insSize;
        continue;
      }
      case sFDiv: {
        int32 insSize = 5;
        checkAlloc(FloatCellCount);

        double Lhs = floatVal(varble(operand(3)));
        double Rhs = floatVal(varble(operand(4)));

        if (Rhs == 0) {
          breakBlock(1);
          RSLT.value = divZero;
          RSLT.status = Abnormal;
        } else {
          varble(operand(2)) = makeFloat(H, Lhs / Rhs);
        }
        PC += insSize;
        continue;
      }
      case sFMod: {
        int32 insSize = 5;
        checkAlloc(FloatCellCount);

        double Lhs = floatVal(varble(operand(3)));
        double Rhs = floatVal(varble(operand(4)));

        if (Rhs == 0) {
          breakBlock(1);
          RSLT.value = divZero;
          RSLT.status = Abnormal;
        } else {
          varble(operand(2)) = makeFloat(H, fmod(Lhs, Rhs));
        }
        PC += insSize;
        continue;
      }
      case sFAbs: {
        int32 insSize = 3;
        checkAlloc(FloatCellCount);

        double Rhs = floatVal(varble(operand(2)));

        varble(operand(1)) = makeFloat(H, Rhs < 0.0 ? -Rhs : Rhs);
        PC += insSize;
        continue;
      }
      case sFEq: {
        int32 insSize = 4;
        double Lhs = floatVal(varble(operand(2)));
        double Rhs = floatVal(varble(operand(3)));

        varble(operand(1)) = (Lhs == Rhs ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sFLt: {
        int32 insSize = 4;
        double Lhs = floatVal(varble(operand(2)));
        double Rhs = floatVal(varble(operand(3)));

        varble(operand(1)) = (Lhs < Rhs ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sFGe: {
        int32 insSize = 4;
        double Lhs = floatVal(varble(operand(2)));
        double Rhs = floatVal(varble(operand(3)));

        varble(operand(1)) = (Lhs >= Rhs ? trueEnum : falseEnum);
        PC += insSize;
        continue;
      }
      case sAlloc: {
        int32 insSize = 4;
        /* heap allocate term */
        labelPo lbl = C_LBL(getConstant(operand(1)));
        int32 arity = lblArity(lbl);

        checkAlloc(NormalCellCount(arity));
        normalPo cl = allocateStruct(H, lbl); /* allocate a closure on the heap */
        for (int32 ix = 0; ix < arity; ix++)
          cl->args[ix] = varble(operand(ix+3)); /* fill in free variables by getting from locals */
        varble(operand(1)) = (termPo) cl; /* put the structure back on the stack */
        PC += arity + 4;
        continue;
      }
      case sClosure: {
        /* heap allocate closure */
        checkAlloc(ClosureCellCount);
        int32 insSize = 4;
        labelPo lbl = C_LBL(getConstant(operand(1)));
        if (!labelDefined(lbl)) {
          logMsg(logFile, "label %A not defined", lbl);
          bail();
        }
        int32 arity = lblArity(lbl);

        closurePo cl = newClosure(H, lbl, varble(operand(3)));
        varble(operand(2)) = (termPo) cl; /* put the structure back on the stack */
        PC += insSize;
        continue;
      }
      case sDrop: {
        logMsg(logFile, "Drop not implemented");
        bail();
        continue;
      }
      case sBump: {
        logMsg(logFile, "Bump not implemented");
        bail();
        continue;
      }
      case sFiber: {
        int32 insSize = 3;
        termPo fiberLambda = varble(operand(2));
        saveRegisters();
        stackPo child = newStack(P, False, fiberLambda);
        restoreRegisters();
        varble(operand(1)) = (termPo) child;
        PC += insSize;
        continue;
      }
      case sSuspend: {
        // Suspend identified fiber.
        int32 insSize = 3;
        stackPo stack = C_STACK(varble(operand(1)));
        termPo event = varble(operand(1));

        if (stackState(stack) != active) {
          logMsg(logFile, "tried to suspend %s fiber %T", stackStateName(stackState(stack)), stack);
          bail();
        } else {
          PC += insSize;
          saveRegisters();
          detachStack(P, stack);
          restoreRegisters();
          RSLT.value = event;
          RSLT.status = Normal;
          continue;
        }
      }
      case sResume: {
        int32 insSize = 3;
        stackPo stack = C_STACK(varble(operand(1)));
        termPo event = varble(operand(2));

        if (stackState(stack) != suspended) {
          logMsg(logFile, "tried to resume %s stack %T", stackStateName(stackState(stack)), stack);
          bail();
        } else {
          PC += insSize;
          saveRegisters();
          attachStack(P, stack);
          restoreRegisters();
          RSLT.value = event;
          RSLT.status = Normal;
          continue;
        }
      }
      case sRetire: {
        // Similar to suspend, except that we trash the suspending stack
        stackPo fiber = C_STACK(varble(operand(1)));
        termPo event = varble(operand(2));

        if (stackState(fiber) != active) {
          logMsg(logFile, "tried to retire a non-active stack %T", fiber);
          bail();
        } else {
          saveRegisters();
          detachStack(P, fiber);
          dropStack(fiber);
          restoreRegisters();
          RSLT.value = event;
          RSLT.status = Normal;
#ifdef TRACESTACK
          if (traceStack > noTracing)
            verifyStack(STK, H);
#endif
          continue;
        }
      }
      case sUnderflow: {
        saveRegisters(); // Seal off the current stack
        assert(stackState(STK) == active);
        P->stk = dropStack(STK);
        restoreRegisters();
        continue;
      }
      case sLine: {
        int32 insSize = 2;
        if (lineDebugging) {
          termPo loc = getConstant(operand(1));
          PC += insSize; // We aim to continue at the next instruction
          saveRegisters();
          lineDebug(P, loc);
          restoreRegisters();
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      case sdBug: {
        int32 insSize = 2;
        if (lineDebugging) {
          termPo loc = getConstant(operand(1));
          PC += insSize; // We aim to continue at the next instruction
          saveRegisters();
          enterDebugger(P, loc);
          restoreRegisters();
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      case sBind: {
        int32 insSize = 3;
        if (lineDebugging) {
          termPo vrNm = getConstant(operand(1));
          PC += insSize; // We aim to continue at the next instruction
          saveRegisters();
          bindDebug(P, vrNm, operand(2));
          restoreRegisters();
          continue;
        } else {
          PC += insSize;
          continue;
        }
      }
      default:
        syserr("Illegal instruction");
    }
    syserr("PC case issues");
  }
}

#ifndef NOJIT
// Directly enter jitted code
ReturnStatus exec(enginePo P) {
  stackPo STK = P->stk;
  register methodPo PROG = STK->prog;

#ifdef TRACEJIT
if (traceJit) {
  logMsg(logFile, "entering jitted code %T", PROG);
}
#endif

return invokeJitMethod(P, PROG);
}
#endif
