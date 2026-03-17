//
// Created by Francis McCabe on 11/4/25.
//

#include "code.h"
#include "analyse.h"
#include "analyseP.h"
#include "debugP.h"
#include "ssaOps.h"
#include "codeP.h"
#include "array.h"
#include "hash.h"
#include "constants.h"
#include "intervalSet.h"

#ifdef TRACEJIT
tracingLevel traceSSA = noTracing;
#endif

scopePo checkScope(scopePo scope, int32 tgt);

#define operand(x) (code[pc+(x)].op.ltrl)

retCode analyseBlock(analysisPo analysis, scopePo parent, ssaInsPo code, int32 start, int32 pc, int32 limit) {
  ScopeBlock scope = {
    .start = start, .limit = limit, .parent = parent
  };

  retCode ret = Ok;

  for (; ret == Ok && pc < limit; pc++) {
    switch (code[pc].op.op) {
      case sHalt: {
        recordVariableUse(analysis,operand(1), pc);
        pc += 2;
        continue;
      }
      case sAbort: {
        recordVariableUse(analysis,operand(2), pc);
        setSafePoint(analysis, pc);
        pc += 2;
        continue;
      }
      case sCall:
      case sEscape: {
        int32 arity = operand(2);

        for (int32 ax = 0; ax < arity; ax++)
          recordVariableUse(analysis,operand(3+ax), pc);

        setSafePoint(analysis, pc);
        pc += arity + 3;
        continue;
      }
      case sOCall: {
        int32 arity = operand(2);
        recordVariableUse(analysis,operand(1), pc);
        for (int32 ax = 0; ax < arity; ax++)
          recordVariableUse(analysis,operand(3+ax), pc);

        setSafePoint(analysis, pc);
        pc += arity + 3;
        continue;
      }
      case sTCall: {
        int32 arity = operand(2);
        recordVariableUse(analysis,operand(1), pc);
        for (int32 ax = 0; ax < arity; ax++)
          recordVariableUse(analysis,operand(3+ax), pc);
        pc += arity + 3;
        continue;
      }
      case sTOCall: {
        int32 arity = operand(2);
        recordVariableUse(analysis,operand(1), pc);
        for (int32 ax = 0; ax < arity; ax++)
          recordVariableUse(analysis,operand(3+ax), pc);
        pc += arity + 3;
        continue;
      }
      case sRSP: {
        recordVariableStart(analysis,operand(1), phi, pc);
        pc += 2;
        continue;
      }
      case sRSX: {
        recordVariableStart(analysis,operand(2), local, pc);
        pc += 3;
        continue;
      }
      case sEntry: {
        int32 arity = operand(1);
        for (int32 ax = 0; ax < arity; ax++)
          newArgVar(analysis, ax);

        int32 count = operand(2);
        for (int32 lx = 0; lx < count; lx++)
          newLocalVar(analysis, -(lx + 1));
        pc += 3;
        continue;
      }
      case sRet:
      case sXRet: {
        recordVariableUse(analysis,operand(1), pc);
        pc += 2;
        continue;
      }
      case sBlock: {
        int32 skipLen = operand(1);
        ret = analyseBlock(analysis, &scope, code, pc, pc + 1, pc + skipLen + 1);
        pc += skipLen + 1;
        continue;
      }
      case sValof: {
        recordVariableStart(analysis,operand(1), phi, pc);
        int32 skipLen = operand(2);
        ret = analyseBlock(analysis, &scope, code, pc, pc + 2, pc + skipLen + 2);
        pc += skipLen + 2;
        continue;
      }
      case sBreak:
      case sLoop: {
        pc += 2;
        continue;
      }
      case sResult: {
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sIf:
      case sIfNot: {
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sCase:
      case sICase:
      case sIxCase: {
        recordVariableUse(analysis,operand(1), pc);
        int32 caseLimit = operand(2);
        analyseBlock(analysis, &scope, code, pc, pc + 2, pc + caseLimit + 2);
        pc += caseLimit + 2;
        continue;
      }
      case sCLit:
      case sCInt:
      case sCFlt:
      case sCChar:
      case sCLbl: {
        recordVariableUse(analysis,operand(3), pc);
        pc += 3;
        continue;
      }
      case sMC: {
        recordVariableStart(analysis,operand(1), local, pc);
        pc += 3;
        continue;
      }
      case sMv: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sLG: {
        pc += 2;
        continue;
      }
      case sSG: {
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sSav: {
        recordVariableStart(analysis,operand(1), local, pc);
        pc += 2;
        continue;
      }
      case sLdSav: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sTstSav: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sStSav: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sCell:
      case sGet: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sAssign: {
        recordVariableUse(analysis,operand(1), pc);
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sNth: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sStNth: {
        recordVariableUse(analysis,operand(1), pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sIAdd:
      case sISub:
      case sIMul: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sIDiv:
      case sIMod: {
        recordVariableStart(analysis,operand(2), local, pc);
        recordVariableUse(analysis,operand(3), pc);
        recordVariableUse(analysis,operand(4), pc);
        pc += 5;
        continue;
      }
      case sIAbs: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sIEq:
      case sILt:
      case sIGe:
      case sCEq:
      case sCLt:
      case sCGe: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sBAnd:
      case sBOr:
      case sBXor:
      case sBLsl:
      case sBLsr:
      case sBAsr: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sBNot: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        pc += 3;
        continue;
      }
      case sFAdd:
      case sFSub:
      case sFMul: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        recordVariableUse(analysis,operand(3), pc);
        setSafePoint(analysis, pc);
        pc += 4;
      }
      case sFDiv:
      case sFMod: {
        recordVariableStart(analysis,operand(2), local, pc);
        recordVariableUse(analysis,operand(3), pc);
        recordVariableUse(analysis,operand(4), pc);
        setSafePoint(analysis, pc);
        pc += 5;
        continue;
      }
      case sFAbs: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        setSafePoint(analysis, pc);
        pc += 3;
        continue;
      }
      case sFEq:
      case sFLt:
      case sFGe: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        recordVariableUse(analysis,operand(3), pc);
        pc += 4;
        continue;
      }
      case sAlloc: {
        recordVariableStart(analysis,operand(2), local, pc);
        int32 arity = operand(3);

        for (int32 ax = 0; ax < arity; ax++)
          recordVariableUse(analysis,operand(ax+3), pc);
        setSafePoint(analysis, pc);
        pc += arity + 4;
        continue;
      }
      case sClosure: {
        recordVariableStart(analysis,operand(2), local, pc);
        recordVariableUse(analysis,operand(3), pc);
        setSafePoint(analysis, pc);
        pc += 4;
        continue;
      }
      case sDrop:
      case sBump: {
        recordVariableUse(analysis,operand(1), pc);
        pc += 2;
        continue;
      }
      case sFiber: {
        recordVariableStart(analysis,operand(1), local, pc);
        recordVariableUse(analysis,operand(2), pc);
        setSafePoint(analysis, pc);
        pc += 3;
        continue;
      }
      case sResume:
      case sSuspend:
      case sRetire: {
        recordVariableUse(analysis,operand(1), pc);
        recordVariableUse(analysis,operand(2), pc);
        setSafePoint(analysis, pc);
        pc += 3;
        continue;
      }
      case sUnderflow: {
        pc++;
        continue;
      }
      case sLine: {
        if (lineDebugging) {
          setSafePoint(analysis, pc);
        }
        pc += 2;
        continue;
      }
      case sBind: {
        recordVariableUse(analysis,operand(2), pc);
        if (lineDebugging) {
          setSafePoint(analysis, pc);
        }
        pc += 3;
        continue;
      }
      case sdBug: {
        if (lineDebugging) {
          setSafePoint(analysis, pc);
        }
        pc+=2;
        continue;
      }
      default:
        return Error;
    }
  }
  return ret;
}

scopePo checkScope(scopePo scope, int32 tgt) {
  while (scope != Null) {
    if (tgt == scope->start) {
      return scope;
    }
    scope = scope->parent;
  }
  return Null;
}

static void markVarSlots(analysisPo analysis, methodPo mtd);

retCode analyseMethod(methodPo mtd, analysisPo results) {
  return analyseSpecialMethod(mtd, 0, results);
}

void markVarSlots(analysisPo analysis, methodPo mtd) {
  arrayPo ranges = varRanges(analysis);

  intervalSetPo allocMap = newIntervalSet();

  int32 start = 0;
  int32 tableSize = arrayCount(ranges);

  while (start < tableSize) {
    varDescPo var = *(varDescPo *) nthEntry(ranges, start);

    if (isSafe(analysis, var))
      markVarAsRegister(var);
    else
      markVarAsMemory(var);
    start++;
  }

  assert(intervalSetIsEmpty(allocMap));

  deleteIntervalSet(allocMap);
  eraseArray(ranges,Null,Null);
}

int32 slotCount(analysisPo analysis) {
  return (int32) hashSize(analysis->vars);
}
