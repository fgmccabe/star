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

retCode analyseBlock(analysisPo analysis, scopePo scope, ssaInsPo code, int32 pc, int32 limit) {
  retCode ret = Ok;

  while (ret == Ok && pc < limit) {
    switch (code[pc].op.op) {
    case sHalt: {
      int32 nextPc = pc + 2;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      pc = nextPc;
      continue;
    }
    case sAbort: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sCall:
    case sEscape: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;

      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis, scope, operand(3+ax), nextPc);

      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sOCall: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis, scope, operand(3+ax), nextPc);

      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sTCall: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;
      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis, scope, operand(3+ax), nextPc);
      pc = nextPc;
      continue;
    }
    case sTOCall: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis, scope, operand(3+ax), nextPc);
      pc = nextPc;
      continue;
    }
    case sRSP: {
      int32 nextPc = pc + 2;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      pc = nextPc;
      continue;
    }
    case sRSX: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(2), local, nextPc, nextPc);
      scopePo valofScope = checkScope(scope, pc + operand(1));
      markPhiVariable(analysis, valofScope, 0);
      pc = nextPc;
      continue;
    }
    case sEntry: {
      int32 arity = operand(1);
      for (int32 ax = 0; ax < arity; ax++) {
        newArgVar(analysis, ax);
      }

      int32 count = operand(2);
      for (int32 lx = 0; lx < count; lx++) {
        newLocalVar(analysis, -(lx + 1));
      }
      pc += 3;
      continue;
    }
    case sRet:
    case sXRet: {
      int32 nextPc = pc + 2;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      pc = nextPc;
      continue;
    }
    case sRtn: {
      pc++;
      continue;
    }
    case sLoop: {
      int32 skipLen = operand(1);
      int32 nextPc = pc + skipLen;

      ScopeBlock block = {
        .start = pc, .end = limit, .parent = scope, .kind = sLoop, .phiCnt = 0
      };
      ret = analyseBlock(analysis, &block, code, pc + 2, nextPc);
      markLoopVariables(analysis, &block);
      pc = nextPc;

      continue;
    }
    case sBlock: {
      int32 arity = operand(1);
      int32 blockLen = operand(arity+2);
      int32 nextPc = pc + blockLen;
      varDescPo phiVars[arity];
      ScopeBlock block = {
        .start = pc, .end = nextPc, .parent = scope, .kind = sBlock, .phiCnt = arity, .phiVars = phiVars
      };

      for (int32 px = 0; px < arity; px++) {
        phiVars[px] = newPhiVar(analysis, operand(px+2), &block);
      }

      ret = analyseBlock(analysis, &block, code, pc + arity + 3, nextPc);

      for (int32 px = 0; px < arity; px++) {
        recordVariableStart(analysis, phiVars[px]->varNo, valof, pc, nextPc);
      }
      pc = nextPc;
      continue;
    }
    case sBreak: {
      pc += 2;
      continue;
    }
    case sCont: {
      int32 nextPc = pc + 2;
      scopePo tgtScope = checkScope(scope, pc + operand(1));
      assert(tgtScope->phiCnt==0);

      markLoopVariables(analysis, tgtScope);
      pc = nextPc;
    }
    case sResult: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;

      scopePo tgtScope = checkScope(scope, pc + operand(1));
      assert(tgtScope->phiCnt==arity); // Already verified

      for (int32 ax = 0; ax < arity; ax++) {
        recordVariableUse(analysis, scope, operand(ax+3), nextPc);
      }
      pc = nextPc;
      continue;
    }
    case sIf:
    case sIfNot: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sCase:
    case sICase: {
      int32 caseLimit = operand(2);
      int32 nextPc = pc + caseLimit;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      analyseBlock(analysis, scope, code, pc + 3, nextPc);
      pc = nextPc;
      continue;
    }
    case sIxCase: {
      int32 caseLimit = operand(2);
      int32 nextPc = pc + caseLimit + 3;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      analyseBlock(analysis, scope, code, pc + 3, nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sCLit:
    case sCFlt: {
      int32 nextPc = pc + 4;
      recordVariableUse(analysis, scope, operand(3), nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sCChar:
    case sCInt:
    case sCLbl: {
      int32 nextPc = pc + 4;
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc = nextPc;
      continue;
    }
    case sMC: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      pc = nextPc;
      continue;
    }
    case sMv: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sLG: {
      int32 nextPc = pc + 2;
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sSG: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sSav: {
      int32 nextPc = pc + 2;
      setSafePoint(analysis, nextPc);
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      pc = nextPc;
      continue;
    }
    case sLdSav: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sTstSav: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sStSav: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sCell: {
      int32 nextPc = pc + 3;
      setSafePoint(analysis, nextPc);
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sGet: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sAssign: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sNth: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sStNth: {
      int32 nextPc = pc + 4;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc = nextPc;
      continue;
    }
    case sIAdd:
    case sISub:
    case sIMul: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sIDiv:
    case sIMod: {
      int32 nextPc = pc + 5;
      recordVariableStart(analysis, operand(2), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      recordVariableUse(analysis, scope, operand(4), nextPc);
      markPhiVariable(analysis, checkScope(scope, pc + operand(1)), 0);
      pc += 5;
      continue;
    }
    case sIAbs: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sIEq:
    case sILt:
    case sIGe:
    case sCEq:
    case sCLt:
    case sCGe: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sBAnd:
    case sBOr:
    case sBXor:
    case sBLsl:
    case sBLsr:
    case sBAsr: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc = nextPc;
      continue;
    }
    case sBNot: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sFAdd:
    case sFSub:
    case sFMul: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sFDiv:
    case sFMod: {
      int32 nextPc = pc + 5;
      recordVariableStart(analysis, operand(2), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      recordVariableUse(analysis, scope, operand(4), nextPc);
      markPhiVariable(analysis, checkScope(scope, pc + operand(1)), 0);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sFAbs: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sFEq:
    case sFLt:
    case sFGe: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      pc = nextPc;
      continue;
    }
    case sAlloc: {
      int32 arity = operand(3);
      int32 nextPc = pc + arity + 4;
      recordVariableStart(analysis, operand(2), local, nextPc, nextPc);

      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis, scope, operand(ax+4), nextPc);
      setSafePoint(analysis, (arity > 2 ? pc : nextPc));
      pc = nextPc;
      continue;
    }
    case sClosure: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis, operand(2), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(3), nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sDrop:
    case sBump: {
      int32 nextPc = pc + 2;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      pc = nextPc;
      continue;
    }
    case sFiber: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis, operand(1), local, nextPc, nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sResume:
    case sSuspend:
    case sRetire: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis, scope, operand(1), nextPc);
      recordVariableUse(analysis, scope, operand(2), nextPc);
      setSafePoint(analysis, nextPc);
      pc = nextPc;
      continue;
    }
    case sUnderflow: {
      pc++;
      continue;
    }
    case sLine: {
      if (lineDebugging > generalTracing) {
        setSafePoint(analysis, pc);
      }
      pc += 2;
      continue;
    }
    case sBind: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis, scope, operand(2), nextPc);
      if (lineDebugging > generalTracing) {
        setSafePoint(analysis, pc);
      }
      pc = nextPc;
      continue;
    }
    case sdBug: {
      if (lineDebugging > noTracing) {
        setSafePoint(analysis, pc);
      }
      pc += 2;
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

static void markVarSlots(analysisPo analysis);

retCode analyseMethod(methodPo mtd, analysisPo analysis) {
  setupAnalysis(analysis);
  int32 endPc = codeSize(mtd);
  ScopeBlock block = {
    .start = 0, .end = endPc, .parent = Null, .kind = sBlock, .phiCnt = 0, .phiVars = Null
  };
  retCode ret = analyseBlock(analysis, &block, entryPoint(mtd), 0, endPc);
  markVarSlots(analysis);

#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showAnalysis(logFile, analysis);
  }
#endif
  return ret;
}

void markVarSlots(analysisPo analysis) {
  arrayPo ranges = varRanges(analysis);

  intervalSetPo allocMap = newIntervalSet();

  int32 start = 0;
  int32 tableSize = arrayCount(ranges);

  while (start < tableSize) {
    varDescPo var = *(varDescPo*)nthEntry(ranges, start);
    if (var->end < 0) // Variable never read
      var->end = var->start;

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
  return (int32)hashSize(analysis->vars);
}
