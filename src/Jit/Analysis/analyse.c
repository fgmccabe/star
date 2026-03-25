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

  for (; ret == Ok && pc < limit;){
    switch (code[pc].op.op){
    case sHalt: {
      int32 nextPc = pc + 2;
      recordVariableUse(analysis,operand(1), nextPc);
      pc = nextPc;
      continue;
    }
    case sAbort: {
      int32 nextPc = pc + 2;
      recordVariableUse(analysis,operand(2), nextPc);
      setSafePoint(analysis, pc);
      pc = nextPc;
      continue;
    }
    case sCall:
    case sEscape: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;

      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis,operand(3+ax), nextPc);

      setSafePoint(analysis, pc);
      pc = nextPc;
      continue;
    }
    case sOCall: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;
      recordVariableUse(analysis,operand(1), nextPc);
      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis,operand(3+ax), nextPc);

      setSafePoint(analysis, pc);
      pc = nextPc;
      continue;
    }
    case sTCall: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;
      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis,operand(3+ax), nextPc);
      pc = nextPc;
      continue;
    }
    case sTOCall: {
      int32 arity = operand(2);
      int32 nextPc = pc + arity + 3;
      recordVariableUse(analysis,operand(1), nextPc);
      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis,operand(3+ax), nextPc);
      pc = nextPc;
      continue;
    }
    case sRSP: {
      recordVariableStart(analysis,operand(1), local, pc);
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
      int32 nextPc = pc + 2;
      recordVariableUse(analysis,operand(1), nextPc);
      pc = nextPc;
      continue;
    }
    case sRtn: {
      pc++;
      continue;
    }
    case sBlock: {
      int32 skipLen = operand(1);
      ret = analyseBlock(analysis, &scope, code, pc, pc + 2, pc + skipLen);
      pc += skipLen;
      continue;
    }
    case sValof: {
      int32 skipLen = operand(2);
      recordVariableStart(analysis,operand(1), phi, pc);
      ret = analyseBlock(analysis, &scope, code, pc, pc + 3, pc + skipLen);
      pc += skipLen;
      continue;
    }
    case sBreak:
    case sLoop: {
      pc += 2;
      continue;
    }
    case sResult: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis,operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sIf:
    case sIfNot: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis,operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sCase:
    case sICase: {
      int32 caseLimit = operand(2);
      int32 nextPc = pc + caseLimit + 2;
      recordVariableUse(analysis,operand(1), nextPc);
      analyseBlock(analysis, &scope, code, pc, pc + 2, pc + caseLimit + 2);
      pc = nextPc;
      continue;
    }
    case sIxCase: {
      int32 caseLimit = operand(2);
      int32 nextPc = pc + caseLimit + 2;
      recordVariableUse(analysis,operand(1), nextPc);
      analyseBlock(analysis, &scope, code, pc, pc + 2, pc + caseLimit + 2);
      setSafePoint(analysis, pc);
      pc = nextPc;
      continue;
    }
    case sCLit:
    case sCFlt: {
      int32 nextPc = pc + 4;
      recordVariableUse(analysis,operand(3), nextPc);
      setSafePoint(analysis, pc);
      pc = nextPc;
      continue;
    }
    case sCChar:
    case sCInt:
    case sCLbl: {
      int32 nextPc = pc + 4;
      recordVariableUse(analysis,operand(3), nextPc);
      pc = nextPc;
      continue;
    }
    case sMC: {
      recordVariableStart(analysis,operand(1), local, pc);
      pc += 3;
      continue;
    }
    case sMv: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sLG: {
      setSafePoint(analysis, pc);
      pc += 2;
      continue;
    }
    case sSG: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis,operand(2), nextPc);
      pc = nextPc;
      continue;
    }
    case sSav: {
      setSafePoint(analysis, pc);
      recordVariableStart(analysis,operand(1), local, pc);
      pc += 2;
      continue;
    }
    case sLdSav: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sTstSav: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sStSav: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sCell: {
      int32 nextPc = pc + 3;
      setSafePoint(analysis, pc);
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sGet: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sAssign: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis,operand(1), nextPc);
      recordVariableUse(analysis,operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sNth: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sStNth: {
      int32 nextPc = pc + 4;
      recordVariableUse(analysis,operand(1), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sIAdd:
    case sISub:
    case sIMul: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sIDiv:
    case sIMod: {
      int32 nextPc = pc + 5;
      recordVariableStart(analysis,operand(2), local, pc);
      recordVariableUse(analysis,operand(3), nextPc);
      recordVariableUse(analysis,operand(4), nextPc);
      pc += 5;
      continue;
    }
    case sIAbs: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
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
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
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
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sBNot: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      pc += 3;
      continue;
    }
    case sFAdd:
    case sFSub:
    case sFMul: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
      setSafePoint(analysis, pc);
      pc += 4;
    }
    case sFDiv:
    case sFMod: {
      int32 nextPc = pc + 5;
      recordVariableStart(analysis,operand(2), local, pc);
      recordVariableUse(analysis,operand(3), nextPc);
      recordVariableUse(analysis,operand(4), nextPc);
      setSafePoint(analysis, pc);
      pc += 5;
      continue;
    }
    case sFAbs: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      setSafePoint(analysis, pc);
      pc += 3;
      continue;
    }
    case sFEq:
    case sFLt:
    case sFGe: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      recordVariableUse(analysis,operand(3), nextPc);
      pc += 4;
      continue;
    }
    case sAlloc: {
      recordVariableStart(analysis,operand(2), local, pc);
      int32 arity = operand(3);
      int32 nextPc = pc + arity + 4;

      for (int32 ax = 0; ax < arity; ax++)
        recordVariableUse(analysis,operand(ax+3), nextPc);
      setSafePoint(analysis, pc);
      pc += arity + 4;
      continue;
    }
    case sClosure: {
      int32 nextPc = pc + 4;
      recordVariableStart(analysis,operand(2), local, pc);
      recordVariableUse(analysis,operand(3), nextPc);
      setSafePoint(analysis, pc);
      pc += 4;
      continue;
    }
    case sDrop:
    case sBump: {
      int32 nextPc = pc + 2;
      recordVariableUse(analysis,operand(1), nextPc);
      pc += 2;
      continue;
    }
    case sFiber: {
      int32 nextPc = pc + 3;
      recordVariableStart(analysis,operand(1), local, pc);
      recordVariableUse(analysis,operand(2), nextPc);
      setSafePoint(analysis, pc);
      pc += 3;
      continue;
    }
    case sResume:
    case sSuspend:
    case sRetire: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis,operand(1), nextPc);
      recordVariableUse(analysis,operand(2), nextPc);
      setSafePoint(analysis, pc);
      pc += 3;
      continue;
    }
    case sUnderflow: {
      pc++;
      continue;
    }
    case sLine: {
      if (lineDebugging){
        setSafePoint(analysis, pc);
      }
      pc += 2;
      continue;
    }
    case sBind: {
      int32 nextPc = pc + 3;
      recordVariableUse(analysis,operand(2), nextPc);
      if (lineDebugging){
        setSafePoint(analysis, pc);
      }
      pc += 3;
      continue;
    }
    case sdBug: {
      if (lineDebugging){
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
  while (scope != Null){
    if (tgt == scope->start){
      return scope;
    }
    scope = scope->parent;
  }
  return Null;
}

static void markVarSlots(analysisPo analysis);

retCode analyseMethod(methodPo mtd, analysisPo results) {
  setupAnalysis(results);
  int32 endPc = codeSize(mtd);
  retCode ret = analyseBlock(results,Null, entryPoint(mtd), 0, 0, endPc);
  markVarSlots(results);
  return ret;
}

void markVarSlots(analysisPo analysis) {
  arrayPo ranges = varRanges(analysis);

  intervalSetPo allocMap = newIntervalSet();

  int32 start = 0;
  int32 tableSize = arrayCount(ranges);

  while (start < tableSize){
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
