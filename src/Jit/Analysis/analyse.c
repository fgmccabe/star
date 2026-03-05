//
// Created by Francis McCabe on 11/4/25.
//

#include "code.h"
#include "analyse.h"
#include "analyseP.h"
#include "debugP.h"
#include "opcodes.h"
#include "codeP.h"
#include "array.h"
#include "hash.h"
#include "set.h"
#include "constants.h"
#include "intervalSet.h"

#ifdef TRACEJIT
tracingLevel traceSSA = noTracing;
#endif

logical enableSSA = False;

static logical isLastPC(scopePo scope, int32 pc);
scopePo checkScope(scopePo scope, int32 tgt);

retCode analyseBlock(analysisPo analysis, scopePo parent, insPo code, int32 start, int32 pc, int32 limit,
                     varStackPo vStk) {
  ScopeBlock scope = {
    .start = start, .limit = limit, .parent = parent, .stack = vStk
  };

  retCode ret = Ok;

  for (; ret == Ok && pc < limit; pc++) {
    insPo ins = &code[pc];

    switch (ins->op) {
      case Halt:
      case Abort: {
        scope.stack = retireScopeStack(scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Call: {
        labelPo fn = C_LBL(getConstant(code[pc].fst));
        int32 arity = lblArity(fn);

        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case XCall: {
        labelPo fn = C_LBL(getConstant(code[pc].fst));
        int32 arity = lblArity(fn);

        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }

      case TCall: {
        scope.stack = retireScopeStack(scope.stack, pc);
        continue;
      }
      case OCall: {
        int32 arity = code[pc].fst;
        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case XOCall: {
        int32 arity = code[pc].fst;
        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case TOCall: {
        scope.stack = retireScopeStack(scope.stack, pc);
        continue;
      }
      case Escape: {
        int32 escNo = code[pc].fst; /* escape number */
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case XEscape: {
        int32 escNo = code[pc].fst; /* escape number */
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Entry: {
        int32 count = code[pc].fst;
        for (int32 lx = 0; lx < count; lx++)
          newLocalVar(analysis, -(lx + 1));
        continue;
      }
      case Ret:
      case XRet: {
        scope.stack = retireScopeStack(scope.stack, pc);
        assert(isLastPC(&scope, pc));
        continue;
      }
      case Block: {
        int32 blockLen = code[pc].alt;

        ret = analyseBlock(analysis, &scope, code, pc, pc + 1, pc + blockLen + 1, Null);
        pc += blockLen;
        continue;
      }
      case Valof: {
        int32 blockLen = code[pc].alt;

        ret = analyseBlock(analysis, &scope, code, pc, pc + 1, pc + blockLen + 1, Null);
        pc += blockLen;
        scope.stack = newPhiVar(analysis, scope.stack, pc + 1);
        continue;
      }
      case Loop: {
        scopePo loopScope = checkScope(&scope, pc + code[pc].alt + 1);
        scope.stack = retireScopeStack(scope.stack, pc);
        scopePo thisScope = &scope;
        while (thisScope != loopScope && thisScope != Null) {
          thisScope->stack = retireScopeStack(thisScope->stack, pc);
          thisScope = thisScope->parent;
        }
        continue;
      }
      case Break: {
        scopePo breakScope = checkScope(&scope, pc + code[pc].alt + 1)->parent;
        scopePo thisScope = &scope;
        while (thisScope != breakScope && thisScope != Null) {
          thisScope->stack = retireScopeStack(thisScope->stack, pc);
          thisScope = thisScope->parent;
        }
        continue;
      }
      case Result: {
        scope.stack = retireScopeStack(scope.stack, pc);
        continue;
      }
      case Drop:
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      case Rst: {
        int32 depth = code[pc].fst;
        while (stackDepth(&scope) > depth)
          scope.stack = retireStackVar(scope.stack, pc);
        continue;
      }
      case Fiber: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Resume:
      case Suspend: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Retire: {
        scope.stack = retireScopeStack(scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Underflow: {
        scope.stack = retireStackVar(scope.stack, pc);
        assert(scope.stack==Null);
        break;
      }
      case LdV:
      case LdC:
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      case Ld:
        recordVariableUse(analysis, ins->fst, pc + 1);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      case St:
        scope.stack = retireStackVar(scope.stack, pc);
        recordVariableStart(analysis, ins->fst, local, pc);
        continue;
      case Tee:
        scope.stack = retireStackVar(scope.stack, pc);
        recordVariableStart(analysis, code[pc].fst, local, pc);
        continue;
      case StV:
        recordVariableStart(analysis, code[pc].fst, local, pc);
        continue;
      case LdG:
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      case StG:
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      case TG:
        continue;
      case Sav:
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      case TstSav:
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      case LdSav: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case StSav: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      }
      case TSav: {
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      }
      case Cell: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Get: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case Assign: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      }
      case CLit: {
        scope.stack = retireStackVar(scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case CInt:
      case CFlt:
      case CChar:
      case CLbl: {
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      }
      case Nth: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case StNth: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case If:
      case IfNot: {
        scope.stack = retireStackVar(scope.stack, pc);
        continue;
      }
      case Case: {
        int32 mx = code[pc].fst;
        scope.stack = retireStackVar(scope.stack, pc);
        setSafePoint(analysis, pc);
        pc += mx;
        continue;
      }
      case ICase:
      case IxCase: {
        int32 mx = code[pc].fst;
        scope.stack = retireStackVar(scope.stack, pc);

        pc += mx;
        continue;
      }
      case IAdd:
      case ISub:
      case IMul: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case IDiv:
      case IMod: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case IAbs: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
      }
      case IEq:
      case ILt:
      case IGe:
      case CEq:
      case CLt:
      case CGe:
      case BAnd:
      case BOr:
      case BXor:
      case BLsl:
      case BLsr:
      case BAsr: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case BNot: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
      }
      case FAdd:
      case FSub:
      case FMul:
      case FDiv:
      case FMod: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case FAbs: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
      }
      case FEq:
      case FLt:
      case FGe: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        continue;
      }
      case Alloc: {
        labelPo fn = C_LBL(getConstant(code[pc].fst));
        int32 arity = lblArity(fn);

        for (int32 ax = 0; ax < arity; ax++)
          scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Closure: {
        scope.stack = retireStackVar(scope.stack, pc);
        scope.stack = newStackVar(analysis, scope.stack, pc);
        setSafePoint(analysis, pc);
        continue;
      }
      case Frame:
        continue;
      case Line:
      case dBug:
      case Bind:
        if (lineDebugging) {
          setSafePoint(analysis, pc);
        }
        continue;
      default:
        return Error;
    }
  }
  scope.stack = retireScopeStack(scope.stack, pc);
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

logical isLastPC(scopePo scope, int32 pc) {
  return pc + 1 == scope->limit;
}

static void markVarSlots(analysisPo analysis, methodPo mtd);

retCode analyseMethod(methodPo mtd, analysisPo results) {
  return analyseSpecialMethod(mtd, 0, results);
}

retCode analyseSpecialMethod(methodPo mtd, int32 depth, analysisPo results) {
  initAnalysis();

  hashPo vars = newVarTable();
  treePo index = newVarIndex();
  setPo safes = newSet();

  results->vars = vars;
  results->index = index;
  results->safes = safes;

  for (int32 ax = 0; ax < mtdArity(mtd); ax++) {
    newArgVar(vars, ax, results);
  }

  varStackPo specialStack = Null;

  for (int32 ix = 0; ix < depth; ix++) {
    specialStack = newStackVar(results, specialStack, 0);
  }

  retCode ret = analyseBlock(results, Null, entryPoint(mtd), 0, 0, codeSize(mtd), specialStack);
  markVarSlots(results, mtd);
  return ret;
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
