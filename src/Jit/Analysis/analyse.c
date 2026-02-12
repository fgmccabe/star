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

retCode analyseBlock(analysisPo analysis, scopePo parent, insPo code, int32 start, int32 pc, int32 limit)
{
  ScopeBlock scope = {
    .start = start, .limit = limit, .parent = parent, .stack = Null
  };

  retCode ret = Ok;
  for (; ret == Ok && pc < limit; pc++){
    insPo ins = &code[pc];

    switch (ins->op){
    case Halt:
    case Abort: {
      retireScopeStack(&scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Call: {
      labelPo fn = C_LBL(getConstant(code[pc].fst));
      int32 arity = lblArity(fn);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case XCall: {
      labelPo fn = C_LBL(getConstant(code[pc].fst));
      int32 arity = lblArity(fn);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }

    case TCall: {
      retireScopeStack(&scope, pc);
      continue;
    }
    case OCall: {
      int32 arity = code[pc].fst;
      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case XOCall: {
      int32 arity = code[pc].fst;
      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case TOCall: {
      retireScopeStack(&scope, pc);
      continue;
    }
    case Escape: {
      int32 escNo = code[pc].fst; /* escape number */
      escapePo esc = getEscape(escNo);
      int32 arity = escapeArity(esc);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case XEscape: {
      int32 escNo = code[pc].fst; /* escape number */
      escapePo esc = getEscape(escNo);
      int32 arity = escapeArity(esc);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
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
      retireScopeStack(&scope, pc);
      assert(isLastPC(&scope, pc));
      continue;
    }
    case Block: {
      int32 blockLen = code[pc].alt;

      ret = analyseBlock(analysis, &scope, code, pc, pc + 1, pc + blockLen + 1);
      pc += blockLen;
      continue;
    }
    case Valof: {
      int32 blockLen = code[pc].alt;

      ret = analyseBlock(analysis, &scope, code, pc, pc + 1, pc + blockLen + 1);
      pc += blockLen;
      newPhiVar(analysis, &scope, pc + 1);
      continue;
    }
    case Loop: {
      scopePo loopScope = checkScope(&scope, pc + code[pc].alt + 1);
      scopePo thisScope = &scope;
      while (thisScope != loopScope && thisScope != Null){
        retireScopeStack(thisScope, pc);
        thisScope = thisScope->parent;
      }
      continue;
    }
    case Break: {
      scopePo breakScope = checkScope(&scope, pc + code[pc].alt + 1)->parent;
      scopePo thisScope = &scope;
      while (thisScope != breakScope && thisScope != Null){
        retireScopeStack(thisScope, pc);
        thisScope = thisScope->parent;
      }
      continue;
    }
    case Result: {
      retireScopeStack(&scope, pc);
      continue;
    }
    case Drop:
      retireStackVar(&scope, pc);
      continue;
    case Rot: {
      int32 depth = code[pc].fst;

      if (depth > 0)
        rotateStackVars(&scope, pc, depth);
      continue;
    }
    case Rst: {
      int32 depth = code[pc].fst;
      while (stackDepth(&scope) > depth)
        retireStackVar(&scope, pc);
      continue;
    }
    case Fiber: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Resume:
    case Suspend: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Retire: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      retireScopeStack(&scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Underflow:
      return Error;
    case LdV:
    case LdC:
      newStackVar(analysis, &scope, pc);
      continue;
    case Ld:
      recordVariableUse(analysis, ins->fst, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    case St:
      retireStackVar(&scope, pc);
      recordVariableStart(analysis, ins->fst, local, pc + 1);
      continue;
    case Tee:
      retireStackVar(&scope, pc);
      recordVariableStart(analysis, code[pc].fst, local, pc + 1);
      continue;
    case StV:
      recordVariableStart(analysis, code[pc].fst, local, pc + 1);
      continue;
    case LdG:
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    case StG:
      retireStackVar(&scope, pc);
      continue;
    case TG:
      continue;
    case Sav:
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    case TstSav:
      retireStackVar(&scope, pc + 1);
      newStackVar(analysis, &scope, pc);
      continue;
    case LdSav: {
      retireStackVar(&scope, pc + 1);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case StSav: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      continue;
    }
    case TSav: {
      retireStackVar(&scope, pc);
      continue;
    }
    case Cell: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Get: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case Assign: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      continue;
    }
    case CLit: {
      retireStackVar(&scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case CInt:
    case CFlt:
    case CChar:
    case CLbl: {
      retireStackVar(&scope, pc);
      continue;
    }
    case Nth: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case StNth: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case If:
    case IfNot: {
      retireStackVar(&scope, pc);
      continue;
    }
    case Case: {
      int32 mx = code[pc].fst;
      retireStackVar(&scope, pc);
      setSafePoint(analysis, pc);
      pc += mx;
      continue;
    }
    case ICase:
    case IxCase: {
      int32 mx = code[pc].fst;
      retireStackVar(&scope, pc);

      pc += mx;
      continue;
    }
    case IAdd:
    case ISub:
    case IMul: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case IDiv:
    case IMod: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case IAbs: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
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
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case BNot: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
    }
    case FAdd:
    case FSub:
    case FMul:
    case FDiv:
    case FMod: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case FAbs: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
    }
    case FEq:
    case FLt:
    case FGe: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      continue;
    }
    case Alloc: {
      labelPo fn = C_LBL(getConstant(code[pc].fst));
      int32 arity = lblArity(fn);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Closure: {
      retireStackVar(&scope, pc);
      newStackVar(analysis, &scope, pc);
      setSafePoint(analysis, pc);
      continue;
    }
    case Frame:
      continue;
    case Line:
    case dBug:
    case Bind:
      if (lineDebugging){
        setSafePoint(analysis, pc);
      }
      continue;
    default:
      return Error;
    }
  }
  return ret;
}

scopePo checkScope(scopePo scope, int32 tgt)
{
  while (scope != Null){
    if (tgt == scope->start){
      return scope;
    }
    scope = scope->parent;
  }
  return Null;
}

logical isLastPC(scopePo scope, int32 pc)
{
  return pc + 1 == scope->limit;
}

static int32 allocateVarSlots(analysisPo analysis, methodPo mtd);

retCode analyseMethod(methodPo mtd, analysisPo results)
{
  initAnalysis();

  hashPo vars = newVarTable();
  treePo index = newVarIndex();
  setPo safes = newSet();

  results->vars = vars;
  results->index = index;
  results->safes = safes;

  for (int32 ax = 0; ax < mtdArity(mtd); ax++){
    newArgVar(vars, ax, results);
  }

  retCode ret = analyseBlock(results, Null, entryPoint(mtd), 0, 0, codeSize(mtd));
  allocateVarSlots(results, mtd);
  return ret;
}

typedef struct
{
  setPo useMap;
  int32 lastPc;
} AllocInfo;

static int32 findFreeSlot(intervalSetPo usage)
{
  int32 slotNo = findSpace(usage, 1);
  addToIntervalSet(usage, slotNo);
  return slotNo;
}

static void releaseVarSlot(intervalSetPo usage, int32 slotNo)
{
  assert(slotNo>=1);
  assert(inIntervalSet(usage,slotNo));
  removeFromIntervalSet(usage, slotNo);
}

static void retireExistingSlots(arrayPo ranges, intervalSetPo allocMap, int32 pc)
{
  for (int32 ix = 0; ix < arrayCount(ranges); ix++){
    varDescPo var = *(varDescPo*)nthEntry(ranges, ix);
    if (var->end <= pc && varState(var) == beingAllocated){
      releaseVarSlot(allocMap, -stackLoc(var));
      setState(var, allocated);
    }
  }
}

int32 allocateVarSlots(analysisPo analysis, methodPo mtd)
{
  arrayPo ranges = varRanges(analysis);

  intervalSetPo allocMap = newIntervalSet();

  int32 start = 0;
  int32 tableSize = arrayCount(ranges);

  while (start < tableSize){
    varDescPo var = *(varDescPo*)nthEntry(ranges, start);

    retireExistingSlots(ranges, allocMap, var->start);

    if (isSafe(analysis, var))
      markVarAsRegister(var);
    if (varState(var) == unAllocated){
      int32 slotNo = findFreeSlot(allocMap);
      setVarSlot(var, -slotNo);
    }
    start++;
  }

  retireExistingSlots(ranges, allocMap, codeSize(mtd));

  assert(intervalSetIsEmpty(allocMap));

  deleteIntervalSet(allocMap);
  eraseArray(ranges,Null,Null);
  return minSlot(analysis);
}

int32 slotCount(analysisPo analysis)
{
  return (int32)hashSize(analysis->vars);
}
