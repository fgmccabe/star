//
// Created by Francis McCabe on 11/4/25.
//

#include "code.h"
#include "analyse.h"
#include "analyseP.h"
#include "opcodes.h"
#include "codeP.h"
#include "array.h"
#include "hash.h"
#include "set.h"
#include "constants.h"

#ifdef TRACEJIT
tracingLevel traceSSA = noTracing;
#endif

logical enableSSA = False;

typedef enum
{
  loopBack,
  breakOut
} breakType;

static codeSegPo checkBreak(scopePo scope, codeSegPo root, int32 pc, int32 tgt);
static codeSegPo checkLoop(scopePo scope, codeSegPo root, int32 pc, int32 tgt);
static logical isLastPC(scopePo scope, int32 pc);

retCode splitBlock(scopePo parent, codeSegPo root, hashPo vars, setPo safes, insPo code, int32 start, int32 pc,
                   int32 limit)
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
      splitNextPC(root, pc, Null);
      retireScopeStack(&scope, pc);
      continue;
    }
    case Call: {
      labelPo fn = C_LBL(getConstant(code[pc].fst));
      int32 arity = lblArity(fn);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case XCall: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);

      labelPo fn = C_LBL(getConstant(code[pc].fst));
      int32 arity = lblArity(fn);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }

    case TCall: {
      splitNextPC(root, pc, Null);
      retireScopeStack(&scope, pc);
      continue;
    }
    case OCall: {
      int32 arity = code[pc].fst;
      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case XOCall: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      int32 arity = code[pc].fst;
      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case TOCall: {
      splitNextPC(root, pc, Null);
      retireScopeStack(&scope, pc);
      continue;
    }
    case Escape: {
      int32 escNo = code[pc].fst; /* escape number */
      escapePo esc = getEscape(escNo);
      int32 arity = escapeArity(esc);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case XEscape: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);

      int32 escNo = code[pc].fst; /* escape number */
      escapePo esc = getEscape(escNo);
      int32 arity = escapeArity(esc);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case Entry: {
      int32 count = code[pc].fst;
      for (int32 lx = 0; lx < count; lx++)
        newLocalVar(vars, -(lx + 1));
      continue;
    }
    case Ret:
    case XRet: {
      splitNextPC(root, pc, Null);
      retireScopeStack(&scope, pc);
      assert(isLastPC(&scope, pc));
      continue;
    }
    case Block:
    case Valof: {
      int32 blockLen = code[pc].alt;

      ret = splitBlock(&scope, root, vars, safes, code, pc, pc + 1, pc + blockLen + 1);
      pc += blockLen;
      continue;
    }
    case Loop: {
      codeSegPo alt = checkLoop(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      continue;
    }
    case Break: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      continue;
    }
    case Result: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      retireScopeStack(&scope, pc);
      continue;
    }
    case Drop:
      retireStackVar(&scope, pc);
      continue;
    case Dup:
      newStackVar(&scope, vars, pc);
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
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case Resume:
    case Suspend: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case Retire: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      retireScopeStack(&scope, pc);
      addToSet(safes, pc);
      continue;
    }
    case Underflow:
      return Error;
    case LdV:
    case LdC:
      newStackVar(&scope, vars, pc);
      continue;
    case LdA:
      recordVariableUse(vars, ins->fst, pc);
      newStackVar(&scope, vars, pc);
      continue;
    case LdL:
      recordVariableUse(vars, -ins->fst, pc);
      newStackVar(&scope, vars, pc);
      continue;
    case StL:
      retireStackVar(&scope, pc);
      recordVariableStart(vars, -ins->fst, local, pc);
      continue;
    case TL:
      retireStackVar(&scope, pc);
      recordVariableStart(vars, -code[pc].fst, local, pc + 1);
      continue;
    case StV:
      recordVariableStart(vars, -code[pc].fst, local, pc + 1);
      continue;
    case LdG:
      splitNextPC(root, pc, Null);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    case StG:
      retireStackVar(&scope, pc);
      continue;
    case TG:
      continue;
    case Sav:
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    case TstSav:
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    case LdSav: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
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
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case Get: {
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case Assign: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      continue;
    }
    case CLit:
    case CInt:
    case CFlt:
    case CChar:
    case CLbl: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      retireStackVar(&scope, pc);
      continue;
    }
    case Nth: {
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case StNth: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case If:
    case IfNot: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      retireStackVar(&scope, pc);
      continue;
    }
    case ICase:
    case Case:
    case IxCase: {
      int32 mx = code[pc].fst;
      splitAtPC(root, pc + 1 + mx);
      codeSegPo curr = findSeg(root, pc);
      curr->fallthrough = Null;
      retireStackVar(&scope, pc);

      for (int32 ix = 0; ix < mx; ix++){
        int32 casePc = pc + 1 + ix;
        insPo caseIns = &code[casePc];
        switch (caseIns->op){
        case Break: {
          codeSegPo alt = checkBreak(&scope, root, pc, casePc + code[casePc].alt + 1);
          newOutgoing(root, casePc, alt);
          continue;
        }
        case Loop: {
          codeSegPo alt = checkLoop(&scope, root, pc, casePc + code[casePc].alt + 1);
          newOutgoing(root, casePc, alt);
          continue;
        }
        default:
          return Error;
        }
      }
      pc += mx;
      continue;
    }
    case IAdd:
    case ISub:
    case IMul: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case IDiv:
    case IMod: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case IAbs: {
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
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
      newStackVar(&scope, vars, pc);
      continue;
    }
    case BNot: {
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
    }
    case FAdd:
    case FSub:
    case FMul: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case FDiv:
    case FMod: {
      codeSegPo alt = checkBreak(&scope, root, pc, pc + code[pc].alt + 1);
      splitNextPC(root, pc, alt);
      continue;
    }
    case FAbs: {
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
    }
    case FEq:
    case FLt:
    case FGe: {
      retireStackVar(&scope, pc);
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      continue;
    }
    case Alloc: {
      labelPo fn = C_LBL(getConstant(code[pc].fst));
      int32 arity = lblArity(fn);

      for (int32 ax = 0; ax < arity; ax++)
        retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case Closure: {
      retireStackVar(&scope, pc);
      newStackVar(&scope, vars, pc);
      addToSet(safes, pc);
      continue;
    }
    case Frame:
      continue;
    case Line:
    case dBug:
      splitNextPC(root, pc, Null);
      continue;
    default:
      return Error;
    }
  }
  return ret;
}

codeSegPo checkBreak(scopePo scope, codeSegPo root, int32 pc, int32 tgt)
{
  while (scope != Null){
    if (tgt == scope->start){
      codeSegPo tgtSeg = splitAtPC(root, scope->limit);
      codeSegPo seg = findSeg(root, pc);
      linkIncoming(tgtSeg, seg);
      return tgtSeg;
    }
    scope = scope->parent;
  }
  return Null;
}

codeSegPo checkLoop(scopePo scope, codeSegPo root, int32 pc, int32 tgt)
{
  while (scope != Null){
    if (tgt == scope->start){
      codeSegPo tgtSeg = splitAtPC(root, tgt);
      codeSegPo seg = findSeg(root, pc);
      linkIncoming(tgtSeg, seg);
      return tgtSeg;
    }
    scope = scope->parent;
  }
  return Null;
}

logical isLastPC(scopePo scope, int32 pc)
{
  return pc + 1 == scope->limit;
}

AnalysisRecord* analyseMethod(methodPo mtd, AnalysisRecord* results)
{
  codeSegPo root = newCodeSeg(0, codeSize(mtd),Null);
  hashPo vars = newVarTable();
  setPo safes = newSet();

  for (int32 ax = 0; ax < mtdArity(mtd); ax++){
    newArgVar(vars, ax);
  }

  if (splitBlock(Null, root, vars, safes, entryPoint(mtd), 0, 0, codeSize(mtd)) == Ok){
    showSegmented(logFile, mtd, root, vars);
    showSet(logFile, safes);
    results->segments = root;
    results->vars = vars;
    results->safes = safes;
    return results;
  }
  logMsg(logFile, "Could not segment code for %M", mtd);
  return Null;
}
