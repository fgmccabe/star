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

static codeSegPo checkBreak(analysisPo analysis, scopePo scope, int32 pc, int32 tgt);
static codeSegPo checkLoop(analysisPo analysis, scopePo scope, int32 pc, int32 tgt);
static logical isLastPC(scopePo scope, int32 pc);

retCode splitBlock(analysisPo analysis, scopePo parent, insPo code, int32 start, int32 pc, int32 limit,
                   varDescPo phiVar) {
  ScopeBlock scope = {
    .start = start, .limit = limit, .parent = parent, .stack = Null, .phiVar = phiVar
  };

  retCode ret = Ok;
  for (; ret == Ok && pc < limit; pc++) {
    insPo ins = &code[pc];

    switch (ins->op) {
      case Halt:
      case Abort: {
        splitNextPC(analysis, pc, Null);
        retireScopeStack(&scope, pc);
        continue;
      }
      case Call: {
        labelPo fn = C_LBL(getConstant(code[pc].fst));
        int32 arity = lblArity(fn);

        for (int32 ax = 0; ax < arity; ax++)
          retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }
      case XCall: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);

        labelPo fn = C_LBL(getConstant(code[pc].fst));
        int32 arity = lblArity(fn);

        for (int32 ax = 0; ax < arity; ax++)
          retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }

      case TCall: {
        splitNextPC(analysis, pc, Null);
        retireScopeStack(&scope, pc);
        continue;
      }
      case OCall: {
        int32 arity = code[pc].fst;
        for (int32 ax = 0; ax < arity; ax++)
          retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }
      case XOCall: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
        int32 arity = code[pc].fst;
        for (int32 ax = 0; ax < arity; ax++)
          retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }
      case TOCall: {
        splitNextPC(analysis, pc, Null);
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
        addToSet(analysis->safes, pc);
        continue;
      }
      case XEscape: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);

        int32 escNo = code[pc].fst; /* escape number */
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        for (int32 ax = 0; ax < arity; ax++)
          retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
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
        splitNextPC(analysis, pc, Null);
        retireScopeStack(&scope, pc);
        assert(isLastPC(&scope, pc));
        continue;
      }
      case Block: {
        int32 blockLen = code[pc].alt;

        ret = splitBlock(analysis, &scope, code, pc, pc + 1, pc + blockLen + 1, Null);
        pc += blockLen;
        continue;
      }
      case Valof: {
        int32 blockLen = code[pc].alt;
        varDescPo phi = newPhiVar(analysis, &scope, pc);

        ret = splitBlock(analysis, &scope, code, pc, pc + 1, pc + blockLen + 1, phi);
        pc += blockLen;
        continue;
      }
      case Loop: {
        codeSegPo alt = checkLoop(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
        continue;
      }
      case Break: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
        continue;
      }
      case Result: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
        retireStackVarToPhi(&scope, pc, pc + code[pc].alt + 1);
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
        addToSet(analysis->safes, pc);
        continue;
      }
      case Resume:
      case Suspend: {
        retireStackVar(&scope, pc);
        retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }
      case Retire: {
        retireStackVar(&scope, pc);
        retireStackVar(&scope, pc);
        retireScopeStack(&scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }
      case Underflow:
        return Error;
      case LdV:
      case LdC:
        newStackVar(analysis, &scope, pc);
        continue;
      case LdA:
        recordVariableUse(analysis, ins->fst, pc);
        newStackVar(analysis, &scope, pc);
        continue;
      case LdL:
        recordVariableUse(analysis, -ins->fst, pc);
        newStackVar(analysis, &scope, pc);
        continue;
      case StL:
        retireStackVar(&scope, pc);
        recordVariableStart(analysis, -ins->fst, local, pc);
        continue;
      case TL:
        retireStackVar(&scope, pc);
        recordVariableStart(analysis, -code[pc].fst, local, pc + 1);
        continue;
      case StV:
        recordVariableStart(analysis, -code[pc].fst, local, pc + 1);
        continue;
      case LdG:
        splitNextPC(analysis, pc, Null);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      case StG:
        retireStackVar(&scope, pc);
        continue;
      case TG:
        continue;
      case Sav:
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      case TstSav:
        retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        continue;
      case LdSav: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
        retireStackVar(&scope, pc);
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
        addToSet(analysis->safes, pc);
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
      case CLit:
      case CInt:
      case CFlt:
      case CChar:
      case CLbl: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
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
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
        retireStackVar(&scope, pc);
        continue;
      }
      case ICase:
      case Case:
      case IxCase: {
        int32 mx = code[pc].fst;
        splitAtPC(analysis->segments, pc + 1 + mx);
        codeSegPo curr = findSeg(analysis->segments, pc);
        curr->fallthrough = Null;
        retireStackVar(&scope, pc);

        for (int32 ix = 0; ix < mx; ix++) {
          int32 casePc = pc + 1 + ix;
          insPo caseIns = &code[casePc];
          switch (caseIns->op) {
            case Break: {
              codeSegPo alt = checkBreak(analysis, &scope, pc, casePc + code[casePc].alt + 1);
              newOutgoing(analysis->segments, casePc, alt);
              continue;
            }
            case Loop: {
              codeSegPo alt = checkLoop(analysis, &scope, pc, casePc + code[casePc].alt + 1);
              newOutgoing(analysis->segments, casePc, alt);
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
        newStackVar(analysis, &scope, pc);
        continue;
      }
      case IDiv:
      case IMod: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
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
      case FMul: {
        retireStackVar(&scope, pc);
        retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        continue;
      }
      case FDiv:
      case FMod: {
        codeSegPo alt = checkBreak(analysis, &scope, pc, pc + code[pc].alt + 1);
        splitNextPC(analysis, pc, alt);
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
        addToSet(analysis->safes, pc);
        continue;
      }
      case Closure: {
        retireStackVar(&scope, pc);
        newStackVar(analysis, &scope, pc);
        addToSet(analysis->safes, pc);
        continue;
      }
      case Frame:
        continue;
      case Line:
      case dBug:
        splitNextPC(analysis, pc, Null);
        continue;
      default:
        return Error;
    }
  }
  return ret;
}

codeSegPo checkBreak(analysisPo analysis, scopePo scope, int32 pc, int32 tgt) {
  while (scope != Null) {
    if (tgt == scope->start) {
      codeSegPo tgtSeg = splitAtPC(analysis->segments, scope->limit);
      codeSegPo seg = findSeg(analysis->segments, pc);
      linkIncoming(tgtSeg, seg);
      return tgtSeg;
    }
    scope = scope->parent;
  }
  return Null;
}

codeSegPo checkLoop(analysisPo analysis, scopePo scope, int32 pc, int32 tgt) {
  while (scope != Null) {
    if (tgt == scope->start) {
      codeSegPo tgtSeg = splitAtPC(analysis->segments, tgt);
      codeSegPo seg = findSeg(analysis->segments, pc);
      linkIncoming(tgtSeg, seg);
      return tgtSeg;
    }
    scope = scope->parent;
  }
  return Null;
}

logical isLastPC(scopePo scope, int32 pc) {
  return pc + 1 == scope->limit;
}

analysisPo analyseMethod(methodPo mtd, analysisPo results) {
  codeSegPo root = newCodeSeg(0, codeSize(mtd),Null);
  hashPo vars = newVarTable();
  hashPo index = newVarIndex();
  setPo safes = newSet();

  results->segments = root;
  results->vars = vars;
  results->index = index;
  results->safes = safes;

  for (int32 ax = 0; ax < mtdArity(mtd); ax++) {
    newArgVar(vars, ax, results);
  }

  if (splitBlock(results, Null, entryPoint(mtd), 0, 0, codeSize(mtd), Null) == Ok) {
#ifdef TRACEJIT
    if (traceJit) {
      checkIndex(logFile, index);
      showSegmented(logFile, mtd, root);
      outMsg(logFile,"Safe points: ");
      showSet(logFile, safes);
      outMsg(logFile,"\n%_");
    }
#endif

    return results;
  }
  logMsg(logFile, "Could not segment code for %M", mtd);
  return Null;
}
