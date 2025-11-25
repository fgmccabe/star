//
// Created by Francis McCabe on 11/4/25.
//

#include "ssaP.h"
#include "opcodes.h"
#include "codeP.h"

#ifdef TRACEJIT
tracingLevel traceSSA = noTracing;
#endif

typedef struct block_scope_ *scopePo;

typedef struct block_scope_ {
  int32 start;
  int32 limit;
  int32 next;
  scopePo parent;
} ScopeBlock;

typedef enum {
  loopBack,
  breakOut
} breakType;

static codeSegPo checkBreak(scopePo scope, codeSegPo root, int32 tgt);
static codeSegPo checkLoop(scopePo scope, codeSegPo root, int32 tgt);

retCode splitBlock(scopePo parent, codeSegPo root, insPo code, int32 start, int32 pc, int32 limit, int32 next) {
  ScopeBlock scope = {.start = start, .limit = limit, .next = next, .parent = parent};

  retCode ret = Ok;
  for (; ret == Ok && pc < limit; pc++) {
    insPo ins = &code[pc];

    switch (ins->op) {
      case Halt:
      case Abort: {
        splitNextPC(root, pc, Null);
        continue;
      }
      case Call:
        continue;
      case XCall: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }

      case TCall: {
        splitNextPC(root, pc, Null);
        continue;
      }
      case OCall:
        continue;
      case XOCall: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case TOCall: {
        splitNextPC(root, pc, Null);
        continue;
      }
      case Escape: {
        continue;
      }
      case XEscape: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case Entry: {
        continue;
      }
      case Ret:
      case XRet: {
        splitNextPC(root, pc, Null);
        continue;
      }
      case Block:
      case Valof: {
        int32 blockLen = code[pc].alt;

        ret = splitBlock(&scope, root, code, pc, pc + 1, pc + blockLen + 1,
                         pc + blockLen + 1 < limit ? pc + blockLen + 1 : limit);
        pc += blockLen;
        continue;
      }
      case Loop: {
        codeSegPo alt = checkLoop(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case Break:
      case Result: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case Drop:
      case Dup:
      case Rot:
      case Rst:
      case Fiber:
      case Resume:
      case Suspend:
      case Retire:
        continue;
      case Underflow:
        return Error;
      case LdV:
      case LdC:
      case LdA:
      case LdL:
      case StL:
      case StV:
      case TL:
        continue;
      case LdG:
        splitNextPC(root, pc, Null);
        continue;
      case StG:
      case TG:
      case Sav:
      case TstSav:
        continue;
      case LdSav: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case StSav:
      case TSav:
      case Cell:
      case Get:
      case Assign:
        continue;
      case CLit:
      case CInt:
      case CFlt:
      case CChar:
      case CLbl: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case Nth:
      case StNth:
        continue;

      case If:
      case IfNot: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case ICase:
      case Case:
      case IxCase: {
        splitNextPC(root, pc, Null);
        int32 mx = code[pc].fst;
        for (int32 ix = 0; ix < mx; ix++) {
          int32 casePc = pc + 1 + ix;
          insPo caseIns = &code[casePc];
          switch (caseIns->op) {
            case Break: {
              codeSegPo alt = checkBreak(&scope, root, casePc + code[casePc].alt + 1);
              splitNextPC(root, casePc, alt);
              continue;
            }
            case Loop: {
              codeSegPo alt = checkLoop(&scope, root, casePc + code[casePc].alt + 1);
              splitNextPC(root, casePc, alt);
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
      case IMul:
        continue;
      case IDiv:
      case IMod: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case IAbs:
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
      case BAsr:
      case BNot:
      case FAdd:
      case FSub:
      case FMul:
        continue;
      case FDiv:
      case FMod: {
        codeSegPo alt = checkBreak(&scope, root, pc + code[pc].alt + 1);
        splitNextPC(root, pc, alt);
        continue;
      }
      case FAbs:
      case FEq:
      case FLt:
      case FGe:
      case Alloc:
      case Closure:
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

codeSegPo checkBreak(scopePo scope, codeSegPo root, int32 tgt) {
  while (scope != Null) {
    if (tgt == scope->start) {
      return splitAtPC(root, scope->limit);
    }
    scope = scope->parent;
  }
  return Null;
}

codeSegPo checkLoop(scopePo scope, codeSegPo root, int32 tgt) {
  while (scope != Null) {
    if (tgt == scope->start) {
      return splitAtPC(root, tgt);
    }
    scope = scope->parent;
  }
  return Null;
}

codeSegPo segmentMethod(methodPo mtd) {
  codeSegPo root = newCodeSeg(0, codeSize(mtd),Null);
  if (splitBlock(Null, root, entryPoint(mtd), 0, 0, codeSize(mtd), -1) == Ok)
    return root;
  logMsg(logFile, "Could not segment code for %M", mtd);
  return Null;
}
