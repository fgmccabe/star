//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>
#include "constants.h"
#include "debugP.h"
#include "verifyP.h"
#include "ltype.h"
#include "arith.h"
#include "libEscapes.h"
#include "stackP.h"
#include "char.h"

logical enableVerify = True;         // True if we verify code as it is loaded
tracingLevel traceVerify = noTracing;      // Set if tracing code verification

typedef struct {
  logical inited;    //  True if cell has real value
  logical read;      //  Has this cell been read?
} Var, *varPo;

typedef struct verify_context_ *verifyCtxPo;
typedef struct verify_context_ {
  char *prefix;
  methodPo mtd;
  int32 from;
  int32 limit;
  int32 exitDepth;
  verifyCtxPo propagated;    // Where have we propagated to?
  logical tryBlock;      // Is this from a Try block?
  varPo locals;
  int32 lclCount;
  verifyCtxPo parent;
  char *errorMsg;
  long msgLen;
} VerifyContext;

static retCode verifyError(verifyCtxPo ctx, char *msg, ...);
static retCode verifyBlock(int32 from, int32 pc, int32 limit, logical tryBlock, verifyCtxPo parentCtx,
                           int32 currDepth, int32 exitDepth);

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
  if (traceVerify > noTracing)
    showMethodCode(logFile, "Verify method %L\n", mtd);

  int32 lclCnt = lclCount(mtd);
  Var locals[lclCnt];

  for (integer ix = 0; ix < lclCnt; ix++) {
    locals[ix].inited = False;
    locals[ix].read = False;
  }

  VerifyContext mtdCtx = {.prefix = "", .errorMsg=errorMsg, .msgLen=msgLen,
    .mtd=mtd, .from = 0, .limit=codeSize(mtd),
    .parent=Null,
    .exitDepth=1,
    .locals=locals,
    .lclCount=lclCnt,
    .propagated=Null,
    .tryBlock=False};

  return verifyBlock(0, 0, codeSize(mtd), False, &mtdCtx, 0, 1);
}

static void propagateVars(verifyCtxPo ctx, verifyCtxPo tgtCtx) {
  for (integer ix = 0; ix < ctx->lclCount; ix++) {
    if (!(ctx->propagated == tgtCtx))
      tgtCtx->locals[ix].inited |= ctx->locals[ix].inited;
    tgtCtx->locals[ix].read |= ctx->locals[ix].read;
  }
  ctx->propagated = tgtCtx;
}

static retCode checkBreak(verifyCtxPo ctx, int32 pc, int32 tgt, int32 margin) {
  verifyCtxPo tgtCtx = ctx;

  while (tgtCtx != Null && tgtCtx->from != tgt) {
    tgtCtx = tgtCtx->parent;
  }

  if (tgtCtx != Null) {
    if (tgtCtx->exitDepth < margin)
      return verifyError(ctx, ".%d: block does not have sufficient margin %d", pc, margin);
  } else
    return verifyError(ctx, ".%d: break target not found", pc);

  if (tgtCtx->parent != Null)
    propagateVars(ctx, tgtCtx->parent);
  return Ok;
}

static logical isLastPC(int32 pc, int32 limit) {
  return pc >= limit - 1;
}

retCode verifyBlock(int32 from, int32 pc, int32 limit, logical tryBlock, verifyCtxPo parentCtx,
                    int32 currDepth, int32 exitDepth) {
  int32 stackDepth = currDepth;
  insPo code = parentCtx->mtd->instructions;

  char prefix[MAXLINE];
  strMsg(prefix, NumberOf(prefix), "%s.%d", parentCtx->prefix, from);

  int32 lclCnt = parentCtx->lclCount;
  Var locals[lclCnt];
  for (integer ix = 0; ix < lclCnt; ix++)
    locals[ix] = parentCtx->locals[ix];

  VerifyContext ctx = {.prefix=prefix,
    .errorMsg=parentCtx->errorMsg, .msgLen=parentCtx->msgLen,
    .mtd = parentCtx->mtd,
    .from = from,
    .limit = limit,
    .parent = parentCtx,
    .exitDepth = exitDepth,
    .locals = locals,
    .lclCount = lclCnt,
    .propagated=Null,
    .tryBlock=tryBlock};

  while (pc < limit) {
    insPo ins = &code[pc];
    if (traceVerify > generalTracing) {
      disass(logFile, Null, ctx.mtd, ins);
      outMsg(logFile, "\n%_");
    }

    switch (ins->op) {
      case Halt: {
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Halt should be last instruction in block", pc);
        else {
          propagateVars(&ctx, parentCtx);
          return Ok;
        }
      }
      case Nop:
        pc++;
        break;
      case Abort: {
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Abort should be last instruction in block", pc);
        else {
          if (stackDepth < 2)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          propagateVars(&ctx, parentCtx);
          return Ok;
        }
      }
      case Call: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (isALabel(lit)) {
          int32 arity = lblArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
        } else
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);
        if (code[pc + 1].op != Frame)
          return verifyError(&ctx, ".%d: expecting a frame instruction after call", pc);
        pc++;
        continue;
      }
      case XCall: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (isALabel(lit)) {
          int32 arity = lblArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
        } else
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);
        if (code[pc + 1].op != Frame)
          return verifyError(&ctx, ".%d: expecting a frame instruction after call", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 1) != Ok)
          return Error;
        pc++;
        continue;
      }

      case TCall: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (isALabel(lit)) {
          int32 arity = lblArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        } else
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: TCall should be last instruction in block", pc);

        propagateVars(&ctx, parentCtx);
        return Ok;

      }
      case OCall: {
        int arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= arity - 1;
        if (code[pc + 1].op != Frame)
          return verifyError(&ctx, ".%d: expecting a frame instruction after ocall", pc);
        pc++;
        continue;
      }
      case XOCall: {
        int arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= arity - 1;
        if (code[pc + 1].op != Frame)
          return verifyError(&ctx, ".%d: expecting a frame instruction after xocall", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 1) != Ok)
          return Error;
        pc++;
        continue;
      }
      case TOCall: {
        int32 arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: TCall should be last instruction in block", pc);
        propagateVars(&ctx, parentCtx);

        return Ok;
      }
      case Escape: {
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);

        if (esc == Null)
          return verifyError(&ctx, ".%d: invalid escape code: %d", pc, escNo);
        else {
          int32 arity = escapeArity(esc);
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
          if (code[pc + 1].op != Frame)
            return verifyError(&ctx, ".%d: expecting a frame instruction after escape", pc);
          pc++;
          continue;
        }
      }
      case XEscape: {
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);

        if (esc == Null)
          return verifyError(&ctx, ".%d: invalid escape code: %d", pc, escNo);
        else {
          int32 arity = escapeArity(esc);
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
          if (code[pc + 1].op != Frame)
            return verifyError(&ctx, ".%d: expecting a frame instruction after escape", pc);
          if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 1) != Ok)
            return Error;

          pc++;
          continue;
        }
      }
      case Entry: {
        int32 depth = code[pc].fst;
        if (depth != ctx.lclCount)
          return verifyError(&ctx, ".%d local count %d does not match method local count %d", pc, code[pc].fst,
                             ctx.lclCount);
        pc++;
        stackDepth = 0;
        continue;
      }
      case Ret:
      case XRet: {
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Ret should be last instruction in block", pc);
        propagateVars(&ctx, parentCtx);
        return Ok;   // No merge of locals here
      }
      case Block: {
        int32 exitDepth = code[pc].fst;
        int32 blockLen = code[pc].alt;
        pc++;

        if (verifyBlock(pc - 1, pc, pc + blockLen, False, &ctx, stackDepth, exitDepth) == Ok) {
          stackDepth = exitDepth;
          pc += blockLen;
          continue;
        } else
          return Error;
      }

      case Loop: {
        if (!isLastPC(pc, limit))
          return verifyError(&ctx, ".%d: Loop should be last instruction in block", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        return Ok;
      }
      case Break: {
        if (!isLastPC(pc, limit))
          return verifyError(&ctx, ".%d: Break should be last instruction in block", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        return Ok;
      }
      case Result: {
        if (!isLastPC(pc, limit))
          return verifyError(&ctx, ".%d: Result should be last instruction in block", pc);

        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: Result should leave at least one value on stack", pc);
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 1) != Ok)
          return Error;
        return Ok;
      }

      case Drop: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: stack depth %d does not permit a Drop", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }

      case Dup: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: stack depth %d does not permit a Dup", pc, stackDepth);
        stackDepth++;
        pc++;
        continue;
      }

      case Rot: {
        int32 count = code[pc].fst;
        if (stackDepth < count)
          return verifyError(&ctx, ".%d: insufficient stack depth for rotate %d", pc, count);
        pc++;
        continue;
      }
      case Rst: {
        int32 depth = code[pc].fst;
        if (stackDepth < depth)
          return verifyError(&ctx, ".%d: insufficient stack depth for stack reset %d", pc, depth);
        stackDepth = depth;
        pc++;
        continue;
      }
      case Pick: {
        int32 count = code[pc].fst;
        int32 keep = code[pc].alt;

        if (stackDepth < count || stackDepth < keep)
          return verifyError(&ctx, ".%d: insufficient stack depth (%d) for stack keep %d of %d",
                             pc, stackDepth, keep, count);

        if (keep > count)
          return verifyError(&ctx, ".%d: trying to keep more elements (%d) than depth (%d) ", pc, keep, count);

        stackDepth = count;
        pc++;
        continue;
      }
      case Fiber: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient stack depth for Fiber", pc);
        pc++;
        continue;
      }

      case Resume:
      case Suspend: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient stack depth for Resume/Suspend", pc);
        pc++;
        stackDepth--;
        continue;
      }

      case Retire: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient stack depth for Retire", pc);
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Retire should be last instruction in block", pc);

        propagateVars(&ctx, parentCtx);
        return Ok;
      }
      case Underflow:
        return verifyError(&ctx, ".%d: special instruction illegal in regular code %", pc);
      case LdV:
        stackDepth++;
        pc++;
        continue;
      case LdC: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);
        stackDepth++;
        pc++;
        continue;
      }
      case LdA: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo >= codeArity(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds argument number: %d", pc, argNo);
        stackDepth++;
        pc++;
        continue;
      }
      case LdL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        locals[lclNo].read = True;
        if (!locals[lclNo].inited)
          return verifyError(&ctx, ".%d Read from uninitialized local %d", pc, lclNo);
        stackDepth++;
        pc++;
        continue;
      }
      case StL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= ctx.lclCount)
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        locals[lclNo].inited = True;
        stackDepth--;
        pc++;
        continue;
      }
      case StV:
      case TL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= ctx.lclCount)
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        locals[lclNo].inited = True;
        pc++;
        continue;
      }
      case LdG: {
        int32 glbNo = code[pc].fst;

        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(&ctx, ".%d unknown global variable: %d", pc, glbNo);
        stackDepth++;
        pc++;
        continue;
      }
      case StG: {
        int32 glbNo = code[pc].fst;
        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(&ctx, ".%d unknown global variable: %d", pc, glbNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case TG: {
        int32 glbNo = code[pc].fst;
        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(&ctx, ".%d unknown global variable: %d", pc, glbNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case Sav: {
        stackDepth++;
        pc++;
        continue;
      }
      case TstSav: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case LdSav: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;

        pc++;
        continue;
      }
      case StSav: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        pc++;
        continue;
      }
      case TSav: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case Cell: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case Get: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case Assign: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        pc++;
        continue;
      }
      case CLit:{
        int32 key = code[pc].fst;
        if (!isDefinedConstant(key))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CInt:{
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        if (lit==Null || !isInteger(lit))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CFlt:{
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        if (lit==Null || !isFloat(lit))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CChar:{
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        if (lit==Null || !isChar(lit))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CLbl: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid label: %T", pc, lit);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        pc++;
        continue;
      }
      case Nth: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case StNth: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        pc++;
        continue;
      }
      case If:
      case IfNot: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        pc++;
        continue;
      }
      case ICase:
      case Case:
      case IxCase: {
        int32 mx = code[pc].fst;
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        for (int32 ix = 0; ix < mx; ix++) {
          int32 casePc = pc + 1 + ix;
          insPo caseIns = &code[casePc];
          switch (caseIns->op) {
            case Break:
            case Loop:
              if (checkBreak(&ctx, casePc, casePc + code[casePc].alt + 1, 0) != Ok)
                return Error;
              continue;
            default:
              return verifyError(&ctx, ".%d: invalid case instruction", casePc);
          }
        }
        if (!isLastPC(pc + mx + 1, limit))
          return verifyError(&ctx, ".%d: Case should be last instruction in block", pc);

        propagateVars(&ctx, parentCtx);
        return Ok;
      }
      case IAdd:
      case ISub:
      case IMul: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case IDiv:
      case IMod: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case IAbs: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case IEq:
      case ILt:
      case IGe: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case Cmp:
      case ICmp:
      case CCmp:
      case FCmp: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        else if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0) != Ok)
          return Error;
        else {
          stackDepth -= 2;
          pc++;
          continue;
        }
      }
      case CEq:
      case CLt:
      case CGe:
      case BAnd:
      case BOr:
      case BXor:
      case BLsl:
      case BLsr:
      case BAsr: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case BNot: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case FAdd:
      case FSub:
      case FMul: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case FDiv:
      case FMod: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case FAbs: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case FEq:
      case FLt:
      case FGe: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case Alloc: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid symbol literal: %t", pc, lit);
        else {
          int32 arity = lblArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient stack args for Alloc instruction", pc);
          else if (code[pc + 1].op != Frame)
            return verifyError(&ctx, ".%d: expecting Frame instruction after Alloc", pc);
          else {
            stackDepth = stackDepth - arity + 1;
            pc++;
            continue;
          }
        }
      }
      case Closure: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid Closure literal: %t", pc, lit);
        else {
          if (stackDepth < 1)
            return verifyError(&ctx, ".%d: insufficient stack args for Closure instruction", pc);
          else {
            pc++;
            continue;
          }
        }
      }
      case Frame: {
        int32 depth = code[pc].fst;
        if (depth != stackDepth)
          return verifyError(&ctx, ".%d: stack depth %d does not match Frame instruction %d", pc,
                             stackDepth, depth);
        pc++;
        continue;
      }
      case dBug:
        pc++;
        continue;

      default:
        return verifyError(&ctx, ".%d: illegal instruction", pc);
    }
  }
  propagateVars(&ctx, parentCtx);
  return Ok;
}

retCode verifyError(verifyCtxPo ctx, char *msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args;      /* access the generic arguments */
  va_start(args, msg);    /* start the variable argument sequence */


  __voutMsg(O_IO(f), msg, args);  /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0);                /* Terminate the string */

  closeIo(O_IO(f));

  strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "%s%s"RED_ESC_OFF, ctx->prefix, buff);
  return Error;
}

