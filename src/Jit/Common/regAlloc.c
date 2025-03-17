//
// Created by Francis McCabe on 3/12/25.
//

#include "regAlloc.h"
#include "escape.h"
#include "codeP.h"
#include "debugP.h"
#include "strings.h"

static poolPo rangePool = Null;
static poolPo varPool = Null;

void initRegAlloc() {
  if (rangePool == Null) {
    rangePool = newPool(sizeof(RangeRecord), 2048);
    varPool = newPool(sizeof(VarSpec), 2048);
  }
}

int32 computeRanges(methodPo mtd) {
  int32 localCnt = 0;
  int32 stackCnt = 0;

}

typedef struct range_context_ *rangeCxtPo;
typedef struct range_context_ {
  char *prefix;
  methodPo mtd;
  int32 from;
  int32 limit;
  int32 entryDepth;
  int32 exitDepth;
  int32 currDepth;
  rangeCxtPo propagated;    // Where have we propagated to?
  logical tryBlock;      // Is this from a Try block?
  varSpecPo locals;
  int32 lclCount;
  rangeCxtPo parent;
} VerifyContext;

static retCode rangeBlock(int32 from, int32 pc, int32 limit, logical tryBlock, rangeCxtPo parentCtx, int32 entryDepth,
                          int32 exitDepth, int32 currDepth) {
  int32 stackDepth = entryDepth;
  insPo code = parentCtx->mtd->instructions;

  char prefix[MAXLINE];
  strMsg(prefix, NumberOf(prefix), "%s.%d", parentCtx->prefix, from);

  int32 lclCnt = parentCtx->lclCount;
  VarSpec locals[lclCnt];
  for (integer ix = 0; ix < lclCnt; ix++)
    locals[ix] = parentCtx->locals[ix];

  VerifyContext ctx = {.prefix=prefix,
    .mtd = parentCtx->mtd,
    .from = from,
    .limit = limit,
    .parent = parentCtx,
    .entryDepth = entryDepth,
    .exitDepth = exitDepth,
    .currDepth = currDepth,
    .locals = locals,
    .lclCount = lclCnt,
    .propagated=Null,
    .tryBlock=tryBlock};

  while (pc < limit) {
    insPo ins = &code[pc];
    if (traceJit > generalTracing) {
      disass(logFile, Null, ctx.mtd, ins);
      outMsg(logFile, "\n%_");
    }

    switch (ins->op) {
      case Halt: {
        terminateAllVars();
        return Ok;
      }
      case Nop:
        pc++;
        break;
      case Abort: {
        terminateAllVars();
        return Ok;
      }
      case Call: {
        int32 litNo = code[pc].fst;
        assert(litNo >= 0 && litNo < codeLitCount(ctx.mtd));
        termPo lit = getMtdLit(ctx.mtd, litNo);
        assert(isALabel(lit));
        int32 arity = labelArity(C_LBL(lit));
        assert(stackDepth >= arity);
        terminateStackVars(arity);
        newStackVar();
        stackDepth -= arity - 1;
        assert(code[pc + 1].op == Frame);
        pc++;
        continue;
      }
      case TCall: {
        int32 litNo = code[pc].fst;
        assert(litNo >= 0 && litNo < codeLitCount(ctx.mtd));
        termPo lit = getMtdLit(ctx.mtd, litNo);
        assert(isALabel(lit));
        int32 arity = labelArity(C_LBL(lit));
        assert(stackDepth >= arity);
        terminateStackVars(arity);
        terminateAllVars();
        return Ok;

      }
      case OCall: {
        int arity = code[pc].fst;
        assert(stackDepth >= arity);
        stackDepth -= arity - 1;
        assert(code[pc + 1].op == Frame);
        terminateStackVars(arity);
        newStackVar();
        pc++;
        continue;
      }
      case TOCall: {
        int32 arity = code[pc].fst;
        assert(stackDepth >= arity);
        terminateStackVars(arity);
        terminateAllVars();
        return Ok;
      }
      case Escape: {
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        assert(esc != Null);

        int32 arity = escapeArity(esc);
        assert(stackDepth >= arity);
        terminateStackVars(arity);
        newStackVar();
        stackDepth -= arity - 1;
        assert(code[pc + 1].op == Frame);
        pc++;
        continue;
      }
      case Entry: {
        assert(code[pc].fst == ctx.lclCount);
        pc++;
        stackDepth = 0;
        newLocalVars(code[pc].fst);
        continue;
      }
      case Ret: {
        terminateAllVars();
        return Ok;   // No merge of locals here
      }
      case Block: {
        termPo lit = getMtdLit(ctx.mtd, code[pc].fst);
        assert(isString(lit));
        integer sigLn;
        const char *blockSg = strVal(lit, &sigLn);
        int32 blockEntryDepth, blockExitDepth;
        tryRet(extractBlockSig(&blockEntryDepth, &blockExitDepth, &ctx, blockSg, sigLn));

        assert(stackDepth >= blockEntryDepth);
        int32 blockLen = code[pc].alt;
        pc++;

        if (rangeBlock(pc - 1, pc, pc + blockLen, False, &ctx, blockEntryDepth, blockExitDepth, stackDepth) == Ok) {
          stackDepth += blockExitDepth - blockEntryDepth;
          pc += blockLen;
          continue;
        } else
          return Error;
      }

      case Loop: {
        assert(isLastPC(pc, limit));

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
          return Error;
        return Ok;
      }
      case Break: {
        assert(isLastPC(pc, limit));


        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
          return Error;
        return Ok;
      }
      case Result: {
        assert(isLastPC(pc, limit));

        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: Result should leave at least one value on stack", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
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
        int32 count = code[pc].fst;
        if (ctxDepth(ctx.parent, stackDepth) < count)
          return verifyError(&ctx, ".%d: insufficient stack depth for stack reset %d", pc, count);
        stackDepth = count - ctxDepth(ctx.parent, stackDepth);
        if (stackDepth < 0)
          return verifyError(&ctx, ".%d: insufficient block stack depth for stack reset %d", pc, count);
        pc++;
        continue;
      }
      case Pick: {
        int32 depth = code[pc].fst;
        int32 keep = code[pc].alt;

        if (ctxDepth(ctx.parent, stackDepth) < depth)
          return verifyError(&ctx, ".%d: insufficient stack depth for stack reset %d", pc, depth);

        if (keep > depth)
          return verifyError(&ctx, ".%d: trying to keep more elements (%d) than depth (%d) ", pc, keep, depth);

        stackDepth = depth - ctxDepth(ctx.parent, stackDepth);
        pc++;
        continue;
      }
      case Fiber: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient stack depth for Fiber", pc);
        pc++;
        continue;
      }
      case Spawn: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient stack depth for Spawn", pc);
        pc++;
        stackDepth++;
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

      case VoidTry:
        pc++;
        stackDepth++;
        continue;

      case Try: {
        termPo lit = getMtdLit(ctx.mtd, code[pc].fst);
        if (!isString(lit)) {
          return verifyError(&ctx, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
        } else {
          integer sigLn;
          const char *blockSg = strVal(lit, &sigLn);

          int32 blockEntryDepth, blockExitDepth;
          tryRet(extractBlockSig(&blockEntryDepth, &blockExitDepth, &ctx, blockSg, sigLn));

          if (stackDepth < blockEntryDepth - 1) // The Try block has an extra value pushed on stack
            return verifyError(&ctx, ".%d Try Block stack on entry insufficiently deep: %d vs actual %d", pc,
                               blockEntryDepth, stackDepth);

          int32 blockLen = code[pc].alt;
          pc++;

          if (verifyBlock(pc - 1, pc, pc + blockLen, True, &ctx, blockEntryDepth, blockExitDepth, stackDepth) == Ok) {
            stackDepth++; // We have an error code if we fail
            pc += blockLen;
            continue;
          } else
            return Error;
        }
      }
      case EndTry: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient stack depth for EndTry", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth - 1, True) != Ok)
          return Error;
        stackDepth--;
        pc++;
        continue;
      }
      case TryRslt: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient stack depth for TryRslt", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth - 1, True) != Ok)
          return Error;
        stackDepth--;
        pc++;
        continue;
      }
      case Throw: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Throw should be last instruction in block", pc);

        propagateVars(&ctx, parentCtx);
        return Ok;
      }
      case LdV:
        stackDepth++;
        pc++;
        continue;
      case LdC: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
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
      case LdS: {
        int32 stackOff = code[pc].fst;
        if (stackOff < 0 || stackOff >= stackDepth)
          return verifyError(&ctx, ".%d Out of bounds stack offset: %d", pc, stackOff);
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

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth - 1, False) != Ok)
          return Error;

        pc++;
        continue;
      }
      case StSav: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
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
      case CLit: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CLbl: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx.mtd, litNo);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid label: %T", pc, lit);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
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
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case Case:
      case IndxJmp: {
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
              if (checkBreak(&ctx, casePc, casePc + code[casePc].alt + 1, stackDepth, False) != Ok)
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
        if (stackDepth < 3)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 2;
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
        else if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth - 2, False) != Ok)
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
        if (stackDepth < 3)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 2;
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
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx.mtd, litNo);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid symbol literal: %t", pc, lit);
        else {
          int32 arity = labelArity(C_LBL(lit));
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
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx.mtd, litNo);
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
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo frameLit = getMtdLit(ctx.mtd, litNo);
        int32 depth;

        if (isString(frameLit)) {
          integer frameSigLen;
          const char *sig = strVal(frameLit, &frameSigLen);

          tryRet(typeSigArity(sig, frameSigLen, &depth));
        } else if (isInteger(frameLit))
          depth = (int32) integerVal(frameLit);
        else
          return verifyError(&ctx, ".%d: invalid Frame literal: %T", pc, lit);

        if (depth != ctxDepth(&ctx, stackDepth))
          return verifyError(&ctx, ".%d: stack depth %d does not match Frame instruction %d", pc,
                             ctxDepth(ctx.parent, stackDepth), depth);

        pc++;
        continue;
      }
      case dBug:
        pc++;
        continue;

      case illegalOp:
      case maxOpCode:
        return verifyError(&ctx, ".%d: illegal instruction", pc);
    }
  }

  propagateVars(&ctx, parentCtx);
  return
    Ok;
}

int32 ctxDepth(rangeCxtPo ctx, int32 currDepth) {
  int32 depth = currDepth;
  while (ctx != Null) {
    if (ctx->tryBlock)
      depth -= ctx->entryDepth - 1;
    else
      depth -= ctx->entryDepth;
    depth += ctx->currDepth;
    ctx = ctx->parent;
  }
  return depth;
}
