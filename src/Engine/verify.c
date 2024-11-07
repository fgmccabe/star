//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>
#include <debug.h>
#include "verifyP.h"
#include "ltype.h"
#include "arith.h"
#include "libEscapes.h"

logical enableVerify = True;         // True if we verify code as it is loaded
logical traceVerify = False;      // true if tracing code verification

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
  integer exitDepth;
  logical propagated;    // Have we had an exit?
  logical tryBlock;      // Is this from a Try block?
  varPo locals;
  int32 lclCount;
  verifyCtxPo parent;
  char *errorMsg;
  long msgLen;
} VerifyContext;

static retCode verifyError(verifyCtxPo ctx, char *msg, ...);
static retCode
verifyBlock(int32 from, int32 pc, int32 limit, int32 *delta, logical tryBlock, verifyCtxPo parentCtx,
            const char *blockSig, integer sigLen);
static retCode
extractBlockSig(int32 *entryDepth, int32 *exitDepth, verifyCtxPo ctx, const char *blockSig, integer sigLen);

static void initVars(varPo vars, integer count) {
  for (integer ix = 0; ix < count; ix++) {
    vars[ix].inited = False;
    vars[ix].read = False;
  }
}

static void copyVars(varPo vars, varPo src, integer count) {
  for (integer ix = 0; ix < count; ix++)
    vars[ix] = src[ix];
}

static void mergeVars(varPo dst, varPo src, integer count, logical propagated) {
  assert(dst != Null && src != Null);
  for (integer ix = 0; ix < count; ix++) {
    if (!propagated)
      dst[ix].inited |= src[ix].inited;
    dst[ix].read |= src[ix].read;
  }
}

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
#ifdef TRACEVERIFY
  if (traceVerify)
    showMethodCode(logFile, "Verify method %s\n", name, mtd);
#endif

  int32 lclCnt = lclCount(mtd);
  Var locals[lclCnt];

  initVars(locals, lclCnt);

  VerifyContext mtdCtx = {.prefix = "", .errorMsg=errorMsg, .msgLen=msgLen,
    .mtd=mtd, .from = 0, .limit=codeSize(mtd),
    .parent=Null,
    .exitDepth=1,
    .locals=locals,
    .lclCount=lclCnt,
    .propagated=False,
    .tryBlock=False};

  int32 delta = 0;

  char *funBlockSig = "F()p";    // Special block signature for function bodies

  return verifyBlock(0, 0, codeSize(mtd), &delta, False, &mtdCtx, funBlockSig, uniStrLen(funBlockSig));
}

retCode
extractBlockSig(int32 *entryDepth, int32 *exitDepth, verifyCtxPo ctx, const char *blockSig, integer sigLen) {
  if (validTypeSig(blockSig, sigLen) != Ok) {
    strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
    return Error;
  } else {
    integer argPos = 0;
    if (funArgSig(blockSig, sigLen, &argPos) != Ok) {
      strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
      return Error;
    }
    if (typeSigArity(&blockSig[argPos], sigLen - argPos, entryDepth) != Ok) {
      strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "Invalid argument signature of signature literal: %T"RED_ESC_OFF,
             lit);
      return Error;
    } else {
      integer pos = argPos;
      skipTypeSig(blockSig, sigLen, &pos);
      if (isTupleSig(&blockSig[pos], sigLen - pos)) {
        if (typeSigArity(&blockSig[pos], sigLen - pos, exitDepth) != Ok) {
          strMsg(ctx->errorMsg, ctx->msgLen,
                 RED_ESC_ON "Invalid result signature of signature literal: %T"RED_ESC_OFF, lit);
          return Error;
        }
      } else
        *exitDepth = 1;
    }
    return Ok;
  }
}

static retCode checkBreak(verifyCtxPo ctx, int32 pc, integer stackDepth, int32 delta, logical isTry) {
  verifyCtxPo tgtCtx = ctx;

  int32 tgt = pc + 1 + delta;

  while (tgtCtx != Null && tgtCtx->from != tgt) {
    if(!isTry && tgtCtx->tryBlock)
      return verifyError(ctx, ".%d:not permitted to break out of a try block", pc);
    tgtCtx = tgtCtx->parent;
  }

  if (tgtCtx != Null) {
    if (stackDepth < tgtCtx->exitDepth)
      return verifyError(ctx, ".%d: block exit depth %d exceeds current stack depth %d", pc, tgtCtx->exitDepth,
                         stackDepth);
  } else
    return verifyError(ctx, ".%d: break target not found", pc);
  mergeVars(tgtCtx->locals, ctx->locals, lclCount(ctx->mtd), ctx->propagated);
  ctx->propagated = True;
  return Ok;
}

static logical isLastPC(int32 pc, int32 limit) {
  return pc >= limit - 1;
}

retCode verifyBlock(int32 from, int32 pc, int32 limit, int32 *delta, logical tryBlock, verifyCtxPo parentCtx,
                    const char *blockSig, integer sigLen) {
  int32 entryDepth, exitDepth;
  tryRet(extractBlockSig(&entryDepth, &exitDepth, parentCtx, blockSig, sigLen));
  int32 stackDepth = entryDepth;
  insPo code = parentCtx->mtd->instructions;

  *delta = exitDepth - entryDepth;

  char prefix[MAXLINE];
  strMsg(prefix, NumberOf(prefix), "%s.%d", parentCtx->prefix, from);

  int32 lclCnt = parentCtx->lclCount;
  Var locals[lclCnt];
  copyVars(locals, parentCtx->locals, lclCnt);

  VerifyContext ctx = {.prefix=prefix,
    .errorMsg=parentCtx->errorMsg, .msgLen=parentCtx->msgLen,
    .mtd = parentCtx->mtd,
    .from = from,
    .limit = limit,
    .parent = parentCtx,
    .exitDepth=exitDepth,
    .locals = locals,
    .lclCount = lclCnt,
    .propagated=False,
    .tryBlock=tryBlock};

  while (pc < limit) {
    insPo ins = &code[pc];
    switch (ins->op) {
      case Halt: {
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Halt should be last instruction in block", pc);
        else
          break;
      }
      case Nop:
        pc++;
        break;
      case Abort: {
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Abort should be last instruction in block", pc);
        else
          break;
      }
      case Call: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx.mtd, litNo);
        if (isALabel(lit)) {
          int32 arity = labelArity(C_LBL(lit));
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
      case TCall: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx.mtd, litNo);
        if (isALabel(lit)) {
          int32 arity = labelArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
        } else
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: TCall should be last instruction in block", pc);
        break;
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
      case TOCall: {
        int32 arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: TCall should be last instruction in block", pc);
        break;
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
      case Entry:
        pc++;
        stackDepth = 0;
        continue;
      case Ret: {
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Ret should be last instruction in block", pc);
        return Ok;   // No merge of locals here
      }
      case Block: {
        int32 blockDelta = 0;
        termPo lit = getMtdLit(ctx.mtd, code[pc].fst);
        if (!isString(lit)) {
          return verifyError(&ctx, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
        } else {
          integer sigLn;
          const char *blockSg = strVal(lit, &sigLn);
          int32 blockLen = code[pc].alt;
          pc++;

          if (verifyBlock(pc - 1, pc, pc + blockLen, &blockDelta, False, &ctx, blockSg, sigLn) == Ok) {
            stackDepth += blockDelta;
            pc += blockLen;
            continue;
          } else
            return Error;
        }
      }

      case Loop:
      case Break: {
        if (!isLastPC(pc, limit))
          return verifyError(&ctx, ".%d: Break should be last instruction in block", pc);

        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt, False) != Ok)
          return Error;
        pc++;
        break;
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
        if (stackDepth < count)
          return verifyError(&ctx, ".%d: insufficient stack depth for stack reset %d", pc, count);
        pc++;
        continue;
      }
      case Pick: {
        int32 height = code[pc].fst;
        int32 pick = code[pc].alt;

        if (stackDepth < height)
          return verifyError(&ctx, ".%d: insufficient stack depth for stack reset %d", pc, height);
        if (height < pick)
          return verifyError(&ctx, ".%d: pick count should not be greater than height %d", pc, pick, height);
        pc++;
        continue;
      }

      case Fiber:
      case Spawn:
      case Suspend:
      case Resume:
      case Retire:
        return verifyError(&ctx, ".%d: verify not implemented", pc);
      case Underflow:
        return verifyError(&ctx, ".%d: special instruction illegal in regular code %", pc);
      case TEq: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case Try: {
        int32 blockDelta = 0;
        termPo lit = getMtdLit(ctx.mtd, code[pc].fst);
        if (!isString(lit)) {
          return verifyError(&ctx, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
        } else {
          integer sigLn;
          const char *blockSg = strVal(lit, &sigLn);
          int32 blockLen = code[pc].alt;
          pc++;

          if (verifyBlock(pc - 1, pc, pc + blockLen, &blockDelta, True, &ctx, blockSg, sigLn) == Ok) {
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
        stackDepth--;
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt, True) != Ok)
          return Error;
        pc++;
        continue;
      }
      case Throw: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        if (!isLastPC(pc++, limit))
          return verifyError(&ctx, ".%d: Throw should be last instruction in block", pc);
        break;
      }
      case Reset: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case Shift: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case Invoke:
        return verifyError(&ctx, ".%d: verify not implemented", pc);
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
        if (argNo < 0 || argNo > codeArity(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds argument number: %d", pc, argNo);
        stackDepth++;
        pc++;
        continue;
      }
      case LdL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        ctx.locals[lclNo].read = True;
        if (!ctx.locals[lclNo].inited)
          return verifyError(&ctx, ".%d Read from uninitialized local %d", pc, lclNo);
        stackDepth++;
        pc++;
        continue;
      }
      case StL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        ctx.locals[lclNo].inited = True;
        stackDepth--;
        pc++;
        continue;
      }
      case StV:
      case TL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        ctx.locals[lclNo].inited = True;
        pc++;
        continue;
      }
      case StA: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > codeArity(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds argument number: %d", pc, argNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
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
      case Thunk:
      case LdTh: {
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case StTh: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case TTh: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case Cell:
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
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt, False) != Ok)
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
          return verifyError(&ctx, ".%d: invalid label: %t", pc, lit);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt, False) != Ok)
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
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt, False) != Ok)
          return Error;
        stackDepth--;
        pc++;
        continue;
      }
      case Case:
      case IndxJmp: {
        int32 mx = code[pc].fst;
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        for (int32 ix = 0; ix < mx; ix++) {
          int32 casePc = pc + 1 + ix;
          insPo caseIns = &code[casePc];
          switch (caseIns->op) {
            case Break:
            case Loop:
              if (checkBreak(&ctx, casePc, stackDepth, caseIns->alt, False) != Ok)
                return Error;
              continue;
            default:
              return verifyError(&ctx, ".%d: invalid case instruction", casePc);
          }
        }
        pc += mx + 1;
        if (!isLastPC(pc, limit))
          return verifyError(&ctx, ".%d: Case should be last instruction in block", pc);
        continue;
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
        else if (checkBreak(&ctx, pc, stackDepth - 2, code[pc].alt, False) != Ok)
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

        if (depth != stackDepth)
          return verifyError(&ctx, ".%d: stack depth %d does not match Frame instruction %d", pc, stackDepth, depth);

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
  mergeVars(parentCtx->locals, locals, lclCnt, ctx.propagated);
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

