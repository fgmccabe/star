//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>
#include <debug.h>
#include "verifyP.h"
#include "topSort.h"
#include "ltype.h"
#include "arith.h"
#include "libEscapes.h"
#include "codeP.h"
#include "decodeP.h"
#include "array.h"

logical enableVerify = True;         // True if we verify code as it is loaded
logical traceVerify = False;      // true if tracing code verification

typedef struct {
  logical inited;    //  True if cell has real value
  logical read;      //  Has this cell been read?
} Var, *varPo;

typedef struct block_ {
  int32 tpe;         // Index of the type literal
  integer pc;
  integer limit;
  integer insCount;
} BlockSegment, *segmentPo;

typedef struct verify_context_ *verifyCtxPo;
typedef struct verify_context_ {
  char *prefix;
  char *errorMsg;
  long msgLen;
  methodPo mtd;
  arrayPo segments;
  verifyCtxPo parent;
  segmentPo currSeg;
  integer entryDepth;
  integer exitDepth;
  varPo locals;
} VerifyContext;

static retCode segmentCode(methodPo mtd, arrayPo *segments, char *errorMsg, integer msgLen);
static segmentPo findSegment(arrayPo segments, integer pc);
static retCode verifyError(verifyCtxPo ctx, char *msg, ...);
static retCode verifyBlock(segmentPo block, verifyCtxPo ctx);
static retCode extractBlockSig(integer *entryDepth, integer *exitDepth, verifyCtxPo ctx, int32 sigLit);

static varPo mergeVars(varPo seg, varPo next, integer count);

static varPo initVars(integer count) {
  varPo vars = (varPo) malloc(sizeof(Var) * count);
  for (integer ix = 0; ix < count; ix++) {
    vars[ix].inited = False;
    vars[ix].read = False;
  }
  return vars;
}

static void eraseVars(varPo vars) {
  free(vars);
}

varPo copyVars(varPo src, integer count) {
  varPo vars = (varPo) malloc(sizeof(Var) * count);
  for (integer ix = 0; ix < count; ix++)
    vars[ix] = src[ix];
  return vars;
}

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
#ifdef TRACEVERIFY
  if (traceVerify)
    showMethodCode(logFile, "Verify method %s\n", name, mtd);
#endif

  arrayPo segments = Null;
  tryRet(segmentCode(mtd, &segments, errorMsg, msgLen));

  if (segments == Null)
    return Ok; // Empty code
  else {
    VerifyContext mtdCtx = {.prefix = "", .errorMsg=errorMsg, .msgLen=msgLen,
      .mtd=mtd, .segments=segments, .currSeg=(segmentPo) nthEntry(segments, 0), .parent=Null,
      .locals=initVars(lclCount(mtd))};

    for (integer sx = 0; sx < arrayCount(segments); sx++)
      tryRet(verifyBlock((segmentPo) nthEntry(segments, sx), &mtdCtx));

    eraseVars(mtdCtx.locals);
    return Ok;
  }
}

static varPo mergeVars(varPo seg, varPo next, integer count) {
  if (next == Null)
    return copyVars(seg, count);
  else {
    assert(seg != Null);
    for (integer ix = 0; ix < count; ix++) {
      next[ix].inited &= seg[ix].inited;
      next[ix].read |= seg[ix].read;
    }
  }
  return next;
}

static retCode showVars();

static void showVar(char *nm, integer ix, varPo v) {
  outMsg(logFile, " %s[%d]%s%s", nm, ix, v->inited ? "*" : "", v->read ? "R" : "");
}

retCode extractBlockSig(integer *entryDepth, integer *exitDepth, verifyCtxPo ctx, int32 sigLit) {
  termPo lit = getMtdLit(ctx->mtd, sigLit);
  if (!isString(lit)) {
    strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
    return Error;
  } else {
    integer sigLen;
    const char *blockSig = strVal(lit, &sigLen);

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
        strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
        return Error;
      } else {
        integer pos = argPos;
        skipTypeSig(blockSig, sigLen, &pos);
        if (typeSigArity(&blockSig[pos], sigLen - pos, exitDepth) != Ok) {
          strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "Invalid signature literal: %T"RED_ESC_OFF, lit);
          return Error;
        }
      }
      return Ok;
    }
  }
}

static logical pcInBlock(segmentPo seg, integer pc) {
  return pc >= seg->pc && pc < seg->limit;
}

static retCode checkBreak(verifyCtxPo ctx, integer pc, integer stackDepth, integer tgtPc) {
  verifyCtxPo tgtCtx = ctx;

  while (tgtCtx != Null && !pcInBlock(tgtCtx->currSeg, tgtPc)) {
    tgtCtx = tgtCtx->parent;
  }
  if (tgtCtx != Null) {
    if (stackDepth < tgtCtx->exitDepth)
      return verifyError(ctx, ".%d: block exit depth %d exceeds current stack depth %d", pc, tgtCtx->exitDepth,
                         stackDepth);
  } else
    return verifyError(ctx, ".%d: break target not found", pc);
  return Ok;
}

retCode verifyBlock(segmentPo block, verifyCtxPo ctx) {
  integer entryDepth, exitDepth;
  tryRet(extractBlockSig(&entryDepth, &exitDepth, ctx, block->tpe));
  integer stackDepth = entryDepth;
  insPo code = ctx->mtd->instructions;

  for (integer pc = block->pc; pc < block->limit; pc++) {
    insPo ins = &code[pc];
    switch (ins->op) {
      case Halt: {
        if (pc != block->insCount - 1)
          return verifyError(ctx, ".%d: Halt should be last instruction in block", pc);
        else
          return Ok;
      }
      case Nop:
        break;
      case Abort: {
        if (pc != block->insCount - 1)
          return verifyError(ctx, ".%d: Abort should be last instruction in block", pc);
        else
          return Ok;
      }
      case Call: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx->mtd, litNo);
        if (isALabel(lit)) {
          integer arity = labelArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
        } else
          return verifyError(ctx, ".%d: invalid call label: %t", pc, lit);
        if (code[pc + 1].op != Frame)
          return verifyError(ctx, ".%d: expecting a frame instruction after call", pc);
        continue;
      }
      case TCall: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx->mtd, litNo);
        if (isALabel(lit)) {
          integer arity = labelArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
        } else
          return verifyError(ctx, ".%d: invalid call label: %t", pc, lit);
        if (pc != block->insCount - 1)
          return verifyError(ctx, ".%d: TCall should be last instruction in block", pc);
        return Ok;
      }
      case OCall: {
        int arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= arity - 1;
        continue;
      }
      case TOCall: {
        int arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        if (pc != block->insCount - 1)
          return verifyError(ctx, ".%d: TCall should be last instruction in block", pc);
        return Ok;
      }
      case Escape: {
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);

        if (esc == Null)
          return verifyError(ctx, ".%d: invalid escape code: %d", pc, escNo);
        else {
          integer arity = escapeArity(esc);
          if (stackDepth < arity)
            return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
          if (code[pc + 1].op != Frame)
            return verifyError(ctx, ".%d: expecting a frame instruction after escape", pc);
          continue;
        }
      }
      case Entry:
        continue;
      case Ret:
        if (pc != block->insCount - 1)
          return verifyError(ctx, ".%d: Ret should be last instruction in block", pc);
        return Ok;

      case Block: {
        int32 litNo = code[pc].fst;
        integer entryDepth, exitDepth;
        tryRet(extractBlockSig(&entryDepth, &exitDepth, ctx, 0));
        return verifyError(ctx, ".%d: invalid signature string: %T ", pc, lit);

        char prefix[MAXLINE];
        strMsg(prefix, NumberOf(prefix), "%s.%d", ctx->prefix, pc);
        segmentPo blockSeg = findSegment(ctx->segments, code[pc].alt);

        VerifyContext blockCtx = {.prefix = prefix, .errorMsg=ctx->errorMsg, .msgLen=ctx->msgLen,
          .mtd=ctx->mtd, .parent=ctx, .segments=ctx->segments, .currSeg = blockSeg,
          .locals = ctx->locals};

        if (verifyBlock(blockSeg, &blockCtx) == Ok) {
          stackDepth = stackDepth - entryDepth + exitDepth;
          continue;
        } else
          return Error;
      }
      case Loop:
      case Break: {
        if (checkBreak(ctx, pc, stackDepth, code[pc].alt) != Ok)
          return Error;

        continue;
      }

      case Drop: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: stack depth %d does not permit a Drop", pc, stackDepth);
        stackDepth--;
        continue;
      }

      case Dup: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: stack depth %d does not permit a Dup", pc, stackDepth);
        stackDepth++;
        continue;
      }

      case Rot: {
        int32 count = code[pc].fst;
        if (stackDepth < count)
          return verifyError(ctx, ".%d: insufficient stack depth for rotate %d", pc, count);
        continue;
      }
      case Rst: {
        int32 count = code[pc].fst;
        if (stackDepth < count)
          return verifyError(ctx, ".%d: insufficient stack depth for stack reset %d", pc, count);
        continue;
      }
      case Pick: {
        int32 height = code[pc].fst;
        int32 pick = code[pc].alt;

        if (stackDepth < height)
          return verifyError(ctx, ".%d: insufficient stack depth for stack reset %d", pc, height);
        if (height < pick)
          return verifyError(ctx, ".%d: pick count should not be greater than height %d", pc, pick, height);
        continue;
      }

      case Fiber:
        break;
      case Spawn:
        break;
      case Suspend:
        break;
      case Resume:
        break;
      case Retire:
        break;
      case Underflow:
        return verifyError(ctx, ".%d: special instruction illegal in regular code %", pc);
      case TEq: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case Try:
        break;
      case EndTry:
        break;
      case Throw: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case Reset: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case Shift: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case Invoke:
        break;
      case LdV:
        stackDepth++;
        continue;
      case LdC: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        stackDepth++;
        continue;
      }
      case LdA: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > codeArity(ctx->mtd))
          return verifyError(ctx, ".%d Out of bounds argument number: %d", pc, argNo);
        stackDepth++;
        continue;
      }
      case LdL: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > lclCount(ctx->mtd))
          return verifyError(ctx, ".%d Out of bounds local number: %d", pc, argNo);
        stackDepth++;
        continue;
      }
      case StL: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > lclCount(ctx->mtd))
          return verifyError(ctx, ".%d Out of bounds local number: %d", pc, argNo);
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        continue;
      }
      case StV:
      case TL: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > lclCount(ctx->mtd))
          return verifyError(ctx, ".%d Out of bounds local number: %d", pc, argNo);
        continue;
      }
      case StA: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > codeArity(ctx->mtd))
          return verifyError(ctx, ".%d Out of bounds argument number: %d", pc, argNo);
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        continue;
      }
      case LdG: {
        int32 glbNo = code[pc].fst;

        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(ctx, ".%d unknown global variable: %d", pc, glbNo);
        stackDepth++;
        continue;
      }
      case StG: {
        int32 glbNo = code[pc].fst;
        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(ctx, ".%d unknown global variable: %d", pc, glbNo);
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        continue;
      }
      case TG: {
        int32 glbNo = code[pc].fst;
        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(ctx, ".%d unknown global variable: %d", pc, glbNo);
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        continue;
      }
      case Thunk:
      case LdTh: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        continue;
      }
      case StTh: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        continue;
      }
      case TTh: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        continue;
      }
      case Cell:
      case Get: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        continue;
      }
      case Assign: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        continue;
      }
      case CLbl: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx->mtd, litNo);
        if (!isALabel(lit))
          return verifyError(ctx, ".%d: invalid label: %t", pc, lit);
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        continue;
      }
      case Nth: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        continue;
      }
      case StNth: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        continue;
      }
      case If:
      case IfNot: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        if (checkBreak(ctx, pc, stackDepth, code[pc].alt) != Ok)
          return Error;
        continue;
      }
      case Case:
      case IndxJmp: {
        int32 mx = code[pc].fst;
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        for (integer ix = 0; ix < mx; ix++) {
          insPo caseIns = &code[pc + ix];
          switch (caseIns->op) {
            case Break:
            case Loop:
              continue;
            default:
              return verifyError(ctx, ".%d: invalid case instruction", pc + ix);
          }
        }
        continue;
      }
      case IAdd:
      case ISub:
      case IMul: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case IDiv:
      case IMod: {
        if (stackDepth < 3)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        continue;
      }
      case IAbs: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        continue;
      }
      case IEq:
      case ILt:
      case IGe: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case Cmp:
      case ICmp:
      case CCmp:
      case FCmp: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        else if (checkBreak(ctx, pc, stackDepth - 2, code[pc].alt) != Ok)
          return Error;
        else {
          stackDepth -= 1;
          continue;
        }
      }
      case CEq:
      case CLt:
      case CGe: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case BAnd:
      case BOr:
      case BXor:
      case BLsl:
      case BLsr:
      case BAsr: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case BNot: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        continue;
      }
      case FAdd:
      case FSub:
      case FMul: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }
      case FDiv:
      case FMod: {
        if (stackDepth < 3)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        continue;
      }
      case FAbs: {
        if (stackDepth < 1)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        continue;
      }
      case FEq:
      case FLt:
      case FGe: {
        if (stackDepth < 2)
          return verifyError(ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        continue;
      }

      case Alloc: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx->mtd, litNo);
        if (!isALabel(lit))
          return verifyError(ctx, ".%d: invalid symbol literal: %t", pc, lit);
        else {
          integer arity = labelArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(ctx, ".%d: insufficient stack args for Alloc instruction", pc);
          else if (code[pc + 1].op != Frame)
            return verifyError(ctx, ".%d: expecting Frame instruction after Alloc", pc);
          else {
            stackDepth = stackDepth - arity + 1;
            continue;
          }
        }
      }

      case Closure: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx->mtd, litNo);
        if (!isALabel(lit))
          return verifyError(ctx, ".%d: invalid Closure literal: %t", pc, lit);
        else {
          if (stackDepth < 1)
            return verifyError(ctx, ".%d: insufficient stack args for Closure instruction", pc);
          else
            continue;
        }
      }

      case Frame: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo frameLit = getMtdLit(ctx->mtd, litNo);
        integer depth;

        if (isString(frameLit)) {
          integer sigLen;
          const char *sig = strVal(frameLit, &sigLen);

          tryRet(typeSigArity(sig, sigLen, &depth));
        } else if (isInteger(frameLit))
          depth = integerVal(frameLit);
        else
          return verifyError(ctx, ".%d: invalid Frame literal: %T", pc, lit);

        if (depth != stackDepth)
          return verifyError(ctx, ".%d: stack depth %d does not match Frame instruction %d", pc, stackDepth, depth);

        continue;
      }
      case dBug:
        continue;
      case Line:
      case Local: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx->mtd))
          return verifyError(ctx, ".%d: invalid literal number: %d ", pc, litNo);
        continue;
      }
      case illegalOp:
      case maxOpCode:
        return verifyError(ctx, ".%d: illegal instruction", pc);
    }
  }
  return verifyError(ctx, ".%d: execution past last instruction in block", block->insCount);
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

static comparison comparePcs(arrayPo ar, integer ix, integer iy, void *cl);

retCode segmentCode(methodPo mtd, arrayPo *segments, char *errorMsg, integer msgLen) {
  arrayPo segs = *segments = allocArray(sizeof(BlockSegment), 16, True);
  insPo code = mtd->instructions;
  integer limit = mtd->insCount;

  {
    BlockSegment fst = {.pc = 0, .limit=limit, .insCount=-1, .tpe=(int32) methodSigLit(mtd)};
    appendEntry(segs, &fst);
  }


  // We (re)discover the block boundaries in two passes
  for (integer pc = 0; pc < limit; pc++) {
    switch (code[pc].op) {
      case Block:
      case Try: {
        BlockSegment blk = {.pc = code[pc].alt, .limit=limit, .insCount=-1, .tpe=code[pc].fst};
        appendEntry(segs, &blk);
        continue;
      }
      default:
        continue;
    }
  }
  // Phase 2: sort the array
  tryRet(sortArray(segs, comparePcs, Null));

  // Phase 3, trim the segments
  for (integer ix = arrayCount(segs) - 1; ix > 0; ix--) {
    segmentPo seg = (segmentPo) nthEntry(segs, ix);
    segmentPo prev = (segmentPo) nthEntry(segs, ix - 1);

    assert(seg != Null && prev != Null);
    prev->limit = seg->pc;
    prev->insCount = prev->limit - prev->pc;
  }

  return Ok;
}

comparison comparePcs(arrayPo ar, integer ix, integer iy, void *cl) {
  segmentPo sx = (segmentPo) nthEntry(ar, ix);
  segmentPo sy = (segmentPo) nthEntry(ar, iy);
  return ixCmp(sx->pc, sy->pc);
}

segmentPo findSegment(arrayPo segments, integer pc) {
  for (integer ix = 0; ix < arrayCount(segments); ix++) {
    segmentPo seg = (segmentPo) nthEntry(segments, ix);
    if (seg->pc == pc)
      return seg;
  }
  return Null;
}
