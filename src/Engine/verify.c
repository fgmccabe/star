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

typedef struct verify_context_ *verifyCtxPo;
typedef struct verify_context_ {
  char *prefix;
  methodPo mtd;
  int32 from;
  int32 limit;
  integer entryDepth;
  integer exitDepth;
  varPo locals;
  verifyCtxPo parent;
  char *errorMsg;
  long msgLen;
} VerifyContext;

static retCode verifyError(verifyCtxPo ctx, char *msg, ...);
static retCode verifyBlock(int32 from, int32 limit, int32 tpe, int32 *delta, verifyCtxPo ctx);
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

  VerifyContext mtdCtx = {.prefix = "", .errorMsg=errorMsg, .msgLen=msgLen,
			  .mtd=mtd, .from = 0, .limit=codeSize(mtd),
			  .parent=Null,
			  .locals=initVars(lclCount(mtd))};

  int32 delta = 0;

  tryRet(verifyBlock(0,codeSize(mtd),methodSigLit(mtd),&delta,&mtdCtx));

  eraseVars(mtdCtx.locals);
  return Ok;
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

static retCode checkBreak(verifyCtxPo ctx, integer pc, integer stackDepth, integer tgt) {
  verifyCtxPo tgtCtx = ctx;

  while (tgtCtx != Null && tgt>0) {
    tgt--;
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

static logical isLastPC(int32 pc,int32 limit){
  return pc>=limit-1;
}

retCode verifyBlock(int32 from, int32 limit, int32 tpe, int32 *delta,verifyCtxPo parentCtx){
  integer entryDepth, exitDepth;
  tryRet(extractBlockSig(&entryDepth, &exitDepth, parentCtx, tpe));
  integer stackDepth = entryDepth;
  insPo code = parentCtx->mtd->instructions;

  *delta = exitDepth-entryDepth;

  char prefix[MAXLINE];
  strMsg(prefix, NumberOf(prefix), "%s.%d", parentCtx->prefix, from);

  VerifyContext ctx = {.prefix=prefix,
    .errorMsg=parentCtx->errorMsg,.msgLen=parentCtx->msgLen,
    .mtd = parentCtx->mtd,
    .parent = parentCtx,
    .entryDepth = entryDepth, .exitDepth=exitDepth,
    .locals = copyVars(parentCtx->locals,lclCount(parentCtx->mtd)) };

  int32 pc = from;

  while(pc < limit) {
    insPo ins = &code[pc];
    switch (ins->op) {
      case Halt: {
        if (!isLastPC(pc,limit))
          return verifyError(&ctx, ".%d: Halt should be last instruction in block", pc);
        else
	  break;
      }
      case Nop:
	pc++;
        break;
      case Abort: {
        if (!isLastPC(pc,limit))
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
          integer arity = labelArity(C_LBL(lit));
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
          integer arity = labelArity(C_LBL(lit));
          if (stackDepth < arity)
            return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
          stackDepth -= arity - 1;
        } else
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);
        if (!isLastPC(pc,limit))
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
        int arity = code[pc].fst;
        if (stackDepth < arity)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        if (!isLastPC(pc,limit))
          return verifyError(&ctx, ".%d: TCall should be last instruction in block", pc);
        break;
      }
      case Escape: {
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);

        if (esc == Null)
          return verifyError(&ctx, ".%d: invalid escape code: %d", pc, escNo);
        else {
          integer arity = escapeArity(esc);
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
        continue;
      case Ret:{
        if (!isLastPC(pc,limit))
          return verifyError(&ctx, ".%d: Ret should be last instruction in block", pc);
	break;
      }
      case Block: {
        int32 litNo = code[pc].fst;
	int32 blockDelta = 0;

	if(verifyBlock(pc+1,pc+code[pc].alt,litNo,&blockDelta,&ctx)==Ok){
	  stackDepth += blockDelta;
	  pc = pc+code[pc].alt;
	  continue;
	}
	else
	  return Error;
      }

      case Loop:
      case Break: {
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt) != Ok)
          return Error;
	if(!isLastPC(pc,limit))
          return verifyError(&ctx, ".%d: Break should be last instruction in block", pc);
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
        int32 litNo = code[pc].fst;
	int32 blockDelta = 0;

	if(verifyBlock(pc+1,pc+code[pc].alt,litNo,&blockDelta,&ctx)==Ok){
	  stackDepth += blockDelta;
	  pc = pc+code[pc].alt;
	  continue;
	}
	else
	  return Error;
      }
      case EndTry:{
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient stack depth for EndTry", pc);
	stackDepth--;
	pc++;
        continue;
      }
      case Throw: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
	if(!isLastPC(pc,limit))
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
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, argNo);
        stackDepth++;
	pc++;
        continue;
      }
      case StL: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, argNo);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
	pc++;
        continue;
      }
      case StV:
      case TL: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo > lclCount(ctx.mtd))
          return verifyError(&ctx, ".%d Out of bounds local number: %d", pc, argNo);
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
      case CLbl: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
        termPo lit = getMtdLit(ctx.mtd, litNo);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid label: %t", pc, lit);
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt) != Ok)
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
        if (checkBreak(&ctx, pc, stackDepth, code[pc].alt) != Ok)
          return Error;
	pc++;
        continue;
      }
      case Case:
      case IndxJmp: {
        int32 mx = code[pc].fst;
        if (stackDepth < 1)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        for (integer ix = 0; ix < mx; ix++) {
          insPo caseIns = &code[pc + ix];
          switch (caseIns->op) {
            case Break:
            case Loop:
              continue;
            default:
              return verifyError(&ctx, ".%d: invalid case instruction", pc + ix);
          }
        }
	pc+=mx;
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
        else if (checkBreak(&ctx, pc, stackDepth - 2, code[pc].alt) != Ok)
          return Error;
        else {
          stackDepth -= 1;
	  pc++;
          continue;
        }
      }
      case CEq:
      case CLt:
      case CGe: {
        if (stackDepth < 2)
          return verifyError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
	pc++;
        continue;
      }
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
          integer arity = labelArity(C_LBL(lit));
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
          else{
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
        integer depth;

        if (isString(frameLit)) {
          integer sigLen;
          const char *sig = strVal(frameLit, &sigLen);

          tryRet(typeSigArity(sig, sigLen, &depth));
        } else if (isInteger(frameLit))
          depth = integerVal(frameLit);
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
      case Line:
      case Local: {
        int32 litNo = code[pc].fst;
        if (litNo < 0 || litNo >= codeLitCount(ctx.mtd))
          return verifyError(&ctx, ".%d: invalid literal number: %d ", pc, litNo);
	pc++;
        continue;
      }
      case illegalOp:
      case maxOpCode:
        return verifyError(&ctx, ".%d: illegal instruction", pc);
    }
  }
  mergeVars(parentCtx->locals,ctx.locals,lclCount(ctx.mtd));
  eraseVars(ctx.locals);
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

