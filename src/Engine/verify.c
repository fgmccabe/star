//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>

#include "analyseP.h"
#include "constants.h"
#include "ssaOps.h"
#include "debugP.h"
#include "verifyP.h"
#include "arith.h"
#include "escapeP.h"
#include "stackP.h"
#include "char.h"
#include "disass.h"

logical enableVerify = True; // True if we verify code as it is loaded
tracingLevel traceVerify = noTracing; // Set if tracing code verification

#define operand(i) (code[pc+(i)].op.ltrl)

typedef struct {
  logical inited; //  True if cell has real value
  logical read; //  Has this cell been read?
} Var, *varPo;

typedef struct verify_context_ *verifyCtxPo;

typedef struct verify_context_ {
  char *prefix;
  methodPo mtd;
  int32 from;
  int32 limit;
  int32 *maxArgCnt;
  int32 resltVr;
  logical hasResult;
  varPo locals;
  int32 lclCount;
  int32 argPnt;
  verifyCtxPo parent;
  char *errorMsg;
  long msgLen;
} VerifyContext;

static retCode verifyError(verifyCtxPo ctx, char *msg, ...);
static retCode verifyBlock(int32 from,
                           int32 pc,
                           int32 limit,
                           verifyCtxPo parentCtx,
                           logical hasValue, int32 rsltVr);

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
  if (traceVerify > noTracing)
    showMethodCode(logFile, "Verify %A\n", mtd);

  int32 lclCnt = lclCount(mtd) + mtdArity(mtd);
  Var locals[lclCnt];
  int32 argPnt = lclCount(mtd);
  int32 maxArgCnt = 0;

  VerifyContext mtdCtx = {
    .prefix = "", .errorMsg = errorMsg, .msgLen = msgLen,
    .mtd = mtd, .from = 0, .limit = codeSize(mtd),
    .parent = Null,
    .maxArgCnt = &maxArgCnt,
    .locals = locals,
    .lclCount = lclCnt,
    .argPnt = argPnt,
    .resltVr = -1,
    .hasResult = True
  };

  if (argPnt < 0 || argPnt > lclCnt)
    return verifyError(&mtdCtx, "invalid argPnt: %d", argPnt);

  for (int32 ix = 0; ix < lclCnt; ix++) {
    locals[ix].inited = False;
    locals[ix].read = False;
  }

  for (int32 ax = 0; ax < mtdArity(mtd); ax++) {
    locals[ax + argPnt].inited = True;
  }

  return verifyBlock(0, 0, codeSize(mtd), &mtdCtx, False, MAX_INT32);
}

static retCode checkBreak(verifyCtxPo ctx, int32 pc, int32 tgt, logical returningValue) {
  verifyCtxPo tgtCtx = ctx;

  while (tgtCtx != Null && tgtCtx->from != tgt) {
    tgtCtx = tgtCtx->parent;
  }

  if (tgtCtx != Null) {
    if (returningValue) {
      if (!tgtCtx->hasResult)
        return verifyError(tgtCtx, ".%d: exiting non-valof block with value", pc);
    } else {
      if (tgtCtx->hasResult)
        return verifyError(tgtCtx, ".%d: exiting valof block without value", pc);
    }
  } else
    return verifyError(ctx, ".%d: break target not found", pc);

  return Ok;
}

static logical isLastPC(int32 pc, int32 limit) {
  return pc >= limit;
}

static logical validLocal(verifyCtxPo ctx, int32 vrNo) {
  return vrNo + ctx->argPnt >= 0 && vrNo < ctx->lclCount - ctx->argPnt;
}

static logical initedLocal(verifyCtxPo ctx, int32 vrNo) {
  return validLocal(ctx, vrNo) && ctx->locals[ctx->argPnt + vrNo].inited;
}

static void initLocal(verifyCtxPo ctx, int32 vrNo) {
  ctx->locals[ctx->argPnt + vrNo].inited = True;
}

retCode verifyBlock(int32 from, int32 pc, int32 limit, verifyCtxPo parentCtx, logical
                    hasValue, int32 rsltVr) {
  ssaInsPo code = parentCtx->mtd->instructions;

  char prefix[MAXLINE];
  strMsg(prefix, NumberOf(prefix), "%s.%d", parentCtx->prefix, from);

  VerifyContext ctx = {
    .prefix = prefix,
    .errorMsg = parentCtx->errorMsg, .msgLen = parentCtx->msgLen,
    .mtd = parentCtx->mtd,
    .from = from,
    .limit = limit,
    .parent = parentCtx,
    .argPnt = parentCtx->argPnt,
    .maxArgCnt = parentCtx->maxArgCnt,
    .hasResult = hasValue,
    .locals = parentCtx->locals,
    .lclCount = parentCtx->lclCount,
  };

  while (pc < limit) {
    if (traceVerify > generalTracing) {
      disass(logFile, Null, ctx.mtd, &code[pc]);
      outMsg(logFile, "\n%_");
    }

    switch (code[pc].op.op) {
      case sHalt: {
        if (!isLastPC(pc + 2, limit))
          return verifyError(&ctx, ".%d: Halt should be last instruction in block", pc);

        if (!initedLocal(&ctx, operand(1)))
          return verifyError(&ctx, ".%d: var %d not initialized", pc, operand(1));
        return Ok;
      }
      case sAbort: {
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        if (!isLastPC(pc + 3, limit))
          return verifyError(&ctx, ".%d: Abort should be last instruction in block", pc);
        else {
          int32 srcVr = operand(2);
          if (!initedLocal(&ctx, srcVr))
            return verifyError(&ctx, ".%d: var %d not initialized", pc, srcVr);
          return Ok;
        }
      }
      case sCall: {
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);

        int32 arity = lblArity(C_LBL(lit));
        int32 argCnt = operand(2);

        if (argCnt != arity)
          return verifyError(&ctx, ".%d: argument count mismatch", pc);

        for (int32 ix = 0; ix < arity; ix++) {
          int32 argVr = code[pc + 3 + ix].op.ltrl;
          if (!initedLocal(&ctx, argVr))
            return verifyError(&ctx, ".%d: argument %d (local %d) not initialized", pc, ix, argVr);
        }

        if (*ctx.maxArgCnt <= argCnt)
          *ctx.maxArgCnt = argCnt;

        int32 insSize = argCnt + 3;

        if (isLastPC(pc + insSize, limit))
          return verifyError(&ctx, ".%d: Call should not be last instruction in block", pc);
        if (code[pc + insSize].op.op != sRSP && code[pc + insSize].op.op != sRSX)
          return verifyError(&ctx, ".%d: call expecting a RSP after Call", pc);
        pc += insSize;
        continue;
      }
      case sRSP: {
        int32 insWidth = 2;
        int32 callRsltVr = operand(1);
        if (!validLocal(&ctx, callRsltVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, callRsltVr);
        initLocal(&ctx, callRsltVr);
        if (isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: RSP should not be last instruction in block", pc);
        pc += insWidth;
        continue;
      }
      case sRSX: {
        int32 insWidth = 3;
        if (checkBreak(&ctx, pc, operand(1), True) != Ok)
          return Error;

        int32 callRsltVr = operand(1);
        if (!validLocal(&ctx, callRsltVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, callRsltVr);
        initLocal(&ctx, callRsltVr);
        if (isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: RSX should not be last instruction in block", pc);
        pc += insWidth;
        continue;
      }
      case sTCall: {
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid call label: %t", pc, lit);

        int32 arity = lblArity(C_LBL(lit));
        int32 argCnt = operand(2);
        int32 insWidth = arity + 3;

        if (argCnt != arity)
          return verifyError(&ctx, ".%d: argument count mismatch", pc);

        for (int32 ix = 0; ix < arity; ix++) {
          int32 argVr = code[pc + 3 + ix].op.ltrl;
          if (!initedLocal(&ctx, argVr))
            return verifyError(&ctx, ".%d: argument %d (local %d) not initialized", pc, ix, argVr);
        }

        if (*ctx.maxArgCnt <= argCnt)
          *ctx.maxArgCnt = argCnt;

        if (!isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: TCall should be last instruction in block", pc);

        pc += insWidth;
        continue;
      }
      case sOCall: {
        int32 lamVr = operand(1);
        int32 arity = operand(2);
        int32 insSize = arity + 3;

        if (!initedLocal(&ctx, lamVr))
          return verifyError(&ctx, ".%d: lambda var %d not initialized", pc, lamVr);

        for (int32 ix = 0; ix < arity; ix++) {
          int32 argVr = code[pc + 3 + ix].op.ltrl;
          if (!initedLocal(&ctx, argVr))
            return verifyError(&ctx, ".%d: argument %d (local %d) not initialized", pc, ix, argVr);
        }

        if (*ctx.maxArgCnt < arity)
          *ctx.maxArgCnt = arity;

        if (isLastPC(pc + insSize, limit))
          return verifyError(&ctx, ".%d: OCall should not be last instruction in block", pc);
        if (code[pc + insSize].op.op != sRSP && code[pc + insSize].op.op != sRSX)
          return verifyError(&ctx, ".%d: call expecting a RSP after Call", pc);
        pc += insSize;
        continue;
      }
      case sTOCall: {
        int32 lamVr = operand(1);
        int32 arity = operand(2);
        int32 insWidth = arity + 3;

        if (!initedLocal(&ctx, lamVr))
          return verifyError(&ctx, ".%d: lambda var %d not initialized", pc, lamVr);

        for (int32 ix = 0; ix < arity; ix++) {
          int32 argVr = code[pc + 3 + ix].op.ltrl;
          if (!initedLocal(&ctx, argVr))
            return verifyError(&ctx, ".%d: argument %d (local %d) not initialized", pc, ix, argVr);
        }

        if (*ctx.maxArgCnt < arity)
          *ctx.maxArgCnt = arity;

        if (!isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: TOCall should be last instruction in block", pc);
        pc += insWidth;
        continue;
      }
      case sEscape: {
        int32 escNo = operand(1);
        escapePo esc = getEscape(escNo);

        if (esc == Null)
          return verifyError(&ctx, ".%d: invalid escape code: %d", pc, escNo);
        int32 arity = escapeArity(esc);
        if (arity != operand(2))
          return verifyError(&ctx, ".%d: invalid number of arguments: %d", pc, esc);

        int32 insWidth = arity + 3;

        for (int32 ix = 0; ix < arity; ix++) {
          int32 argVr = code[pc + 3 + ix].op.ltrl;
          if (!initedLocal(&ctx, argVr))
            return verifyError(&ctx, ".%d: argument %d (local %d) not initialized", pc, ix, argVr);
        }

        if (isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: Escape should not be last instruction in block", pc);

        if (code[pc + insWidth].op.op != sRSP && code[pc + insWidth].op.op != sRSX)
          return verifyError(&ctx, ".%d: call expecting a RSP after Escape", pc);
        pc += insWidth;
        continue;
      }
      case sEntry: {
        int32 insWidth = 3;
        // TODO: Check the arity/localcount
        pc += insWidth;
        continue;
      }
      case sRet:
      case sXRet: {
        int32 rtnVr = operand(1);
        int32 insWidth = 2;

        if (!initedLocal(&ctx, rtnVr))
          return verifyError(&ctx, ".%d: return var %d not initialized", pc, rtnVr);
        if (!isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: Ret/XRet should be last instruction in block", pc);
        pc += insWidth;
        continue;
      }
      case sRtn: {
        int32 insWidth = 1;

        if (!isLastPC(pc + insWidth, limit))
          return verifyError(&ctx, ".%d: Ret/XRet should be last instruction in block", pc);
        pc += insWidth;
        continue;
      }
      case sBlock: {
        int32 blockLen = operand(1);

        if (verifyBlock(pc, pc + 2, pc + blockLen, &ctx, False, MAX_INT32) == Ok) {
          pc += blockLen;
          continue;
        }
        return Error;
      }
      case sValof: {
        int32 blkRsltVr = operand(1);
        int32 blockLen = operand(2);
        int32 insSize = 3;
        if (!validLocal(&ctx, blkRsltVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, blkRsltVr);

        if (verifyBlock(pc, pc + insSize, pc + blockLen, &ctx, True, blkRsltVr) == Ok) {
          pc += blockLen;
          initLocal(&ctx, blkRsltVr);
          continue;
        } else
          return Error;
      }
      case sBreak: {
        if (checkBreak(&ctx, pc, pc + operand(1), False) != Ok)
          return Error;
        int32 insSize = 2;
        if (!isLastPC(pc + insSize, limit))
          return verifyError(&ctx, ".%d: Break should be last instruction in block", pc);
        return Ok;
      }
      case sLoop: {
        if (checkBreak(&ctx, pc, pc + operand(1), False) != Ok)
          return Error;
        int32 insSize = 2;
        if (!isLastPC(pc + insSize, limit))
          return verifyError(&ctx, ".%d: Break should be last instruction in block", pc);
        return Ok;
      }
      case sResult: {
        int32 insSize = 3;
        int32 blkRsltVr = operand(2);

        if (checkBreak(&ctx, pc, pc + operand(1), True) != Ok)
          return Error;

        if (!initedLocal(&ctx, blkRsltVr))
          return verifyError(&ctx, ".%d: result var %d not initialized", pc, blkRsltVr);

        if (!isLastPC(pc + insSize, limit))
          return verifyError(&ctx, ".%d: Result should be last instruction in block", pc);

        pc += insSize;
        continue;
      }
      case sIf:
      case sIfNot: {
        int32 insSize = 3;
        int32 isRsltVr = operand(2);

        if (checkBreak(&ctx, pc, pc + operand(1), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, isRsltVr))
          return verifyError(&ctx, ".%d: result var %d not initialized", pc, isRsltVr);

        pc += insSize;
        continue;
      }
      case sCLbl: {
        int32 insSize = 4;
        int32 cmpVr = operand(3);

        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid label: %t", pc, lit);

        if (checkBreak(&ctx, pc, pc + operand(2), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, cmpVr))
          return verifyError(&ctx, ".%d: result var %d not initialized", pc, cmpVr);

        pc += insSize;
        continue;
      }
      case sCInt: {
        int32 insSize = 4;
        int32 cmpVr = operand(3);

        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (lit == Null || !isInteger(lit))
          return verifyError(&ctx, ".%d: invalid constant: %t", pc, lit);

        if (checkBreak(&ctx, pc, pc + operand(2), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, cmpVr))
          return verifyError(&ctx, ".%d: result var %d not initialized", pc, cmpVr);

        pc += insSize;
        continue;
      }
      case sCFlt: {
        int32 insSize = 4;
        int32 cmpVr = operand(3);

        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (lit == Null || !isFloat(lit))
          return verifyError(&ctx, ".%d: invalid constant: %t", pc, lit);

        if (checkBreak(&ctx, pc, pc + operand(2), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, cmpVr))
          return verifyError(&ctx, ".%d: result var %d not initialized", pc, cmpVr);

        pc += insSize;
        continue;
      }
      case sCChar: {
        int32 insSize = 4;
        int32 cmpVr = operand(3);

        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (lit == Null || !isChar(lit))
          return verifyError(&ctx, ".%d: invalid constant: %t", pc, lit);

        if (checkBreak(&ctx, pc, pc + operand(2), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, cmpVr))
          return verifyError(&ctx, ".%d: result var %d not initialized", pc, cmpVr);

        pc += insSize;
        continue;
      }
      case sCLit: {
        int32 insSize = 4;
        int32 cmpVr = operand(3);

        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        if (checkBreak(&ctx, pc, pc + operand(2), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, cmpVr))
          return verifyError(&ctx, ".%d: compare var %d not initialized", pc, cmpVr);

        pc += insSize;
        continue;
      }
      case sICase:
      case sCase:
      case sIxCase: {
        int32 gvVr = operand(1);
        int32 caseCount = operand(2);
        int32 insSize = 3;

        if (!initedLocal(&ctx, gvVr))
          return verifyError(&ctx, ".%d: case governing var %d not initialized", pc, gvVr);

        pc += insSize;

        for (int32 ix = 0; ix < caseCount; ix++) {
          switch (code[pc].op.op) {
            case sBreak:
            case sLoop:
              if (checkBreak(&ctx, pc, pc + operand(1), False) != Ok)
                return Error;
              pc += 2;
              continue;
            default:
              return verifyError(&ctx, ".%d: invalid case instruction", pc);
          }
        }
        if (!isLastPC(pc, limit))
          return verifyError(&ctx, ".%d: Case should be last instruction in block", pc);
        continue;
      }
      case sMC: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 constant = operand(2);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (lit == Null)
          return verifyError(&ctx, ".%d: invalid constant: %t", pc, lit);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sMv: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sLG: {
        int32 insSize = 2;
        int32 glbNo = operand(1);

        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(&ctx, ".%d unknown global variable: %d", pc, glbNo);

        pc += insSize;
        continue;
      }
      case sSG: {
        int32 insSize = 3;
        int32 glbNo = operand(1);
        int32 srcVr = operand(1);

        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return verifyError(&ctx, ".%d unknown global variable: %d", pc, glbNo);

        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        pc += insSize;
        continue;
      }
      case sSav: {
        int32 insSize = 2;
        int32 tgtVr = operand(1);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sTstSav: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sLdSav: {
        int32 insSize = 4;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        if (checkBreak(&ctx, pc, operand(1), False) != Ok)
          return Error;

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sStSav:
      case sCell:
      case sGet: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sAssign: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!initedLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not inited", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sNth: {
        int32 insSize = 4;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sStNth: {
        int32 insSize = 4;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!initedLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not inited", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sIAdd:
      case sISub:
      case sIMul: {
        int32 insSize = 4;
        int32 tgtVr = operand(1);
        int32 srcVr1 = operand(2);
        int32 srcVr2 = operand(3);
        if (!initedLocal(&ctx, srcVr1))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr1);
        if (!initedLocal(&ctx, srcVr2))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr2);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sIDiv:
      case sIMod: {
        int32 insSize = 5;
        int32 tgtVr = operand(2);
        int32 srcVr1 = operand(3);
        int32 srcVr2 = operand(4);

        if (checkBreak(&ctx, pc, operand(1), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, srcVr1))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr1);
        if (!initedLocal(&ctx, srcVr2))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr2);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sIAbs: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(2);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sIEq:
      case sILt:
      case sIGe:
      case sCEq:
      case sCLt:
      case sCGe:
      case sBAnd:
      case sBOr:
      case sBXor:
      case sBLsl:
      case sBLsr:
      case sBAsr:
      case sFEq:
      case sFLt:
      case sFGe: {
        int32 insSize = 4;
        int32 tgtVr = operand(1);
        int32 srcVr1 = operand(2);
        int32 srcVr2 = operand(3);
        if (!initedLocal(&ctx, srcVr1))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr1);
        if (!initedLocal(&ctx, srcVr2))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr2);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sBNot: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(2);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sFAdd:
      case sFSub:
      case sFMul: {
        int32 insSize = 4;
        int32 tgtVr = operand(1);
        int32 srcVr1 = operand(2);
        int32 srcVr2 = operand(3);
        if (!initedLocal(&ctx, srcVr1))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr1);
        if (!initedLocal(&ctx, srcVr2))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr2);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sFDiv:
      case sFMod: {
        int32 insSize = 5;
        int32 tgtVr = operand(2);
        int32 srcVr1 = operand(3);
        int32 srcVr2 = operand(4);

        if (checkBreak(&ctx, pc, operand(1), False) != Ok)
          return Error;

        if (!initedLocal(&ctx, srcVr1))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr1);
        if (!initedLocal(&ctx, srcVr2))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr2);
        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sFAbs: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(2);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sAlloc: {
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid alloc label: %t", pc, lit);

        int32 arity = lblArity(C_LBL(lit));
        int32 allocVr = operand(2);
        int32 argCnt = operand(3);

        if (argCnt != arity)
          return verifyError(&ctx, ".%d: argument count mismatch", pc);

        for (int32 ix = 0; ix < arity; ix++) {
          int32 argVr = code[pc + 3 + ix].op.ltrl;
          if (!initedLocal(&ctx, argVr))
            return verifyError(&ctx, ".%d: argument %d (local %d) not initialized", pc, ix, argVr);
        }

        if (*ctx.maxArgCnt <= argCnt)
          *ctx.maxArgCnt = argCnt;

        if (!validLocal(&ctx, allocVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, allocVr);

        initLocal(&ctx, argCnt);

        if (isLastPC(pc + argCnt + 4, limit))
          return verifyError(&ctx, ".%d: Call should not be last instruction in block", pc);
        pc += argCnt + 4;
        continue;
      }
      case sClosure: {
        int32 insSize = 4;
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return verifyError(&ctx, ".%d: invalid alloc label: %t", pc, lit);

        int32 tgtVr = operand(2);
        int32 freeVr = operand(3);

        if (!initedLocal(&ctx, freeVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, freeVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sDrop:
      case sBump: {
        int32 insSize = 2;
        int32 srcVr = operand(1);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        pc += insSize;
        continue;
      }
      case sFiber: {
        int32 insSize = 3;
        int32 tgtVr = operand(1);
        int32 srcVr = operand(2);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        if (!validLocal(&ctx, tgtVr))
          return verifyError(&ctx, "%d: result variable %d not valid", pc, tgtVr);

        initLocal(&ctx, tgtVr);
        pc += insSize;
        continue;
      }
      case sResume:
      case sSuspend:
      case sRetire: {
        int32 insSize = 3;
        int32 srcVr1 = operand(1);
        int32 srcVr2 = operand(2);
        if (!initedLocal(&ctx, srcVr1))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr1);
        if (!initedLocal(&ctx, srcVr2))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr2);
        pc += insSize;
        continue;
      }
      case sUnderflow: {
        int32 insSize = 1;
        pc += insSize;
        continue;
      }
      case sLine:
      case sdBug: {
        int32 insSize = 2;
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        pc += insSize;
        continue;
      }
      case sBind: {
        int32 insSize = 3;
        int32 constant = operand(1);
        if (!isDefinedConstant(constant))
          return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        int32 srcVr = operand(2);
        if (!initedLocal(&ctx, srcVr))
          return verifyError(&ctx, ".%d: source var %d not inited", pc, srcVr);

        pc += insSize;
        continue;
      }
      default:
        return verifyError(&ctx, ".%d: unknown instruction", pc);
    }
  }
  return Ok;
}

retCode verifyError(verifyCtxPo ctx, char *msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args; /* access the generic arguments */
  va_start(args, msg); /* start the variable argument sequence */

  __voutMsg(O_IO(f), msg, args); /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0); /* Terminate the string */

  closeIo(O_IO(f));

  strMsg(ctx->errorMsg, ctx->msgLen, RED_ESC_ON "%s%s"RED_ESC_OFF, ctx->prefix, buff);
  return Error;
}
