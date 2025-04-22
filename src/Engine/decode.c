
#include <unistd.h>
#include <hash.h>
#include "decodeP.h"
#include <globals.h>
#include "constants.h"
#include <cons.h>
#include <consP.h>
#include <stdlib.h>
#include "bignumP.h"
#include "arithP.h"
#include "charP.h"
#include "stringsP.h"
#include "signature.h"
#include "labelsP.h"
#include "closureP.h"
#include "codeP.h"
#include "libEscapes.h"

#ifdef TRACEDECODE
tracingLevel traceDecode = noTracing;
#endif

/*
 * Decode a structure from an input stream
 */

static retCode estimate(ioPo in, integer *amnt);

retCode decodeTerm(ioPo in, heapPo H, termPo *tgt, char *errorMsg, long msgSize) {
  EncodeSupport support = {errorMsg, msgSize, H};
  logical isAsynched = False;

  if (isAFile(O_OBJECT(in))) {
    filePo f = O_FILE(in);
    if (isFileAsynch(f)) {
      isAsynched = True;
      resetAccessMode(f, blocking);
    }
  }

  codePoint ch;

  again:
  switch (inChar(in, &ch)) {
    case Eof:
      return Eof;
    default:
      strMsg(errorMsg, msgSize, "stream prematurely ended");
      return Error;
    case Interrupt:
      goto again;
    case Ok: {
      if (ch != strTrm) {
        strMsg(errorMsg, msgSize, "invalid lead-in in code sequence");
        goto error_exit;
      } else {
        codePoint delim;

        switch (inChar(in, &delim)) {
          case Ok: {
            strBufferPo buffer = newStringBuffer();
            while (True) {
              switch (inChar(in, &ch)) {
                case Ok:
                  if (ch == delim) {
                    rewindStrBuffer(buffer);

                    integer amnt;

                    retCode res = estimate(O_IO(buffer), &amnt);

                    //logMsg(logFile,"Estimate of space requirements: %d heap, %d permanent",perm);


                    if (res == Ok)
                      res = reserveSpace(H, amnt);

                    if (res == Ok) {
                      rewindStrBuffer(buffer); /* re-read from string buffer */
                      strBufferPo tmpBuffer = newStringBuffer();

                      res = decode(O_IO(buffer), &support, H, tgt, tmpBuffer);

                      closeIo(O_IO(buffer));
                      closeIo(O_IO(tmpBuffer));
                    }

                    if (isAFile(O_OBJECT(in))) {
                      if (isAsynched)
                        resetAccessMode(O_FILE(in), asynch);
                    }
                    return res;
                  } else {
                    outChar(O_IO(buffer), ch);
                    continue;
                  }
                case Eof:
                  strMsg(errorMsg, msgSize, "unexpected eof");
                  goto error_exit;
                case Error:
                default:
                  strMsg(errorMsg, msgSize, "stream prematurely ended");
                  goto error_exit;
              }
            }
          }
          default:
            strMsg(errorMsg, msgSize, "stream prematurely ended");
            goto error_exit;
        }
      }
    }
  }
  error_exit:
  if (isAFile(O_OBJECT(in))) {
    if (isAsynched)
      resetAccessMode(O_FILE(in), asynch);
  }
  return Error;
}

/*
 Estimate amount of heap space needed
 */

typedef struct {
  integer amnt;
} Estimation;

static retCode nullEstimate(void *cl) {
  return Ok;
}

static retCode estimateVoid(void *cl) {
  return Ok;
}

static retCode estimateInt(integer _, void *cl) {
  return Ok;
}

static retCode estimateFlt(double dx, void *cl) {
  return Ok;
}

static retCode estimateChar(codePoint _, void *cl) {
  return Ok;
}

static retCode estimateString(char *nm, integer size, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += StringCellCount(size);
  return Ok;
}

static retCode estimateLbl(char *nm, integer arity, void *cl) {
  return Ok;
}

static retCode estimateRecLbl(char *nm, integer arity, FieldRec fields[], void *cl) {
  return Ok;
}

static retCode estimateCns(integer arity, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += arity + 1;
  return Ok;
}

static retCode endEstimateCns(void *cl) {
  return Ok;
}

static retCode estimateLst(integer count, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += count * CONS_CELLCOUNT;
  return Ok;
}

static retCode endEstimateLst(void *cl) {
  return Ok;
}

static retCode estimateClosure(void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += ClosureCellCount;
  return Ok;
}

static retCode endEstimateClosure(void *cl) {
  return Ok;
}

static retCode estimateBignum(uint32 *data, integer count, void *cl) {
  Estimation *info = (Estimation *) cl;

  info->amnt += BignumCellCount(count);
  return Ok;
}

/*
 Estimate amount of heap space needed
 */
retCode estimate(ioPo in, integer *amnt) {
  Estimation info = {0};

  DecodeCallBacks estimateCB = {
    nullEstimate,           // startDecoding
    nullEstimate,           // endDecoding
    estimateVoid,           // decVoid
    estimateInt,            // decInt
    estimateFlt,            // decFlt
    estimateLbl,            // decLbl
    estimateRecLbl,         // record label
    estimateChar,           // Character
    estimateString,         // decString
    estimateCns,            // decCon
    endEstimateCns,         // End of constructor
    estimateLst,            // Start of list
    endEstimateLst,         // End of list
    estimateClosure,        // Estimating a closure
    endEstimateClosure,
    estimateBignum,         // A big number
  };

  retCode ret = streamDecode(in, &estimateCB, &info, NULL, 0);

  if (ret == Ok) {
    *amnt = info.amnt;
  }

  return ret;
}

/*
 Warning: caller assumes responsibility for ensuring that tgt is a valid root
 */

static retCode decodeList(ioPo in, encodePo S, integer count, heapPo H, termPo *tgt, strBufferPo tmpBuffer);

retCode decode(ioPo in, encodePo S, heapPo H, termPo *tgt, strBufferPo tmpBuffer) {
  codePoint ch;
  retCode res = inChar(in, &ch);

  if (res == Eof)
    return Eof;
  switch ((starDecodeKey) ch) {
    case vodTrm: {
      *tgt = (termPo) voidEnum;
      return Ok;
    }
    case intTrm: {
      integer i;
      if ((res = decInt(in, &i)) != Ok)
        return res;
      *tgt = makeInteger(i);
      return Ok;
    }
    case bigTrm: {
      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        char *txt = getTextFromBuffer(tmpBuffer, &len);
        *tgt = bignumFromString(H, txt, len);
        return Ok;
      } else
        return res;
    }
    case fltTrm: {
      double dx;
      if ((res = decFlt(in, &dx)) != Ok)
        return res;
      *tgt = makeFloat(dx);
      return Ok;
    }
    case enuTrm: {
      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        *tgt = (termPo) declareEnum(getTextFromBuffer(tmpBuffer, &len), -1, H);
      }
      return res;
    }
    case lblTrm: {
      int32 arity;

      if ((res = decI32(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        *tgt = (termPo) declareLbl(getTextFromBuffer(tmpBuffer, &len), arity, -1);
      }
      return res;
    }
    case chrTrm: {
      codePoint cp;
      tryRet(inChar(in, &cp));
      if (cp == '\\') {
        tryRet(inChar(in, &cp));
      }
      *tgt = allocateCharacter(cp);
      return Ok;
    }
    case strTrm: {
      if ((res = decodeText(in, tmpBuffer)) == Ok) {
        integer len;
        const char *txt = getTextFromBuffer(tmpBuffer, &len);
        *tgt = (termPo) allocateString(H, txt, len);
      }
      return res;
    }
    case dtaTrm: {
      termPo lbl;
      int32 arity;

      if ((res = decI32(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decode(in, S, H, &lbl, tmpBuffer)) != Ok)
        return res;

      if (res == Ok) {
        if (lblArity(C_LBL(lbl)) != arity) {
          strMsg(S->errorMsg, S->msgSize, "invalid label arity: expecting %d", arity);
          res = Error;
        }
      }

      if (res == Ok) {
        int root = gcAddRoot(H, &lbl);
        normalPo obj = allocateStruct(H, C_LBL(lbl));
        *tgt = (termPo) (obj);

        termPo el = voidEnum;
        gcAddRoot(H, &el);
        gcAddRoot(H, (ptrPo) &obj);

        // In case of GC, we mark all the elements as void before doing any decoding
        for (integer ix = 0; ix < arity; ix++)
          setArg(obj, ix, voidEnum);

        for (integer i = 0; res == Ok && i < arity; i++) {
          res = decode(in, S, H, &el, tmpBuffer); /* read each element of term */
          if (res == Ok)
            setArg(obj, i, el);
        }

        gcReleaseRoot(H, root);
      }

      return res;
    }

    case lstTrm: {
      integer count;

      if ((res = decInt(in, &count)) != Ok) /* How many elements in the list */
        return res;

      if (res == Ok)
        res = decodeList(in, S, count, H, tgt, tmpBuffer);

      return res;
    }

    case cloTrm: {
      labelPo lbl = Null;
      termPo t = Null;
      int root = gcAddRoot(H, &t);
      gcAddRoot(H, (ptrPo) (&lbl));
      res = decode(in, S, H, &t, tmpBuffer);
      if (res != Ok || !isALabel(t))
        return Error;
      else
        lbl = C_LBL(t);
      res = decode(in, S, H, &t, tmpBuffer); /* read the free term */
      if (res == Ok)
        *tgt = (termPo) newClosure(H, lbl, t);
      char sigText[MAX_SYMB_LEN];
      res = decodeString(in, sigText, NumberOf(sigText));

      gcReleaseRoot(H, root);
      return res;
    }

    default: {
      strMsg(S->errorMsg, S->msgSize, "invalid encoding");
      return Error;
    }
  }
}

retCode decodeList(ioPo in, encodePo S, integer count, heapPo H, termPo *tgt, strBufferPo tmpBuffer) {
  if (count == 0) {
    *tgt = (termPo) nilEnum;
    return Ok;
  } else {
    retCode res = decodeList(in, S, count - 1, H, tgt, tmpBuffer);

    if (res == Ok) {
      termPo el;
      int root = gcAddRoot(H, &el);
      res = decode(in, S, H, &el, tmpBuffer); /* read each element of term */
      if (res == Ok)
        *tgt = (termPo) allocateCons(H, el, *tgt);
      gcReleaseRoot(H, root);
    }
    return res;
  }
}

retCode decodeTplCount(ioPo in, int32 *count, char *errorMsg, integer msgSize) {
  if (isLookingAt(in, "n") == Ok) {
    char nm[MAXLINE];
    int32 ar;
    retCode ret = decI32(in, count);
    if (ret == Ok) {
      ret = decodeLbl(in, nm, NumberOf(nm), &ar, errorMsg, msgSize);
      if (ret == Ok) {
        if (ar != *count) {
          strMsg(errorMsg, msgSize, "invalid tuple arity encoding");
          return Error;
        }
      }
    }
    return ret;
  } else
    return Fail;
}

// Used to support decoding
typedef struct break_level_ *breakLevelPo;

typedef struct break_level_ {
  int32 pc;
  normalPo pool;
  breakLevelPo parent;
  char *errorMsg;
  integer msgSize;
} BreakLevel;

static retCode decodeBlock(ioPo in, arrayPo ar, int32 *pc, int32 *tgt, breakLevelPo brk);
static int32 findBreak(breakLevelPo brk, int32 pc, int32 lvl);

retCode decodeInstructions(ioPo in, int32 *insCount, insPo *code, char *errorMsg, long msgSize, termPo constantPool) {
  arrayPo ar = allocArray(sizeof(Instruction), 256, True);
  BreakLevel brk = {.pc=0, .parent=Null, .pool=C_NORMAL(constantPool), .errorMsg=errorMsg, .msgSize=msgSize};
  int32 pc = 0;

  tryRet(decodeBlock(in, ar, &pc, insCount, &brk));
  *code = (insPo) malloc(sizeof(Instruction) * (size_t) *insCount);
  tryRet(copyOutData(ar, (void *) *code, sizeof(Instruction) * (size_t) *insCount));
  eraseArray(ar, Null, Null);

  return Ok;
}

static retCode decodeOp(ioPo in, OpCode *op) {
  integer val;
  retCode ret = decodeInteger(in, &val);
  *op = (OpCode) val;
  return ret;
}

static retCode decodeConstant(ioPo in, int32 *tgt, breakLevelPo brk) {
  int32 litNo;
  retCode ret = decodeI32(in, &litNo);
  if (ret == Ok) {
    if (litNo >= 0 && litNo < termArity(brk->pool)) {
      termPo literal = nthArg(brk->pool, litNo);
      *tgt = defineConstantLiteral(literal);
      return Ok;
    } else {
      strMsg(brk->errorMsg, brk->msgSize, "invalid literal number: %d not in range [0..%d)", litNo,
             termArity(brk->pool));
      return Error;
    }
  } else
    return ret;
}

static retCode decodeIns(ioPo in, arrayPo ar, int32 *pc, int32 *count, breakLevelPo brk) {
  char escNm[MAX_SYMB_LEN];
  int32 thisPc = (int32) arrayCount(ar);
  insPo ins = (insPo) newEntry(ar);
  retCode ret = Ok;

  if ((ret = decodeOp(in, &ins->op)) == Ok) {
    (*pc)++;
    (*count)--;                 // Increment decode counter
    switch (ins->op) {
#define sznOp(Tgt) {(Tgt) = 0x55555555; }
#define sztOs(Tgt) {(Tgt) = 0x7e7e7e7e; }
#define szart(Tgt) ret = decodeI32(in, &(Tgt)); (*count)--;
#define szi32(Tgt) ret = decodeI32(in, &(Tgt)); (*count)--;
#define szarg(Tgt) ret = decodeI32(in, &(Tgt)); (*count)--;
#define szlcl(Tgt) ret = decodeI32(in, &(Tgt)); (*count)--;
#define szlcs(Tgt) ret = decodeI32(in, &(Tgt)); (*count)--;
#define szsym(Tgt) ret = decodeConstant(in, &(Tgt), brk); (*count)--;
#define szlit(Tgt) ret = decodeConstant(in, &(Tgt), brk); (*count)--;
#define sztPe(Tgt) ret = decodeConstant(in, &(Tgt), brk); (*count)--;
#define szEs(Tgt) if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); (Tgt) = lookupEscape(escNm);} (*count)--;
#define szglb(Tgt) {retCode ret = decodeString(in,escNm,NumberOf(escNm)); (Tgt) = globalVarNo(escNm);} (*count)--;
#define szbLk(Tgt) { int32 offset; ret = decodeBlock(in, ar,  pc, &offset, brk);   \
                    (*count)--;                                                    \
                    ins = (insPo)nthEntry(ar,thisPc);                              \
                    (Tgt) = offset;}
#define szlVl(Tgt) { int32 lvl; ret = decodeI32(in, &lvl); (Tgt) = findBreak(brk, *pc, lvl); (*count)--; }

#define instruction(Op, A1, A2, Dl, _, Cmt)\
      case Op:{                             \
        sz##A1(ins->fst)                   \
        sz##A2(ins->alt)                   \
        break;                             \
      }

#include "instructions.h"

#undef instruction
#undef szi32
#undef szart
#undef szarg
#undef szlcl
#undef szlcs
#undef szbLk
#undef szlVl
#undef szsym
#undef szEs
#undef szlit
#undef sztPe
#undef szglb
#undef sznOp
#undef sztOs
      default: {
        strMsg(brk->errorMsg, brk->msgSize, "invalid instruction encoding");
        return Error;
      }
    }
  }
  return ret;
}

static retCode decodeI(ioPo in, arrayPo ar, int32 *pc, int32 *count, breakLevelPo brk){
  int32 thisPc = (int32) arrayCount(ar);
  insPo ins = (insPo) newEntry(ar);
  retCode ret = Ok;

  if ((ret = decodeOp(in, &ins->op)) == Ok) {
    (*pc)++;
    (*count)--;                 // Increment decode counter
    switch (ins->op) {
  case Halt:
  {
    ret = decodeI32(in,&ins->fst);
    (*count)--;
    return ret;
  }
      case Nop:{
      return Ok;
    }

  case Abort: {
    return Ok;
  }
  case Call: {
    ret = decodeConstant(in, &ins->fst, brk); (*count)--;
    return ret;
  }
  case TCall: {
    ret = decodeConstant(in, &ins->fst, brk); (*count)--;
    return ret;
  }
  case OCall: {
    ret = decodeI32(in,&ins->fst);
    (*count)--;
    return ret;
  }
  case TOCall: {
    ret = decodeI32(in,&ins->fst);
    (*count)--;
    return ret;
  }
  case Escape: {
    char escNm[MAX_SYMB_LEN];


ret = decodeString(in,escNm,NumberOf(escNm)); ins->fst =   (int32)lookupEscape(escNm); (*count)--;
    return ret;
  }

  case Entry:{
    ret = decodeI32(in,&ins->fst);
    (*count)--;
    return ret;
  }

  case Ret: {
    return Ok;
  }
  case Block: {
    ret = decodeConstant(in, &ins->fst, brk); (*count)--;

    int32 offset; ret = decodeBlock(in, ar,  pc, &offset, brk);
                    (*count)--;
                    ins = (insPo)nthEntry(ar,thisPc);
                    ins->alt = offset;

                    return ret;
  }

  case Loop: {
    if (!isLastPC(pc, limit))
      return verifyError(&ctx, ".%d: Loop should be last instruction in block", pc);

    if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
      return Error;
    return Ok;
  }
  case Break: {
    if (!isLastPC(pc, limit))
      return verifyError(&ctx, ".%d: Break should be last instruction in block", pc);

    if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
      return Error;
    return Ok;
  }
  case Result: {
    if (!isLastPC(pc, limit))
      return verifyError(&ctx, ".%d: Result should be last instruction in block", pc);

    if (stackDepth < 1)
      return verifyError(&ctx, ".%d: Result should leave at least one value on stack", pc);

    int32 depth = code[pc].fst;
    if (ctxDepth(ctx.parent, stackDepth) < depth)
      return verifyError(&ctx, ".%d: insufficient stack depth for stack Result %d", pc, depth);
    stackDepth = depth - ctxDepth(ctx.parent, stackDepth);

    if (checkBreak(ctx.parent, pc, pc + code[pc].alt + 1, stackDepth+1, False) != Ok)
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
    if (ctxDepth(ctx.parent, stackDepth) < depth)
      return verifyError(&ctx, ".%d: insufficient stack depth for stack reset %d", pc, depth);
    stackDepth = depth - ctxDepth(ctx.parent, stackDepth);
    if (stackDepth < 0)
      return verifyError(&ctx, ".%d: insufficient block stack depth for stack reset %d", pc, depth);
    pc++;
    continue;
  }
  case Pick: {
    int32 count = code[pc].fst;
    int32 keep = code[pc].alt;

    if (ctxDepth(ctx.parent, stackDepth) < count || ctxDepth(ctx.parent, stackDepth) < keep)
      return verifyError(&ctx, ".%d: insufficient stack depth for stack keep %d of %d", pc, keep, count);

    if (keep > count)
      return verifyError(&ctx, ".%d: trying to keep more elements (%d) than depth (%d) ", pc, keep, count);

    stackDepth = count - ctxDepth(ctx.parent, stackDepth);
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

  case Try: {
    int32 constant = code[pc].fst;
    if (!isDefinedConstant(constant))
      return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

    termPo lit = getConstant(constant);
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
    int32 key = code[pc].fst;
    if (!isDefinedConstant(key))
      return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, key);
    if (stackDepth < 1)
      return verifyError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
    stackDepth--;
    if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, stackDepth, False) != Ok)
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
    int32 constant = code[pc].fst;
    if (!isDefinedConstant(constant))
      return verifyError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

    termPo frameLit = getConstant(constant);
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

retCode decodeBlock(ioPo in, arrayPo ar, int32 *pc, int32 *tgt, breakLevelPo brk) {
  BreakLevel blkBrk = {.pc=(*pc), .parent=brk, .pool=brk->pool, .errorMsg=brk->errorMsg, .msgSize=brk->msgSize};
  int32 count;

  retCode ret = decodeTplCount(in, &count, brk->errorMsg, brk->msgSize);

  if (ret == Ok) {
    while (ret == Ok && count > 0) {
      ret = decodeIns(in, ar, pc, &count, &blkBrk);
    }

    *tgt = *pc - blkBrk.pc;
  }
  return ret;
}

int32 findBreak(breakLevelPo brk, int32 pc, int32 lvl) {
  for (int l = 1; l < lvl && brk != Null; l++)
    brk = brk->parent;
  if (brk != Null) {
    return brk->pc - pc - 1;
  } else
    return 0;
}

