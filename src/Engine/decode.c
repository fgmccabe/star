
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

static retCode decodeSigHeight(ioPo in, int32 *tgt, breakLevelPo brk) {
  integer litNo;
  retCode ret = decodeInteger(in, &litNo);
  if (ret == Ok) {
    if (litNo >= 0 && litNo < termArity(brk->pool)) {
      termPo literal = nthArg(brk->pool, litNo);
      if(isString(literal)){
        integer sigLen;
        integer arity, returns;
        const char *sig = strVal(literal, &sigLen);
        funSigArity(sig, sigLen, &arity);
      }
      else{
        strMsg(brk->errorMsg, brk->msgSize, "invalid literal: %T not a string", literal);
        return Error;
      }
      *tgt = defineConstantLiteral(literal);
      return Ok;
    } else {
      strMsg(brk->errorMsg, brk->msgSize, "invalid literal number: %d not in range [0..%d)", litNo,
             termArity(brk->pool));
      return Error;
    }
  } else
    return ret;
  *tgt = (int32) val;
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

static retCode decodeI(ioPo in, arrayPo ar, int32 *pc, int32 *count, breakLevelPo brk) {
  int32 thisPc = (int32) arrayCount(ar);
  insPo ins = (insPo) newEntry(ar);
  retCode ret = Ok;

  if ((ret = decodeOp(in, &ins->op)) == Ok) {
    (*pc)++;
    (*count)--;                 // Increment decode counter
    switch (ins->op) {
      case Halt: {
        ret = decodeI32(in, &ins->fst);
        (*count)--;
        return ret;
      }
      case Nop:
      case Abort: {
        return Ok;
      }
      case Call:
      case TCall: {
        ret = decodeConstant(in, &ins->fst, brk);
        (*count)--;
        return ret;
      }
      case OCall: {
        ret = decodeI32(in, &ins->fst);
        (*count)--;
        return ret;
      }
      case TOCall: {
        ret = decodeI32(in, &ins->fst);
        (*count)--;
        return ret;
      }
      case Escape: {
        char escNm[MAX_SYMB_LEN];

        ret = decodeString(in, escNm, NumberOf(escNm));
        ins->fst = (int32) lookupEscape(escNm);
        (*count)--;
        return ret;
      }

      case Entry: {
        ret = decodeI32(in, &ins->fst);
        (*count)--;
        return ret;
      }

      case Ret: {
        return Ok;
      }
      case Block: {
        (*count) -= 2;

        ret = decodeConstant(in, &ins->fst, brk);
        if (ret == Ok)
          ret = decodeBlock(in, ar, pc, &ins->alt, brk);
        return ret;
      }

      case Loop:
      case Break: {
        (*count)--;
        return decodeI32(in, &ins->alt);
      }
      case Result: {
        (*count) -= 2;
        ret = decodeI32(in, &ins->fst);
        if (ret == Ok)
          ret = decodeI32(in, &ins->alt);
        return ret;
      }

      case Drop:
      case Dup: {
        return Ok;
      }

      case Rot:
      case Rst: {
        (*count)--;
        return decodeI32(in, &ins->fst);
      }
      case Pick: {
        (*count) -= 2;
        ret = decodeI32(in, &ins->fst);
        if (ret == Ok)
          ret = decodeI32(in, &ins->alt);
        return ret;
      }
      case Fiber:
      case Resume:
      case Suspend:
      case Retire: {
        return Ok;
      }
      case Underflow:
        return Ok;
      case Try: {
        ret = decodeConstant(in, &ins->fst, brk);
        (*count)--;

        int32 offset;
        ret = decodeBlock(in, ar, pc, &offset, brk);
        (*count)--;
        ins = (insPo) nthEntry(ar, thisPc);
        ins->alt = offset;

        return ret;
      }
      case EndTry:
      case TryRslt: {
        (*count)--;
        return decodeI32(in, &ins->alt);
      }
      case Throw:
        return Ok;
      case LdV:
        return Ok;
      case LdC: {
        (*count)--;
        return decodeConstant(in, &ins->fst, brk);
      }
      case LdA:
      case LdL:
      case StL:
      case StV:
      case TL: {
        (*count)--;
        return decodeI32(in, &ins->fst);
      }
      case LdG:
      case StG:
      case TG: {
        char glbNm[MAX_SYMB_LEN];

        (*count)--;

        ret = decodeString(in, glbNm, NumberOf(glbNm));
        ins->fst = globalVarNo(glbNm);
      }
      case Sav:
      case TstSav:
      case StSav:
      case TSav:
        return Ok;
      case LdSav: {
        (*count)--;
        return decodeI32(in, &ins->alt);
      }
      case Cell:
      case Get:
      case Assign:
        return Ok;
      case CLit:
      case CLbl: {
        (*count) -= 2;
        ret = decodeConstant(in, &ins->fst, brk);
        if (ret == Ok)
          ret = decodeI32(in, &ins->alt);
        return ret;
      }
      case Nth:
      case StNth: {
        (*count)--;
        return decodeI32(in, &ins->fst);
      }
      case If:
      case IfNot: {
        (*count)--;
        return decodeI32(in, &ins->alt);
      }
      case Case:
      case IndxJmp: {
        (*count)--;
        return decodeI32(in, &ins->fst);
      }
      case IAdd:
      case ISub:
      case IMul:
        return Ok;
      case IDiv:
      case IMod:
        return Ok;

        /*   { */
        /*       (*count)--; */
        /* return decodeI32(in,&ins->alt); */
        /* } */
      case IAbs:
      case IEq:
      case ILt:
      case IGe:
        return Ok;
      case Cmp:
      case ICmp:
      case CCmp:
      case FCmp: {
        (*count)--;
        return decodeI32(in, &ins->alt);
      }
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
        return Ok;
      case FAdd:
      case FSub:
      case FMul:
        return Ok;
      case FDiv:
      case FMod:
        return Ok;
      case FAbs:
      case FEq:
      case FLt:
      case FGe:
        return Ok;
      case Alloc:
      case Closure:
      case Frame: {
        (*count)--;
        return decodeConstant(in, &ins->fst, brk);
      }
      case dBug:
        return Ok;
      default:
        return Error;
    }
  }
  return ret;
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

