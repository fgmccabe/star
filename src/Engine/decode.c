
#include <unistd.h>
#include <hash.h>
#include "decodeP.h"
#include <globals.h>
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
#include "array.h"
#include "codeP.h"

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
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
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
      integer arity;

      if ((res = decInt(in, &arity)) != Ok) /* How many arguments in the class */
        return res;

      if ((res = decode(in, S, H, &lbl, tmpBuffer)) != Ok)
        return res;

      if (res == Ok) {
        if (labelArity(C_LBL(lbl)) != arity) {
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

retCode decodeTplCount(ioPo in, integer *count, char *errorMsg, integer msgSize) {
  if (isLookingAt(in, "n") == Ok) {
    char nm[MAXLINE];
    integer ar;
    retCode ret = decInt(in, count);
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
  arrayPo locs;
  integer pc;
  breakLevelPo parent;
} BreakLevel;

static void recordLoc(breakLevelPo brk, integer pc, int32 litNo){
  arrayPo ar = brk->locs;
  MethodLoc loc = {.pc=pc, .litNo=litNo};
  appendEntry(ar,&loc);
}

static retCode
decodeBlock(ioPo in, arrayPo ar, integer *next, breakLevelPo brk, char *errorMsg, long msgSize);
static integer findBreak(breakLevelPo brk, integer pc, int32 lvl);

static comparison byPc(arrayPo ar, integer ix, integer iy, void *cl) {
  methodLocPo sx = (methodLocPo) nthEntry(ar, ix);
  methodLocPo sy = (methodLocPo) nthEntry(ar, iy);
  return ixCmp(sx->pc, sy->pc);
}

retCode decodeInstructions(ioPo in, integer *insCount, insPo *code, arrayPo *locs, char *errorMsg, long msgSize) {
  arrayPo ar = allocArray(sizeof(Instruction), 256, True);
  arrayPo lcs = allocArray(sizeof(MethodLoc), 16, True);
  BreakLevel brk = {.locs=lcs,.pc=0,.parent=Null};

  tryRet(decodeBlock(in, ar, insCount, &brk, errorMsg, msgSize));
  tryRet(copyOutData(ar, (void *) code, sizeof(Instruction) * (size_t) insCount));
  eraseArray(ar, Null, Null);

  if(arrayCount(lcs)>0){
    sortArray(lcs,byPc,Null); // sort by pc, to make searching for location easier
    *locs = lcs;
  } else{
    *locs = Null;
    eraseArray(lcs,Null,Null);
  }

  return Ok;
}

static retCode decodeI32(ioPo in, int32 *rest) {
  integer val;
  retCode ret = decodeInteger(in, &val);
  *rest = (int32)val;
  return ret;
}

static retCode
decodeIns(ioPo in, arrayPo ar, integer pc, integer *nextLoc, breakLevelPo brk, char *errorMsg, long msgSize) {
  integer and;
  char escNm[MAX_SYMB_LEN];
  insPo ins = (insPo) nthEntry(ar, pc);
  retCode ret = Ok;

  if (decodeInteger(in, (integer *) &ins->op) == Ok) {
    switch (ins->op) {
#define sznOp
#define sztOs
#define szart tryRet(decodeI32(in, &ins->fst));
#define szi32 tryRet(decodeI32(in, &ins->fst));
#define szarg tryRet(decodeI32(in, &ins->fst));
#define szlcl tryRet(decodeI32(in, &ins->fst));
#define szlcs tryRet(decodeI32(in, &ins->fst));
#define szsym tryRet(decodeI32(in, &ins->fst));
#define szEs if(ret==Ok){ret = decodeString(in,escNm,NumberOf(escNm)); ins->fst = lookupEscape(escNm);}
#define szlit tryRet(decodeI32(in, &ins->fst));
#define sztPe tryRet(decodeI32(in, &ins->fst));
#define szglb {retCode ret = decodeString(in,escNm,NumberOf(escNm)); ins->fst = globalVarNo(escNm);}
#define szbLk { ins->alt = *nextLoc; BreakLevel blkBrk = {.pc=pc,.parent=brk, .locs=parent->locs}; \
                    tryRet(decodeBlock(in, *nextLoc, nextLock, blkBrk, errorMsg, msgsize)); \
                    }
#define szlVl { int32 lvl; tryRet(decodeI32(in, &lvl)); ins->fst = findBreak(brk,pc, lvl); }
#define szlNe { tryRet(decodeI32(in, &ins->fst)); recordLoc(brk,pc,ins->fst);}

#define instruction(Op, A1, A2, Dl, Tp, Cmt)\
      case Op:{                             \
        sz##A1                          \
        sz##A2                          \
        break;                              \
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
#undef szlNe
      default: {
        strMsg(errorMsg, msgSize, "invalid instruction encoding");
        return Error;
      }
    }
  }
  setNth(ar, pc, (void *) &ins);
  return Ok;
}

retCode
decodeBlock(ioPo in, arrayPo ar, integer *next, breakLevelPo brk, char *errorMsg, long msgSize) {
  integer insCount;
  retCode ret = decodeTplCount(in, &insCount, errorMsg, msgSize);

  if (ret == Ok) {
    ret = reserveRoom(ar, *next + insCount);
    if (ret != Ok) {
      strMsg(errorMsg, msgSize, "could not allocate space for %d instructions in code", insCount);
      return Error;
    } else {
      integer nextLoc = *next;
      *next += insCount;
      for (integer ix = 0; ret == Ok && ix < insCount;) {
        ret = decodeIns(in, ar, nextLoc++, next, brk, errorMsg, msgSize);
      }
      return ret;
    }
  }
  return ret;
}

integer findBreak(breakLevelPo brk, integer pc, int32 lvl) {
  for (int l = 0; l < lvl && brk != Null; l++)
    brk = brk->parent;
  if (brk != Null) {
    return brk->pc-pc;
  } else
    return 0;
}

