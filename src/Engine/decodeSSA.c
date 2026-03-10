#include <unistd.h>
#include "decode.h"
#include <globals.h>
#include "constants.h"
#include <consP.h>
#include <stdlib.h>
#include "arithP.h"
#include "signature.h"
#include "codeP.h"
#include "escapeP.h"
#include "ssaOps.h"
#include "pkgP.h"
#include "stackP.h"
#include "verify.h"
#include "verifyP.h"

// Used to support decoding
typedef struct break_level_ *breakLevelPo;

typedef struct break_level_ {
  int32 pc;
  normalPo pool;
  breakLevelPo parent;
  char *errorMsg;
  integer msgSize;
} BreakLevel;

static retCode decodeBlock(ioPo in, arrayPo ar, int32 *pc, int32 *tgt, breakLevelPo brk, int32 *stackHeight);
static int32 findBreak(breakLevelPo brk, int32 pc, int32 lvl);

static retCode decodeInstructions(ioPo in, int32 *codeCount, ssaInsPo *code, char *errorMsg, long msgSize,
                                  termPo constantPool, int32 *stackHeight) {
  arrayPo ar = allocArray(sizeof(int32), 256, True);
  BreakLevel brk = {.pc = 0, .parent = Null, .pool = C_NORMAL(constantPool), .errorMsg = errorMsg, .msgSize = msgSize};
  int32 pc = 0;

  tryRet(decodeBlock(in, ar, &pc, codeCount, &brk, stackHeight));
  *code = (ssaInsPo) malloc(sizeof(int32) * (size_t) *codeCount);
  tryRet(copyOutData(ar, (void *) *code, sizeof(int32) * (size_t) *codeCount));
  eraseArray(ar, Null, Null);

  return Ok;
}

static retCode decodeOp(ioPo in, breakLevelPo brk, ssaOp *op) {
  integer val;
  retCode ret = decodeInteger(in, &val);
  if (ret == Ok) {
    if (val >= 0 && val < maxCode) {
      *op = val;
      return ret;
    }
  }
  strMsg(brk->errorMsg, brk->msgSize, "invalid opcode: %d", (int32) val);
  return Error;
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

static retCode decodeOperands(ioPo in, arrayPo ar, int32 *pc, int32 *count, int32 *stackHeight, breakLevelPo brk,
                              char *mt);

static retCode decodeIns(ioPo in, arrayPo ar, int32 *pc, int32 *count, int32 *stackHeight, breakLevelPo brk) {
  retCode ret = Ok;

  ssaOp op;
  if ((ret = decodeOp(in, brk, &op)) == Ok) {
    (*pc)++;
    appendEntry(ar, &op);
    (*count)--; // Increment decode counter
    switch (op) {
#undef instr
#define sym "s"
#define lcl "v"
#define lcls "V"
#define lit "l"
#define glb "g"
#define art "a"
#define i32 "i"
#define Es "e"
#define bLk "k"
#define lVl "b"
#define none ""
#define instr(M, Fmt) \
  case s##M:\
      ret = decodeOperands(in, ar, pc, count, stackHeight, brk, Fmt); \
      break;

#include "ssaInstructions.h"

      default: {
        strMsg(brk->errorMsg, brk->msgSize, "invalid instruction");
        return Error;
      }
    }
  }
  return ret;
}

#undef sym
#undef lcl
#undef lcls
#undef lit
#undef glb
#undef art
#undef i32
#undef Es
#undef bLk
#undef lVl

#define sym 's'
#define lcl 'v'
#define lcls 'V'
#define lit 'l'
#define glb 'g'
#define art 'a'
#define i32 'i'
#define Es 'e'
#define bLk 'k'
#define lVl 'b'

retCode decodeOperands(ioPo in, arrayPo ar, int32 *pc, int32 *count, int32 *stackHeight, breakLevelPo brk, char *fmt) {
  int32 basePc = (*pc) - 1;
  while (*fmt != '\0') {
    switch (*fmt++) {
      case sym:
      case lit: {
        int32 litNo;
        retCode ret = decodeConstant(in, &litNo, brk);
        if (ret == Ok) {
          appendEntry(ar, &litNo);
          (*pc)++;
          (*count)--;
          continue;
        }
        return ret;
      }
      case lcl:
      case art:
      case i32: {
        int32 vNo;
        retCode ret = decodeI32(in, &vNo);
        if (ret == Ok) {
          appendEntry(ar, &vNo);
          (*pc)++;
          (*count)--;
          continue;
        }
        return ret;
      }
      case lcls: {
        int32 vrCount;
        retCode ret = decodeTplCount(in, &vrCount, brk->errorMsg, brk->msgSize);
        if (ret == Ok) {
          appendEntry(ar, &vrCount);
          (*pc)++;
          (*count)--;

          for (int32 ix = 0; ix < vrCount; ix++) {
            int32 vNo;
            ret = decodeI32(in, &vNo);
            if (ret == Ok) {
              appendEntry(ar, &vNo);
              (*pc)++;
              (*count)--;
            } else {
              strMsg(brk->errorMsg, brk->msgSize, "invalid variable number");
              return ret;
            }
          }
          continue;
        }
        if (*stackHeight < vrCount)
          *stackHeight = vrCount;
        return ret;
      }
      case glb: {
        char glbNm[MAX_SYMB_LEN];
        retCode ret = decodeString(in, glbNm,NumberOf(glbNm));
        if (ret == Ok) {
          int32 glbNo = (int32) globalVarNo(glbNm);
          appendEntry(ar, &glbNo);
          (*pc)++;
          (*count)--;
          continue;
        }
        return ret;
      }
      case Es: {
        char escNm[MAX_SYMB_LEN];
        retCode ret = decodeString(in, escNm,NumberOf(escNm));
        if (ret == Ok) {
          int32 escNo = (int32) lookupEscape(escNm);
          appendEntry(ar, &escNo);
          (*pc)++;
          (*count)--;
          continue;
        }
        return ret;
      }
      case bLk: {
        int32 thisPc = *pc;
        int32 *fwd = newEntry(ar);
        appendEntry(ar, &thisPc);
        (*pc)++;
        (*count)--;
        retCode ret = decodeBlock(in, ar, pc, count, brk, stackHeight);
        if (ret == Ok) {
          *fwd = *pc;
          continue;
        }
        return ret;
      }
      case lVl: {
        int32 lvl;
        retCode ret = decodeI32(in, &lvl);
        if (ret == Ok) {
          int32 lvlPc = findBreak(brk, basePc, lvl);
          appendEntry(ar, &lvlPc);
          (*pc)++;
          (*count)--;
          continue;
        }
        return ret;
      }
      default:
        strMsg(brk->errorMsg, brk->msgSize, "invalid instruction operand");
        return Error;
    }
  }
  return Ok;
}

retCode decodeBlock(ioPo in, arrayPo ar, int32 *pc, int32 *tgt, breakLevelPo brk, int32 *stackHeight) {
  BreakLevel blkBrk = {
    .pc = (*pc), .parent = brk, .pool = brk->pool, .errorMsg = brk->errorMsg, .msgSize = brk->msgSize
  };
  int32 count;

  retCode ret = decodeTplCount(in, &count, brk->errorMsg, brk->msgSize);

  if (ret == Ok) {
    while (ret == Ok && count > 0) {
      ret = decodeIns(in, ar, pc, &count, stackHeight, &blkBrk);
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

retCode decodePolicies(ioPo in, heapPo H, DefinitionMode *redefine, char *errorMsg, long msgSize) {
  int32 policyCount;
  retCode ret = decodeTplCount(in, &policyCount, errorMsg, msgSize);
  for (integer ix = 0; ret == Ok && ix < policyCount; ix++) {
    char nameBuff[MAX_SYMB_LEN];
    if (decodeString(in, nameBuff, NumberOf(nameBuff)) == Ok) {
      if (uniIsLit(nameBuff, "soft")) {
        *redefine = softDef;
      } else; // ignore unknown policies
    } else {
      strMsg(errorMsg, msgSize, "problem in loading policy");
      return Error;
    }
  }
  return ret;
}

retCode loadCode(ioPo in, heapPo H, packagePo owner, char *errorMsg, long msgSize) {
  char prgName[MAX_SYMB_LEN];
  int32 arity;
  int32 lclCount = 0;
  int32 sigIndex;
  DefinitionMode redefine = hardDef;

  retCode ret = decodeLbl(in, prgName, NumberOf(prgName), &arity, errorMsg, msgSize);

#ifdef TRACEPKG
  if (tracePkg >= detailedTracing)
    logMsg(logFile, "loading function %s/%d", &prgName, arity);
#endif

  if (ret == Ok)
    ret = decodePolicies(in, H, &redefine, errorMsg, msgSize);

  if (ret == Ok)
    ret = decodeI32(in, &sigIndex);

  if (ret == Ok)
    ret = decodeI32(in, &lclCount);

  if (ret == Ok) {
    termPo pool = voidEnum;
    int root = gcAddRoot(H, &pool);
    EncodeSupport support = {errorMsg, msgSize, H};
    strBufferPo tmpBuffer = newStringBuffer();

    ret = decode(in, &support, H, &pool, tmpBuffer);

    if (ret == Ok) {
      int32 insCount = 0;
      ssaInsPo instructions = Null;
      int32 stackHeight = 0;
      ret = decodeInstructions(in, &insCount, &instructions, errorMsg, msgSize, pool, &stackHeight);

      if (ret == Ok) {
        termPo locals = voidEnum;
        gcAddRoot(H, &locals);
        ret = decode(in, &support, H, &locals, tmpBuffer);

        if (ret == Ok) {
          labelPo lbl = declareLbl(prgName, arity, -1);

          if (labelMtd(lbl) != Null) {
            if (redefine != softDef) {
              strMsg(errorMsg, msgSize, "attempt to redeclare method %A", lbl);
              ret = Error;
            } // Otherwise don't redefine
          } else {
            gcAddRoot(H, (ptrPo) &lbl);

            methodPo mtd = defineMtd(H, insCount, instructions, lclCount,
                                     stackHeight + lclCount + (int32) FrameCellCount, lbl);
            if (enableVerify)
              ret = verifyMethod(mtd, prgName, errorMsg, msgSize);

#ifndef NOJIT
            if (ret == Ok && jitOnLoad)
              ret = jitMethod(mtd, errorMsg, msgSize);
#endif
          }
        }
      }
      gcReleaseRoot(H, root);
    }
  }

  if (ret == Error)
    logMsg(logFile, "problem in loading %s/%d: %s", prgName, arity, errorMsg);

  return ret;
}
