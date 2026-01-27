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

retCode decodeSSAInstructions(ioPo in, int32 *codeCount, ssaInsPo *code, char *errorMsg, long msgSize,
                              termPo constantPool) {
  arrayPo ar = allocArray(sizeof(int32), 256, True);
  BreakLevel brk = {.pc = 0, .parent = Null, .pool = C_NORMAL(constantPool), .errorMsg = errorMsg, .msgSize = msgSize};
  int32 pc = 0;

  tryRet(decodeBlock(in, ar, &pc, codeCount, &brk));
  *code = (ssaInsPo) malloc(sizeof(int32) * (size_t) *codeCount);
  tryRet(copyOutData(ar, (void *) *code, sizeof(int32) * (size_t) *codeCount));
  eraseArray(ar, Null, Null);

  return Ok;
}

static retCode decodeOp(ioPo in, ssaOp *op) {
  integer val;
  retCode ret = decodeInteger(in, &val);
  if (ret == Ok) {
    if (val >= 0 && val < maxCode) {
      *op = (ssaOp) val;
      return ret;
    }
  }
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

static retCode decodeOperands(ioPo in, arrayPo ar, int32 *pc, int32 *count, breakLevelPo brk, char *mt);

static retCode decodeIns(ioPo in, arrayPo ar, int32 *pc, int32 *count, breakLevelPo brk) {
  retCode ret = Ok;

  ssaOp op;
  if ((ret = decodeOp(in, &op)) == Ok) {
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
  case M:\
      ret = decodeOperands(in, ar, pc, count, brk, Fmt);

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

retCode decodeOperands(ioPo in, arrayPo ar, int32 *pc, int32 *count, breakLevelPo brk, char *fmt) {
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
        retCode ret = decodeBlock(in, ar, pc, count, brk);
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
          int32 lvlPc = findBreak(brk, *pc, lvl);
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

retCode decodeBlock(ioPo in, arrayPo ar, int32 *pc, int32 *tgt, breakLevelPo brk) {
  BreakLevel blkBrk = {
    .pc = (*pc), .parent = brk, .pool = brk->pool, .errorMsg = brk->errorMsg, .msgSize = brk->msgSize
  };
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
