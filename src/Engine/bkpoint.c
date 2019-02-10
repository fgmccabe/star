//
// Created by Francis McCabe on 5/25/18.
//

#include "bkpoint.h"
#include <str.h>
#include <arith.h>
#include <labels.h>

typedef struct _break_point_ {
  BreakPtType bkType;
  char nm[MAX_SYMB_LEN];
  integer lineNo;
  integer offset;
} BreakPoint;

static BreakPoint breakPoints[10];
static int breakPointCount = 0;

retCode addBreakPoint(breakPointPo bp) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (!breakPointInUse(&breakPoints[ix])) {
      breakPoints[ix] = *bp;
      return Ok;
    }
  }
  if (breakPointCount < NumberOf(breakPoints)) {
    breakPoints[breakPointCount++] = *bp;
    return Ok;
  } else
    return Fail;
}

logical sameBreakPoint(breakPointPo b1, breakPointPo b2) {
  if (b1->bkType == b2->bkType) {
    switch (b1->bkType) {
      case lineBreak:
        return (logical) (uniCmp(b1->nm, b2->nm) == same && b1->lineNo == b2->lineNo && b1->offset == b2->offset);
      case callBreak:
        return (logical) (uniCmp(b1->nm, b2->nm) == same && b1->offset == b2->offset);
    }
  }
  return False;
}

logical isBreakPoint(breakPointPo b, const char *pk, integer lineNo, integer offset) {
  if (b->bkType == lineBreak && uniCmp(b->nm, pk) == same && b->lineNo == lineNo) {
    if (offset != -1)
      return (logical) (b->offset == -1 || b->offset == offset);
    else
      return True;
  }
  return False;
}

logical isCallBreakPoint(breakPointPo b, const char *nm, integer arity) {
  if (b->lineNo >= 0)
    return (logical) (b->bkType == callBreak && uniCmp(b->nm, nm) == same && b->lineNo == arity);
  else
    return (logical) (b->bkType == callBreak && uniCmp(b->nm, nm) == same);
}

retCode isValidBreakPoint(breakPointPo b) {
  switch (b->bkType) {
    case callBreak: {
      labelPo lbl = findLbl(b->nm, b->lineNo);
      return lbl != Null ? Ok : Error;
    }
    case lineBreak:
      return Ok;
    default:
      return Error;
  }
}

logical lineBreakPointHit(normalPo loc) {
  char pkgNm[MAX_SYMB_LEN];

  copyString2Buff(C_STR(nthArg(loc, 0)), pkgNm, NumberOf(pkgNm));
  integer lineNo = integerVal(nthArg(loc, 1));
  integer offset = integerVal(nthArg(loc, 2));

  for (int ix = 0; ix < breakPointCount; ix++) {
    if (isBreakPoint(&breakPoints[ix], pkgNm, lineNo, offset))
      return True;
  }
  return False;
}

logical callBreakPointHit(labelPo lbl) {
  char *const lblNm = labelName(lbl);
  const integer lblArity = labelArity(lbl);

  for (int ix = 0; ix < breakPointCount; ix++) {
    if (isCallBreakPoint(&breakPoints[ix], lblNm, lblArity))
      return True;
  }
  return False;
}

logical breakPointInUse(breakPointPo b) {
  return (logical) (b->lineNo >= 0);
}

void markBpOutOfUse(breakPointPo b) {
  b->lineNo = -1;
}

retCode clearBreakPoint(breakPointPo bp) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (sameBreakPoint(bp, &breakPoints[ix])) {
      if (ix == breakPointCount - 1) {
        breakPointCount--;
        while (!breakPointInUse(&breakPoints[ix]))
          breakPointCount--;
        return Ok;
      } else {
        markBpOutOfUse(&breakPoints[ix]);
        return Ok;
      }
    }
  }

  return Fail;
}

/*
 * A Break point is specified as:
 * pkg:line,
 * pkg:line:off, or
 * prg/arity
 */

retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp) {
  integer b = 0;
  integer ix = 0;

  integer line = -1;
  integer offset = -1;
  BreakPtType bkType = lineBreak;

  enum {
    initSte,
    inNme,
    inLine,
    inOffset,
    inArity
  } pState = initSte;

  while (ix < bLen && buffer[ix] == ' ')
    ix++;

  if (codePointAt(buffer, ix, bLen) == (codePoint) '\'') {
    codePoint delim = nextCodePoint(buffer, &ix, bLen);
    pState = inNme;

    while (ix < bLen) {
      codePoint cp = nextCodePoint(buffer, &ix, bLen);

      if (cp != delim) {
        appendCodePoint(bp->nm, &b, NumberOf(bp->nm), cp);
      } else {
        appendCodePoint(bp->nm, &b, NumberOf(bp->nm), 0);
        break;
      }
    }
  }

  while (ix < bLen) {
    codePoint cp = nextCodePoint(buffer, &ix, bLen);
    switch (cp) {
      case '\n':
      case 0:
        appendCodePoint(bp->nm, &b, NumberOf(bp->nm), 0);
        bp->lineNo = line;
        bp->offset = offset;
        bp->bkType = bkType;
        return Ok;
      case ':': {
        switch (pState) {
          case inNme:
            appendCodePoint(bp->nm, &b, NumberOf(bp->nm), 0);
            pState = inLine;
            line = 0;
            continue;
          case inLine:
            pState = inOffset;
            offset = 0;
            continue;
          default:
            outMsg(logFile, "invalid break point: %S\n", buffer, bLen);
            return Error;
        }
      }
      case '/': {
        switch (pState) {
          case inNme:
            appendCodePoint(bp->nm, &b, NumberOf(bp->nm), 0);
            pState = inArity;
            bkType = callBreak;
            line = offset = 0;
            continue;
          default:
            outMsg(logFile, "invalid break point: %S\n", buffer, bLen);
            return Error;
        }
      }
      case ' ':
        switch (pState) {
          case initSte:
            continue;
          case inNme:
            appendCodePoint(bp->nm, &b, NumberOf(bp->nm), 0);
            pState = inLine;
            line = offset = 0;
            continue;
          case inLine:
            pState = inOffset;
            continue;
          case inOffset:
          case inArity:
            continue;
        }
      default:
        switch (pState) {
          case inNme:
            appendCodePoint(bp->nm, &b, NumberOf(bp->nm), cp);
            continue;
          case inLine:
          case inArity:
            if (isNdChar(cp)) {
              line = line * 10 + digitValue(cp);
              continue;
            } else {
              outMsg(logFile, "invalid break point line number: %S\n", buffer, bLen);
              return Error;
            }
          case inOffset:
            if (isNdChar(cp)) {
              offset = offset * 10 + digitValue(cp);
              continue;
            } else if (cp == (codePoint) '*') {
              offset = -1;
              continue;
            } else {
              outMsg(logFile, "invalid break point line offset: %S\n", buffer, bLen);
              return Error;
            }
          case initSte:
            if (!isSpaceChar(cp)) {
              pState = inNme;
              appendCodePoint(bp->nm, &b, NumberOf(bp->nm), cp);
            }
            continue;
        }
    }
  }
  return Error;
}

DebugWaitFor dbgAddBreakPoint(char *line, processPo p, insWord ins, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok)
    ret = isValidBreakPoint(&bp);
  if (ret == Ok)
    ret = addBreakPoint(&bp);
  if (ret != Ok) {
    outMsg(logFile, "Could not set spy point on %s\n", line);
    outMsg(logFile, "usage: +pkg/Arity or +pkg:LineNo\n%_");
  } else
    outMsg(logFile, "%sspy point set on %s\n%_", bp.bkType == callBreak ? "call " : "line ", line);
  return moreDebug;
}

DebugWaitFor dbgClearBreakPoint(char *line, processPo p, insWord ins, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok)
    ret = clearBreakPoint(&bp);
  if (ret != Ok)
    outMsg(logFile, "Could not clear spy point on %s\n%_", line);
  else
    outMsg(logFile, "spy point cleared on %s\n%_", line);
  return moreDebug;
}
