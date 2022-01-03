//
// Created by Francis McCabe on 5/25/18.
//

#include "bkpoint.h"
#include <arith.h>
#include <labels.h>

integer addBreakPoints(breakPointPo bp) {
  return setLabelBreakPoint(bp->nm, uniStrLen(bp->nm), bp->arity);
}

integer clearBreakPoints(breakPointPo bp) {
  return clearLabelBreakPoint(bp->nm, uniStrLen(bp->nm), bp->arity);
}

retCode showAllBreakPoints(ioPo outChnnl) {
  return showLabelBreakPoints(outChnnl);
}

//A Break point is specified as:
//  prg/arity
//or
//  prg/*
//
//where prg is a wild card pattern
//
//E.g. *fooBar/2
//  foo*ba?r/*


retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp) {
  integer b = 0;
  integer ix = 0;

  integer arity = -1;

  enum {
    initSte,
    inNme,
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
        bp->arity = arity;
        return Ok;
      case '/': {
        if (pState == inNme) {
          appendCodePoint(bp->nm, &b, NumberOf(bp->nm), 0);
          pState = inArity;
          arity = 0;
          continue;
        } else {
          outMsg(logFile, "invalid break point: %S\n", buffer, bLen);
          return Error;
        }
      }
      default:
        switch (pState) {
          case inNme:
            appendCodePoint(bp->nm, &b, NumberOf(bp->nm), cp);
            continue;
          case inArity:
            if (isNdChar(cp)) {
              arity = arity * 10 + digitValue(cp);
              continue;
            } else if (cp == '*') {
              bp->arity = -1;
              return Ok;
            } else {
              outMsg(logFile, "invalid break point line number: %S\n", buffer, bLen);
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
