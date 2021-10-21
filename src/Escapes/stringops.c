//
// Created by Francis McCabe on 3/8/18.
//

#include <stringsP.h>
#include <arithP.h>
#include <stringBuffer.h>
#include <assert.h>
#include <tpl.h>
#include <globals.h>
#include <alloca.h>
#include "arithmetic.h"
#include "consP.h"
#include "option.h"
#include "charP.h"

ReturnStatus g__chr_eq(processPo p, ptrPo tos) {
  return (ReturnStatus) {.ret=Ok, .result=(charVal(tos[0]) == charVal(tos[1]) ? trueEnum : falseEnum)};
}

ReturnStatus g__chr_lt(processPo p, ptrPo tos) {
  return (ReturnStatus) {.ret=Ok, .result=(charVal(tos[0]) < charVal(tos[1]) ? trueEnum : falseEnum)};
}

ReturnStatus g__chr_ge(processPo p, ptrPo tos) {
  return (ReturnStatus) {.ret=Ok, .result=(charVal(tos[0]) >= charVal(tos[1]) ? trueEnum : falseEnum)};
}

ReturnStatus g__chr_hash(processPo p, ptrPo tos) {
  return (ReturnStatus) {.ret=Ok, .result=allocateInteger(processHeap(p), charVal(tos[0]))};
}

ReturnStatus g__chr_quote(processPo p, ptrPo tos) {
  strBufferPo strb = newStringBuffer();
  retCode ret = qtChar(O_IO(strb), charVal(tos[0]));

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus result = {.ret=ret,
    .result=(termPo) allocateString(processHeap(p), buff, oLen)};

  closeFile(O_IO(strb));
  return result;
}

ReturnStatus g__str_eq(processPo p, ptrPo tos) {
  stringPo Arg1 = C_STR(tos[0]);
  stringPo Arg2 = C_STR(tos[1]);

  logical eq = sameString(Arg1, Arg2);

  return (ReturnStatus) {.ret=Ok, .result=(eq ? trueEnum : falseEnum)};
}

// Lexicographic comparison
ReturnStatus g__str_lt(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen, rlen;
  const char *lhs = strVal(Arg1, &llen);
  const char *rhs = strVal(Arg2, &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen) {
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2) {
      return (ReturnStatus) {.ret=Ok, .result=trueEnum};
    } else if (chl > ch2) {
      return (ReturnStatus) {.ret=Ok, .result=falseEnum};
    }
  }
  if (ri < rlen) { // There is more on the right, so the left counts as being smaller
    return (ReturnStatus) {.ret=Ok, .result=trueEnum};
  } else {
    return (ReturnStatus) {.ret=Ok, .result=falseEnum};
  }
}

ReturnStatus g__str_ge(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen, rlen;
  const char *lhs = strVal(Arg1, &llen);
  const char *rhs = strVal(Arg2, &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen) {
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2) {
      return (ReturnStatus) {.ret=Ok, .result=falseEnum};
    } else if (chl > ch2) {
      return (ReturnStatus) {.ret=Ok, .result=trueEnum};
    }
  }
  if (li <= llen) { // There is more on the left, so it counts as being bigger
    return (ReturnStatus) {.ret=Ok, .result=trueEnum};
  } else {
    return (ReturnStatus) {.ret=Ok, .result=falseEnum};
  }
}

ReturnStatus g__str_hash(processPo p, ptrPo tos) {
  stringPo lhs = C_STR(tos[0]);

  if (lhs->hash == 0) {
    integer len;
    const char *str = strVal(tos[0], &len);
    lhs->hash = uniNHash(str, len);
  }

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(processHeap(p), lhs->hash)};
}

ReturnStatus g__str_len(processPo p, ptrPo tos) {
  integer len;
  const char *str = strVal(tos[0], &len);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(processHeap(p), len)};
}

ReturnStatus g__str2flt(processPo p, ptrPo tos) {
  integer len;
  const char *str = strVal(tos[0], &len);
  double flt;
  heapPo H = processHeap(p);

  switch (parseDouble(str, len, &flt)) {
    case Ok:
      return (ReturnStatus) {.ret=Ok,
        .result=(termPo) wrapSome(H, (termPo) allocateFloat(H, flt))};
    default:
    case Error:
      return (ReturnStatus) {.ret=Ok, .result = noneEnum};
  }
}

ReturnStatus g__str2int(processPo p, ptrPo tos) {
  integer len;
  const char *str = strVal(tos[0], &len);
  integer ix;
  heapPo H = processHeap(p);

  integer pos = 0;
  switch (parseInteger(str, &pos, len, &ix)) {
    case Ok:
      return (ReturnStatus) {.ret=Ok,
        .result=(termPo) wrapSome(H, (termPo) allocateInteger(processHeap(p), ix))};
    default:
      return (ReturnStatus) {.ret=Ok, .result = noneEnum};
  }
}

ReturnStatus g__str_gen(processPo p, ptrPo tos) {
  integer len;
  const char *str = strVal(tos[0], &len);
  char rnd[MAXLINE];

  strMsg(rnd, NumberOf(rnd), "%S%d", str, minimum(len, NumberOf(rnd) - INT64_DIGITS), randomInt());

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateString(processHeap(p), rnd, uniStrLen(rnd))};
}

ReturnStatus g__stringOf(processPo p, ptrPo tos) {
  termPo Arg2 = tos[1];
  termPo t = tos[0];
  integer depth = integerVal(Arg2);

  strBufferPo strb = newStringBuffer();
  retCode ret = dispTerm(O_IO(strb), t, 0, depth, False);

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus result = {.ret=ret,
    .result=(termPo) allocateString(processHeap(p), buff, oLen)};

  closeFile(O_IO(strb));
  return result;
}

ReturnStatus g__str_quote(processPo p, ptrPo tos) {
  termPo t = tos[0];

  strBufferPo strb = newStringBuffer();
  retCode ret = quoteStrg(O_IO(strb), C_STR(t));

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus result = {.ret=ret,
    .result=(termPo) allocateString(processHeap(p), buff, oLen)};

  closeFile(O_IO(strb));
  return result;
}

typedef enum {
  alignLeft,
  alignCenter,
  alignRight
} Alignment;

typedef enum {
  leftToRight,
  rightToLeft
} SrcAlign;

static char *const CHARS = "chrs_";
static char *const PAIR = "pair_";

ReturnStatus g__str_format(processPo p, ptrPo tos) {
  integer fLen;
  const char *fmt = strVal(tos[1], &fLen);
  Alignment alignment = alignLeft;
  SrcAlign sourceAlign = leftToRight;
  integer width = 0;
  integer srcWidth = 0;
  codePoint pad = ' ';

  integer fPos = 0;

  // How much of the source are we taking?
  parseInteger(fmt, &fPos, fLen, &srcWidth);

  if (srcWidth < 0) {
    sourceAlign = rightToLeft;
    srcWidth = -srcWidth;
  }

  // What is the output alignment?
  codePoint f = nextCodePoint(fmt, &fPos, fLen);
  switch (f) {
    case 'l':
    case 'L':
      alignment = alignLeft;
      break;
    case 'c':
    case 'C':
      alignment = alignCenter;
      break;
    case 'r':
    case 'R':
      alignment = alignRight;
      break;
    default:;
  }

  // What is the output width?
  parseInteger(fmt, &fPos, fLen, &width);

  if (width < 0) {
    width = -width;
  }

  if (fPos < fLen) {
    pad = nextCodePoint(fmt, &fPos, fLen);
  }

  strBufferPo strb = newStringBuffer();

  integer txtLen = 0;
  const char *txt = strVal(tos[0], &txtLen);
  integer txtPos = 0;

  integer txtQ = (width == 0 ? txtLen : minimum(width, txtLen));

  if (srcWidth != 0)
    txtQ = minimum(txtQ, srcWidth);

  switch (sourceAlign) {
    default:
    case leftToRight:
      txtPos = 0;
      break;
    case rightToLeft:
      txtPos = maximum(txtLen - txtQ, 0);
      break;
  }

  switch (alignment) {
    default:
    case alignLeft: {
      for (integer cx = 0; cx < txtQ; cx++)
        appendCodePointToStrBuffer(strb, nextCodePoint(txt, &txtPos, txtLen));
      for (integer cx = txtQ; cx < width; cx++)
        appendCodePointToStrBuffer(strb, pad);
      break;
    }
    case alignCenter: {
      integer space = (width - txtQ) / 2;
      for (integer cx = 0; cx < space; cx++)
        appendCodePointToStrBuffer(strb, pad);
      for (integer cx = 0; cx < txtQ; cx++)
        appendCodePointToStrBuffer(strb, nextCodePoint(txt, &txtPos, txtLen));
      for (integer cx = space + txtQ; cx < width; cx++)
        appendCodePointToStrBuffer(strb, pad);
      break;
    }
    case alignRight: {
      for (integer cx = 0; cx < width - txtQ; cx++)
        appendCodePointToStrBuffer(strb, pad);
      for (integer cx = 0; cx < txtQ; cx++)
        appendCodePointToStrBuffer(strb, nextCodePoint(txt, &txtPos, txtLen));
      break;
    }
  }

  ReturnStatus result = {.ret=Ok,
    .result= allocateFromStrBuffer(strb, processHeap(p))};

  closeFile(O_IO(strb));
  return result;
}

ReturnStatus g__explode(processPo p, ptrPo tos) {
  stringPo str = C_STR(tos[0]);
  integer len = strLength(str);
  char buffer[len + 1];

  retCode ret = copyChars2Buff(str, buffer, len + 1);
  if (ret == Ok) {
    heapPo H = processHeap(p);
    termPo list = (termPo) nilEnum;
    termPo el = voidEnum;
    int root = gcAddRoot(H, (ptrPo) &list);
    gcAddRoot(H, &el);

    integer pos = len;
    while (ret == Ok && pos > 0) {
      codePoint cp;
      ret = prevPoint(buffer, &pos, &cp);
      if (ret == Ok) {
        el = (termPo) allocateInteger(H, (integer) cp);
        list = (termPo) allocateCons(H, el, list);
      }
    }

    gcReleaseRoot(H, root);

    assert(consLength(list) == countCodePoints(buffer, 0, len));

    return (ReturnStatus) {.ret=Ok, .result=(termPo) list};
  } else {
    return (ReturnStatus) {.ret=ret, .result=(termPo) voidEnum};
  }
}

void dS(termPo w) {
  termPo s = w;

  while (isCons(s)) {
    integer cp = integerVal(consHead(C_NORMAL(s)));
    outChar(logFile, cp);
    s = consTail(C_NORMAL(s));
  }
  outStr(logFile, "\n");
  flushOut();
}

ReturnStatus g__implode(processPo p, ptrPo tos) {
  termPo list = tos[0];

  strBufferPo strb = newStringBuffer();

  while (isCons(list)) {
    normalPo pr = C_NORMAL(list);
    outChar(O_IO(strb), (codePoint) integerVal(consHead(pr)));
    list = consTail(pr);
  }

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  termPo result = (termPo) allocateString(processHeap(p), buff, oLen);

  closeFile(O_IO(strb));

  return (ReturnStatus) {.ret=Ok, .result=result};
}

ReturnStatus g__str_find(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  integer len;
  const char *str = strVal(Arg1, &len);
  integer tlen;
  const char *tgt = strVal(Arg2, &tlen);
  integer start = integerVal(Arg3);

  integer found = uniSearch(str, len, start, tgt, tlen);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(processHeap(p), found)};
}

ReturnStatus g__sub_str(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  integer len;
  const char *str = strVal(Arg1, &len);
  integer start = integerVal(Arg2);
  integer count = integerVal(Arg3);

  char buff[count + 1];
  uniMove(buff, count + 1, &str[start], count);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateString(processHeap(p), buff, count)};
}

ReturnStatus g__str_hdtl(processPo p, ptrPo tos) {
  stringPo src = C_STR(tos[0]);
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);
  heapPo H = processHeap(p);

  integer offset = 0;
  codePoint ch;
  retCode ret = nxtPoint(str, &offset, len, &ch);

  if (ret == Ok) {
    termPo chCode = allocateInteger(H, ch);
    int mark = gcAddRoot(H, &chCode);
    termPo rest = allocateString(H, &str[offset], len - offset);
    gcAddRoot(H, &rest);
    normalPo pair = allocateTpl(H, 2);
    setArg(pair, 0, chCode);
    setArg(pair, 1, rest);
    gcReleaseRoot(H, mark);
    return (ReturnStatus) {.ret=Ok, .result=(termPo) pair};
  } else {
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__str_cons(processPo p, ptrPo tos) {
  integer ch = integerVal(tos[0]);
  stringPo src = C_STR(tos[1]);
  integer len = strLength(src);
  integer offset = 0;
  char str[len + 16];
  appendCodePoint(str, &offset, len + 16, (codePoint) ch);
  retCode ret = copyChars2Buff(src, &str[offset], len + 16);
  heapPo H = processHeap(p);

  return (ReturnStatus) {.ret=ret,
    .result=(termPo) allocateString(processHeap(p), str, offset + len)};
}

ReturnStatus g__code2str(processPo p, ptrPo tos) {
  integer ch = integerVal(tos[0]);
  integer codeLength = 0;
  char str[16];
  appendCodePoint(str, &codeLength, NumberOf(str), (codePoint) ch);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateString(processHeap(p), str, codeLength)};
}

ReturnStatus g__str_apnd(processPo p, ptrPo tos) {
  integer ch = integerVal(tos[1]);
  stringPo src = C_STR(tos[0]);
  integer len = strLength(src);
  integer offset = len;
  char str[len + 16];
  copyChars2Buff(src, str, len + 16);
  heapPo H = processHeap(p);

  retCode ret = appendCodePoint(str, &offset, len + 16, (codePoint) integerVal(tos[0]));

  return (ReturnStatus) {.ret=ret,
    .result=(termPo) allocateString(processHeap(p), str, offset)};
}

ReturnStatus g__str_back(processPo p, ptrPo tos) {
  stringPo src = C_STR(tos[0]);
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);
  heapPo H = processHeap(p);

  integer offset = len;
  codePoint ch;
  retCode ret = prevPoint(str, &offset, &ch);

  if (ret == Ok) {
    termPo chCode = allocateInteger(H, ch);
    int mark = gcAddRoot(H, (ptrPo) &chCode);
    termPo rest = allocateString(H, str, offset);
    gcAddRoot(H, &rest);
    normalPo pair = allocateTpl(H, 2);
    setArg(pair, 0, rest);
    setArg(pair, 1, chCode);
    gcReleaseRoot(H, mark);
    return (ReturnStatus) {.ret=Ok, .result=(termPo) pair};
  } else {
    return (ReturnStatus) {.ret=ret, .result=voidEnum};
  }
}

ReturnStatus g__str_split(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer len;
  const char *str = strVal(Arg1, &len);
  integer start = integerVal(Arg2);

  char buff[len];
  uniMove(buff, len, str, len);

  heapPo H = processHeap(p);
  normalPo pair = allocateTpl(H, 2);
  int root = gcAddRoot(H, (ptrPo) &pair);

  termPo lhs = (termPo) allocateString(H, buff, start);
  setArg(pair, 0, lhs);

  termPo rhs = (termPo) allocateString(H, &buff[start], len - start);
  setArg(pair, 1, rhs);

  gcReleaseRoot(H, root);
  return (ReturnStatus) {.ret=Ok, .result=(termPo) pair};
}

ReturnStatus g__str_concat(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen;
  const char *lhs = strVal(Arg1, &llen);
  integer rlen;
  const char *rhs = strVal(Arg2, &rlen);

  integer len = llen + rlen + 1;
  char buff[len];
  uniMove(buff, len, lhs, llen);
  uniMove(&buff[llen], len - llen, rhs, rlen);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateString(processHeap(p), buff, llen + rlen)};
}

ReturnStatus g__str_splice(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  integer from = integerVal(tos[1]);
  integer cnt = integerVal(tos[2]);
  termPo Arg4 = tos[3];

  integer llen;
  const char *lhs = strVal(Arg1, &llen);
  integer rlen;
  const char *rhs = strVal(Arg4, &rlen);

  // Clamp the from and cnt values
  if (from < 0)
    from = 0;
  if (cnt < 0)
    cnt = 0;
  if (from > llen)
    from = llen;
  if (from + cnt > llen)
    cnt = llen - from;

  integer len = llen + rlen - cnt;
  char buff[len];
  uniMove(buff, len, lhs, from);
  uniMove(&buff[from], len - from, rhs, rlen);
  uniMove(&buff[from + rlen], len - from - rlen, &lhs[from + cnt], llen - from - cnt);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateString(processHeap(p), buff, len)};
}

ReturnStatus g__str_start(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen;
  const char *lhs = strVal(Arg1, &llen);
  integer rlen;
  const char *rhs = strVal(Arg2, &rlen);

  return (ReturnStatus) {.ret=Ok,
    .result=(uniIsPrefix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum)};
}

logical isStringTerm(termPo t) {
  if (isNormalPo(t)) {
    normalPo s = C_NORMAL(t);
    if (hasLabel(s, CHARS, 1))
      return True;
    else if (hasLabel(s, "pair_", 2)) {
      return isStringTerm(nthArg(s, 0)) && isStringTerm(nthArg(s, 1));
    } else
      return False;
  } else
    return False;
}

retCode dispStringTerm(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  if (isNormalPo(t)) {
    normalPo s = C_NORMAL(t);
    if (hasLabel(s, CHARS, 1) && isString(nthArg(s, 0))) {
      return outChars(out, nthArg(s, 0), precision, depth, alt);
    } else if (hasLabel(s, PAIR, 2)) {
      tryRet(dispStringTerm(out, nthArg(s, 0), precision, depth, alt));
      return dispStringTerm(out, nthArg(s, 1), precision, depth, alt);
    } else
      return Error;
  } else
    return Error;
}

retCode str_flatten(strBufferPo str, termPo t) {
  if (isNormalPo(t)) {
    normalPo ss = C_NORMAL(t);
    integer cx = termArity(ss);
    retCode ret = Ok;
    for (integer ix = 0; ret == Ok && ix < cx; ix++) {
      ret = str_flatten(str, nthArg(ss, ix));
    }
    return ret;
  } else if (isString(t)) {
    integer len;
    const char *elTxt = strVal(t, &len);
    return outText(O_IO(str), elTxt, len);
  } else
    return Error;
}

ReturnStatus g__str_multicat(processPo p, ptrPo tos) {
  strBufferPo strb = newStringBuffer();

  retCode ret = str_flatten(strb, tos[0]);

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus rt = {.ret=ret,
    .result=(termPo) allocateString(processHeap(p), buff, oLen)};
  closeFile(O_IO(strb));
  return rt;
}

ReturnStatus g__str_fltn(processPo p, ptrPo tos) {
  strBufferPo str = newStringBuffer();

  retCode ret = str_flatten(str, tos[0]);

  if (ret == Ok) {
    integer oLen;
    const char *buff = getTextFromBuffer(str, &oLen);

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateString(processHeap(p), buff, oLen)};
    closeFile(O_IO(str));
    return rt;
  } else {
    closeFile(O_IO(str));
    return (ReturnStatus) {.ret=ret, .result = voidEnum};
  }
}

ReturnStatus g__str_reverse(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  integer len;
  const char *lhs = strVal(Arg1, &len);

  char buff[len];
  uniMove(buff, len, lhs, len);

  uniReverse(buff, len);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateString(processHeap(p), buff, len)};
}
