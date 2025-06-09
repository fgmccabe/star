//
// Created by Francis McCabe on 3/8/18.
//

#include <stringsP.h>
#include <arithP.h>
#include <assert.h>
#include <tpl.h>
#include <globals.h>
#include <stdlib.h>
#include "arithmetic.h"
#include "consP.h"
#include "option.h"
#include "charP.h"
#include "errorCodes.h"

ReturnStatus g__chr_eq(processPo P) {
  codePoint lhs = charVal(popVal(P));
  codePoint rhs = charVal(popVal(P));
  pshVal(P, lhs == rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__chr_lt(processPo P) {
  codePoint lhs = charVal(popVal(P));
  codePoint rhs = charVal(popVal(P));
  pshVal(P, lhs < rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__chr_ge(processPo P) {
  codePoint lhs = charVal(popVal(P));
  codePoint rhs = charVal(popVal(P));
  pshVal(P, lhs >= rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__chr_hash(processPo P) {
  codePoint lhs = charVal(popVal(P));
  pshVal(P, makeInteger(lhs));
  return Normal;
}

ReturnStatus g__chr_quote(processPo P) {
  strBufferPo strb = newStringBuffer();
  qtChar(O_IO(strb), charVal(popVal(P)));

  pshVal(P, allocateFromStrBuffer(processHeap(P), strb));

  closeIo(O_IO(strb));
  return Normal;
}

// Support formatting of char values
ReturnStatus g__chr_format(processPo P) {
  codePoint cp = charVal(popVal(P));
  integer fLen;
  const char *fmt = strVal(popVal(P), &fLen);
  strBufferPo strb = newStringBuffer();
  retCode ret;

  // Allowed formats are q, c, x, 0, 9
  if (fLen == 1) {
    switch (fmt[0]) {
      case 'q': {
        ret = qtChar(O_IO(strb), cp);
        break;
      }
      case 'c': {
        ret = outChar(O_IO(strb), cp);
        break;
      }
      case 'x': {
        ret = outMsg(O_IO(strb), "%x", (integer) cp);
        break;
      }
      default: {
        ret = outMsg(O_IO(strb), "%d", (integer) cp);
        break;
      }
    }

    if (ret == Ok) {
      pshVal(P, allocateFromStrBuffer(processHeap(P), strb));
      closeIo(O_IO(strb));
      return Normal;
    } else {
      closeIo(O_IO(strb));
      pshVal(P, eINVAL);
      return Abnormal;
    }
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__str_eq(processPo P) {
  stringPo s1 = C_STR(popVal(P));
  stringPo s2 = C_STR(popVal(P));

  logical eq = stringHash(s1) == stringHash(s2) && sameString(s1, s2);
  pshVal(P, (eq ? trueEnum : falseEnum));
  return Normal;
}

// Lexicographic comparison
ReturnStatus g__str_lt(processPo P) {
  integer llen, rlen;
  const char *lhs = strVal(popVal(P), &llen);
  const char *rhs = strVal(popVal(P), &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen) {
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2) {
      pshVal(P, trueEnum);
      return Normal;
    } else if (chl > ch2) {
      pshVal(P, falseEnum);
      return Normal;
    }
  }
  if (ri < rlen)
    // There is more on the right, so the left counts as being smaller
    pshVal(P, trueEnum);
  else
    pshVal(P, falseEnum);

  return Normal;
}

ReturnStatus g__str_ge(processPo P) {
  integer llen, rlen;
  const char *lhs = strVal(popVal(P), &llen);
  const char *rhs = strVal(popVal(P), &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen) {
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2) {
      pshVal(P, falseEnum);
      return Normal;
    } else if (chl > ch2) {
      pshVal(P, trueEnum);
      return Normal;
    }
  }
  if (li <= llen) {
    // There is more on the left, so it counts as being bigger
    pshVal(P, trueEnum);
    return Normal;
  } else {
    pshVal(P, falseEnum);
    return Normal;
  }
}

ReturnStatus g__str_hash(processPo P) {
  termPo a1 = popVal(P);
  stringPo lhs = C_STR(a1);

  if (lhs->hash == 0) {
    integer len;
    const char *str = strVal(a1, &len);
    lhs->hash = uniNHash(str, len);
  }

  pshVal(P, makeInteger(lhs->hash));
  return Normal;
}

ReturnStatus g__str_len(processPo P) {
  pshVal(P, makeInteger(strLength(C_STR(popVal(P)))));
  return Normal;
}

ReturnStatus g__str2flt(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  double flt;

  switch (parseDouble(str, len, &flt)) {
    case Ok:
      pshVal(P, (termPo) wrapSome(processHeap(P), makeFloat(flt)));
      return Normal;
    default:
    case Error:
      pshVal(P, noneEnum);
      return Normal;
  }
}

ReturnStatus g__str2int(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  integer ix;

  integer pos = 0;
  switch (parseInteger(str, &pos, len, &ix)) {
    case Ok:
      pshVal(P, (termPo) wrapSome(processHeap(P), makeInteger(ix)));
      return Normal;
    default:
      pshVal(P, noneEnum);
      return Normal;
  }
}

ReturnStatus g__str_charat(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  integer ix = integerVal(popVal(P));

  if (ix >= len) {
    pshVal(P, noneEnum);
    return Normal;
  } else {
    codePoint cp;
    retCode ret = uniCharAt(str, len, ix, &cp);
    if (ret == Ok)
      pshVal(P, (termPo) wrapSome(processHeap(P), allocateCharacter(cp)));
    else
      pshVal(P, noneEnum);
    return Normal;
  }
}

ReturnStatus g__str_gen(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  char rnd[MAXLINE];

  strMsg(rnd, NumberOf(rnd), "%S%d", str, minimum(len, NumberOf(rnd) - INT64_DIGITS), randomInt());

  pshVal(P, allocateString(processHeap(P), rnd, uniStrLen(rnd)));
  return Normal;
}

ReturnStatus g__stringOf(processPo P) {
  termPo a1 = popVal(P);
  integer depth = integerVal(popVal(P));

  strBufferPo strb = newStringBuffer();
  dispTerm(O_IO(strb), a1, 0, depth, False);

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  pshVal(P, allocateString(processHeap(P), buff, oLen));

  closeIo(O_IO(strb));
  return Normal;
}

ReturnStatus g__str_quote(processPo P) {
  strBufferPo strb = newStringBuffer();
  quoteStrg(O_IO(strb), C_STR(popVal(P)));

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  pshVal(P, allocateString(processHeap(P), buff, oLen));
  closeIo(O_IO(strb));
  return Normal;
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

ReturnStatus g__str_format(processPo P) {
  termPo a1 = popVal(P);
  termPo a2 = popVal(P);
  integer fLen;
  const char *fmt = strVal(a2, &fLen);
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
  const char *txt = strVal(a1, &txtLen);
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

  pshVal(P, allocateFromStrBuffer(processHeap(P), strb));

  closeIo(O_IO(strb));
  return Normal;
}

ReturnStatus g__explode(processPo P) {
  stringPo str = C_STR(popVal(P));
  integer len = strLength(str);
  char buffer[len + 1];

  copyChars2Buff(str, buffer, len + 1);
heapPo h = processHeap(P);
  termPo list = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(h, (ptrPo) &list);
  gcAddRoot(h, &el);

  integer pos = len;
  retCode ret = Ok;
  while (ret == Ok && pos > 0) {
    codePoint cp;
    ret = prevPoint(buffer, &pos, &cp);
    if (ret == Ok) {
      el = allocateCharacter(cp);
      list = (termPo) allocateCons(h, el, list);
    }
  }

  gcReleaseRoot(h, root);

  assert(consLength(list) == countCodePoints(buffer, 0, len));

  pshVal(P, list);
  return Normal;
}

void dS(termPo w) {
  termPo s = w;

  while (isCons(s)) {
    integer cp = integerVal(consHead(C_NORMAL(s)));
    outChar(logFile, (codePoint) cp);
    s = consTail(C_NORMAL(s));
  }
  outStr(logFile, "\n");
  flushOut();
}

ReturnStatus g__implode(processPo P) {
  termPo list = popVal(P);

  integer size = 1;
  for (termPo lst = list; isCons(lst); lst = consTail(C_NORMAL(lst))) {
    normalPo en = C_NORMAL(lst);
    size += codePointSize(charVal(consHead(en)));
  }

  char buff[MAXLINE];
  char *buffer = (size > MAXLINE ? (char *) malloc(sizeof(char) * size) : buff);
  integer pos = 0;

  while (isCons(list)) {
    normalPo pr = C_NORMAL(list);
    codePoint ch = charVal(consHead(pr));
    appendCodePoint(buffer, &pos, size, ch);
    list = consTail(pr);
  }

  termPo result = (termPo) allocateString(processHeap(P), buffer, pos);

  if (buffer != buff)
    free(buffer);
  pshVal(P, result);
  return Normal;
}

ReturnStatus g__str_find(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  integer tlen;
  const char *tgt = strVal(popVal(P), &tlen);
  integer start = integerVal(popVal(P));

  integer found = uniSearch(str, len, start, tgt, tlen);
  pshVal(P, makeInteger(found));
  return Normal;
}

ReturnStatus g__sub_str(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  integer start = integerVal(popVal(P));
  integer count = integerVal(popVal(P));

  count = minimum(count, len - start);

  char buff[count + 1];
  uniMove(buff, count + 1, &str[start], count);
  pshVal(P, allocateString(processHeap(P), buff, count));
  return Normal;
}

ReturnStatus g__str_hdtl(processPo P) {
  stringPo src = C_STR(popVal(P));
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);

  integer offset = 0;
  codePoint ch;
  retCode ret = nxtPoint(str, &offset, len, &ch);
  heapPo h = processHeap(P);

  if (ret == Ok) {
    termPo chCode = allocateCharacter(ch);
    int mark = gcAddRoot(h, &chCode);
    termPo rest = allocateString(h, &str[offset], len - offset);
    gcAddRoot(h, &rest);
    normalPo pair = allocateTpl(h, 2);
    setArg(pair, 0, chCode);
    setArg(pair, 1, rest);
    gcReleaseRoot(h, mark);
    pshVal(P, (termPo) wrapSome(h, (termPo) pair));
    return Normal;
  } else {
    pshVal(P, noneEnum);
    return Normal;
  }
}

ReturnStatus g__str_cons(processPo P) {
  codePoint ch = charVal(popVal(P));
  stringPo src = C_STR(popVal(P));
  integer len = strLength(src);
  integer offset = 0;
  char str[len + 16];
  appendCodePoint(str, &offset, len + 16, ch);
  copyChars2Buff(src, &str[offset], len + 16);
  pshVal(P, allocateString(processHeap(P), str, offset + len));
  return Normal;
}

ReturnStatus g__code2str(processPo P) {
  codePoint ch = charVal(popVal(P));
  integer codeLength = 0;
  char str[16];
  appendCodePoint(str, &codeLength, NumberOf(str), (codePoint) ch);

  pshVal(P, allocateString(processHeap(P), str, codeLength));
  return Normal;
}

ReturnStatus g__str_apnd(processPo P) {
  stringPo src = C_STR(popVal(P));
  codePoint ch = charVal(popVal(P));
  integer len = strLength(src);
  integer offset = len;
  char str[len + 16];
  copyChars2Buff(src, str, len + 16);

  appendCodePoint(str, &offset, len + 16, ch);
  pshVal(P, allocateString(processHeap(P), str, offset));
  return Normal;
}

ReturnStatus g__str_back(processPo P) {
  stringPo src = C_STR(popVal(P));
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);

  integer offset = len;
  codePoint ch;
  retCode ret = prevPoint(str, &offset, &ch);

  if (ret == Ok) {
    termPo chCode = allocateCharacter(ch);
    heapPo h = processHeap(P);
    int mark = gcAddRoot(h, (ptrPo) &chCode);
    termPo rest = allocateString(h, str, offset);
    gcAddRoot(h, &rest);
    normalPo pair = allocateTpl(h, 2);
    setArg(pair, 0, rest);
    setArg(pair, 1, chCode);
    gcReleaseRoot(h, mark);
    pshVal(P, (termPo) pair);
    return Normal;
  } else {
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}

ReturnStatus g__str_split(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  integer start = integerVal(popVal(P));

  char buff[len];
  uniMove(buff, len, str, len);
heapPo h = processHeap(P);
  normalPo pair = allocateTpl(h, 2);
  int root = gcAddRoot(h, (ptrPo) &pair);

  termPo lhs = (termPo) allocateString(h, buff, start);
  setArg(pair, 0, lhs);

  termPo rhs = (termPo) allocateString(h, &buff[start], len - start);
  setArg(pair, 1, rhs);

  gcReleaseRoot(h, root);
  pshVal(P, (termPo) pair);
  return Normal;
}

ReturnStatus g__str_concat(processPo P) {
  integer llen;
  const char *lhs = strVal(popVal(P), &llen);
  integer rlen;
  const char *rhs = strVal(popVal(P), &rlen);

  integer len = llen + rlen + 1;
  char buff[len];
  uniMove(buff, len, lhs, llen);
  uniMove(&buff[llen], len - llen, rhs, rlen);

  pshVal(P, allocateString(processHeap(P), buff, llen + rlen));
  return Normal;
}

ReturnStatus g__str_splice(processPo P) {
  integer llen;
  const char *lhs = strVal(popVal(P), &llen);
  integer from = integerVal(popVal(P));
  integer cnt = integerVal(popVal(P));
  integer rlen;
  const char *rhs = strVal(popVal(P), &rlen);

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

  pshVal(P, allocateString(processHeap(P), buff, len));
  return Normal;
}

ReturnStatus g__str_start(processPo P) {
  integer llen;
  const char *lhs = strVal(popVal(P), &llen);
  integer rlen;
  const char *rhs = strVal(popVal(P), &rlen);
  pshVal(P, (uniIsPrefix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum));
  return Normal;
}

ReturnStatus g__str_end(processPo P) {
  integer llen;
  const char *lhs = strVal(popVal(P), &llen);
  integer rlen;
  const char *rhs = strVal(popVal(P), &rlen);

  pshVal(P, (uniIsSuffix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum));
  return Normal;
}

ReturnStatus g__str_multicat(processPo P) {
  termPo t = popVal(P);
  strBufferPo strb = newStringBuffer();

  retCode ret = Ok;
  while (ret == Ok && isCons(t)) {
    normalPo c = C_NORMAL(t);
    integer len;
    const char *elTxt = strVal(consHead(c), &len);
    ret = outText(O_IO(strb), elTxt, len);
    t = consTail(c);
  }
  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);
  pshVal(P, allocateString(processHeap(P), buff, oLen));
  closeIo(O_IO(strb));
  return Normal;
}

ReturnStatus g__str_reverse(processPo P) {
  integer len;
  const char *lhs = strVal(popVal(P), &len);

  char buff[len];
  uniMove(buff, len, lhs, len);

  uniReverse(buff, len);

  pshVal(P, allocateString(processHeap(P), buff, len));

  return Normal;
}
