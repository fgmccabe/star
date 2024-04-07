//
// Created by Francis McCabe on 3/8/18.
//

#include <stringsP.h>
#include <arithP.h>
#include <stringBuffer.h>
#include <assert.h>
#include <tpl.h>
#include <globals.h>
#include <stdlib.h>
#include "arithmetic.h"
#include "consP.h"
#include "option.h"
#include "charP.h"
#include "stringops.h"
#include "errorCodes.h"

ReturnStatus g__chr_eq(heapPo h, termPo a1, termPo a2) {
  return (ReturnStatus) {.ret=Normal, .result=(charVal(a1) == charVal(a2) ? trueEnum : falseEnum)};
}

ReturnStatus g__chr_lt(heapPo h, termPo a1, termPo a2) {
  return (ReturnStatus) {.ret=Normal, .result=(charVal(a1) < charVal(a2) ? trueEnum : falseEnum)};
}

ReturnStatus g__chr_ge(heapPo h, termPo a1, termPo a2) {
  return (ReturnStatus) {.ret=Normal, .result=(charVal(a1) >= charVal(a2) ? trueEnum : falseEnum)};
}

ReturnStatus g__chr_hash(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Normal, .result=makeInteger(charVal(a1))};
}

ReturnStatus g__chr_quote(heapPo h, termPo a1) {
  strBufferPo strb = newStringBuffer();
  qtChar(O_IO(strb), charVal(a1));

  ReturnStatus result = {.ret=Normal, .result= allocateFromStrBuffer(h, strb)};

  closeIo(O_IO(strb));
  return result;
}

// Support formatting of char values
ReturnStatus g__chr_format(heapPo h, termPo a1, termPo a2) {
  codePoint cp = charVal(a1);
  integer fLen;
  const char *fmt = strVal(a2, &fLen);
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

    ReturnStatus result = {.ret=Normal, .result= allocateFromStrBuffer(h, strb)};
    closeIo(O_IO(strb));
    return result;
  } else {
    ReturnStatus result = {.ret=Normal, .result=allocateCString(h, "format error")};
    return result;
  }
}

ReturnStatus g__str_eq(heapPo h, termPo a1, termPo a2) {
  stringPo s1 = C_STR(a1);
  stringPo s2 = C_STR(a2);

  logical eq = stringHash(s1) == stringHash(s2) && sameString(s1, s2);

  return (ReturnStatus) {.ret=Normal, .result=(eq ? trueEnum : falseEnum)};
}

// Lexicographic comparison
ReturnStatus g__str_lt(heapPo h, termPo a1, termPo a2) {
  integer llen, rlen;
  const char *lhs = strVal(a1, &llen);
  const char *rhs = strVal(a2, &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen) {
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2) {
      return (ReturnStatus) {.ret=Normal, .result=trueEnum};
    } else if (chl > ch2) {
      return (ReturnStatus) {.ret=Normal, .result=falseEnum};
    }
  }
  if (ri < rlen) { // There is more on the right, so the left counts as being smaller
    return (ReturnStatus) {.ret=Normal, .result=trueEnum};
  } else {
    return (ReturnStatus) {.ret=Normal, .result=falseEnum};
  }
}

ReturnStatus g__str_ge(heapPo h, termPo a1, termPo a2) {
  integer llen, rlen;
  const char *lhs = strVal(a1, &llen);
  const char *rhs = strVal(a2, &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen) {
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2) {
      return (ReturnStatus) {.ret=Normal, .result=falseEnum};
    } else if (chl > ch2) {
      return (ReturnStatus) {.ret=Normal, .result=trueEnum};
    }
  }
  if (li <= llen) { // There is more on the left, so it counts as being bigger
    return (ReturnStatus) {.ret=Normal, .result=trueEnum};
  } else {
    return (ReturnStatus) {.ret=Normal, .result=falseEnum};
  }
}

ReturnStatus g__str_hash(heapPo h, termPo a1) {
  stringPo lhs = C_STR(a1);

  if (lhs->hash == 0) {
    integer len;
    const char *str = strVal(a1, &len);
    lhs->hash = uniNHash(str, len);
  }

  return (ReturnStatus) {.ret=Normal,
    .result=makeInteger(lhs->hash)};
}

ReturnStatus g__str_len(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Normal,
    .result= makeInteger(strLength(C_STR(a1)))};
}

ReturnStatus g__str2flt(heapPo h, termPo a1) {
  integer len;
  const char *str = strVal(a1, &len);
  double flt;
  heapPo H = h;

  switch (parseDouble(str, len, &flt)) {
    case Ok:
      return (ReturnStatus) {.ret=Normal,
        .result=(termPo) wrapSome(H, makeFloat(flt))};
    default:
    case Error:
      return (ReturnStatus) {.ret=Normal, .result = noneEnum};
  }
}

ReturnStatus g__str2int(heapPo h, termPo a1) {
  integer len;
  const char *str = strVal(a1, &len);
  integer ix;
  heapPo H = h;

  integer pos = 0;
  switch (parseInteger(str, &pos, len, &ix)) {
    case Ok:
      return (ReturnStatus) {.ret=Normal,
        .result=(termPo) wrapSome(H, makeInteger(ix))};
    default:
      return (ReturnStatus) {.ret=Normal, .result = noneEnum};
  }
}

ReturnStatus g__str_charat(heapPo h, termPo a1, termPo a2) {
  integer len;
  const char *str = strVal(a1, &len);
  integer ix = integerVal(a2);

  if (ix >= len)
    return (ReturnStatus) {.ret=Normal, .result = noneEnum};
  else {
    codePoint cp;
    retCode ret = uniCharAt(str, len, ix, &cp);
    if (ret == Ok) {
      return (ReturnStatus) {.ret=Normal, .result = (termPo) wrapSome(h, allocateCharacter(cp))};
    } else
      return (ReturnStatus) {.ret=Normal, .result = noneEnum};
  }
}

ReturnStatus g__str_gen(heapPo h, termPo a1) {
  integer len;
  const char *str = strVal(a1, &len);
  char rnd[MAXLINE];

  strMsg(rnd, NumberOf(rnd), "%S%d", str, minimum(len, NumberOf(rnd) - INT64_DIGITS), randomInt());

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, rnd, uniStrLen(rnd))};
}

ReturnStatus g__stringOf(heapPo h, termPo a1, termPo a2) {
  integer depth = integerVal(a2);

  strBufferPo strb = newStringBuffer();
  dispTerm(O_IO(strb), a1, 0, depth, False);

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus result = {.ret=Normal,
    .result=(termPo) allocateString(h, buff, oLen)};

  closeIo(O_IO(strb));
  return result;
}

ReturnStatus g__str_quote(heapPo h, termPo a1) {
  strBufferPo strb = newStringBuffer();
  quoteStrg(O_IO(strb), C_STR(a1));

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus result = {.ret=Normal,
    .result=(termPo) allocateString(h, buff, oLen)};

  closeIo(O_IO(strb));
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

ReturnStatus g__str_format(heapPo h, termPo a1, termPo a2) {
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

  ReturnStatus result = {.ret=Normal,
    .result= allocateFromStrBuffer(h, strb)};

  closeIo(O_IO(strb));
  return result;
}

ReturnStatus g__explode(heapPo h, termPo a1) {
  stringPo str = C_STR(a1);
  integer len = strLength(str);
  char buffer[len + 1];

  copyChars2Buff(str, buffer, len + 1);

  heapPo H = h;
  termPo list = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(H, (ptrPo) &list);
  gcAddRoot(H, &el);

  integer pos = len;
  retCode ret = Ok;
  while (ret == Ok && pos > 0) {
    codePoint cp;
    ret = prevPoint(buffer, &pos, &cp);
    if (ret == Ok) {
      el = allocateCharacter(cp);
      list = (termPo) allocateCons(H, el, list);
    }
  }

  gcReleaseRoot(H, root);

  assert(consLength(list) == countCodePoints(buffer, 0, len));

  return (ReturnStatus) {.ret=Normal, .result=(termPo) list};
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

ReturnStatus g__implode(heapPo h, termPo a1) {
  termPo list = a1;

  integer size = 1;
  for (termPo lst = a1; isCons(lst); lst = consTail(C_NORMAL(lst))) {
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

  termPo result = (termPo) allocateString(h, buffer, pos);

  if (buffer != buff)
    free(buffer);
  return (ReturnStatus) {.ret=Normal, .result=result};
}

ReturnStatus g__str_find(heapPo h, termPo a1, termPo a2, termPo a3) {
  integer len;
  const char *str = strVal(a1, &len);
  integer tlen;
  const char *tgt = strVal(a2, &tlen);
  integer start = integerVal(a3);

  integer found = uniSearch(str, len, start, tgt, tlen);

  return (ReturnStatus) {.ret=Normal, .result=makeInteger(found)};
}

ReturnStatus g__sub_str(heapPo h, termPo a1, termPo a2, termPo a3) {
  integer len;
  const char *str = strVal(a1, &len);
  integer start = integerVal(a2);
  integer count = integerVal(a3);

  count = minimum(count, len - start);

  char buff[count + 1];
  uniMove(buff, count + 1, &str[start], count);

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, buff, count)};
}

ReturnStatus g__str_hdtl(heapPo h, termPo a1) {
  stringPo src = C_STR(a1);
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);
  heapPo H = h;

  integer offset = 0;
  codePoint ch;
  retCode ret = nxtPoint(str, &offset, len, &ch);

  if (ret == Ok) {
    termPo chCode = allocateCharacter(ch);
    int mark = gcAddRoot(H, &chCode);
    termPo rest = allocateString(H, &str[offset], len - offset);
    gcAddRoot(H, &rest);
    normalPo pair = allocateTpl(H, 2);
    setArg(pair, 0, chCode);
    setArg(pair, 1, rest);
    gcReleaseRoot(H, mark);
    return (ReturnStatus) {.ret=Normal, .result=(termPo) wrapSome(h, (termPo) pair)};
  } else {
    return (ReturnStatus) {.ret=Normal, .result=noneEnum};
  }
}

ReturnStatus g__str_cons(heapPo h, termPo a1, termPo a2) {
  codePoint ch = charVal(a1);
  stringPo src = C_STR(a2);
  integer len = strLength(src);
  integer offset = 0;
  char str[len + 16];
  appendCodePoint(str, &offset, len + 16, ch);
  copyChars2Buff(src, &str[offset], len + 16);

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, str, offset + len)};
}

ReturnStatus g__code2str(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);
  integer codeLength = 0;
  char str[16];
  appendCodePoint(str, &codeLength, NumberOf(str), (codePoint) ch);

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, str, codeLength)};
}

ReturnStatus g__str_apnd(heapPo h, termPo a1, termPo a2) {
  codePoint ch = charVal(a2);
  stringPo src = C_STR(a1);
  integer len = strLength(src);
  integer offset = len;
  char str[len + 16];
  copyChars2Buff(src, str, len + 16);

  appendCodePoint(str, &offset, len + 16, ch);

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, str, offset)};
}

ReturnStatus g__str_back(heapPo h, termPo xc, termPo a1) {
  stringPo src = C_STR(a1);
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);
  heapPo H = h;

  integer offset = len;
  codePoint ch;
  retCode ret = prevPoint(str, &offset, &ch);

  if (ret == Ok) {
    termPo chCode = allocateCharacter(ch);
    int mark = gcAddRoot(H, (ptrPo) &chCode);
    termPo rest = allocateString(H, str, offset);
    gcAddRoot(H, &rest);
    normalPo pair = allocateTpl(H, 2);
    setArg(pair, 0, rest);
    setArg(pair, 1, chCode);
    gcReleaseRoot(H, mark);
    return (ReturnStatus) {.ret=Normal, .result=(termPo) pair};
  } else {
    return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=eNOTFND};
  }
}

ReturnStatus g__str_split(heapPo h, termPo a1, termPo a2) {
  integer len;
  const char *str = strVal(a1, &len);
  integer start = integerVal(a2);

  char buff[len];
  uniMove(buff, len, str, len);

  heapPo H = h;
  normalPo pair = allocateTpl(H, 2);
  int root = gcAddRoot(H, (ptrPo) &pair);

  termPo lhs = (termPo) allocateString(H, buff, start);
  setArg(pair, 0, lhs);

  termPo rhs = (termPo) allocateString(H, &buff[start], len - start);
  setArg(pair, 1, rhs);

  gcReleaseRoot(H, root);
  return (ReturnStatus) {.ret=Normal, .result=(termPo) pair};
}

ReturnStatus g__str_concat(heapPo h, termPo a1, termPo a2) {
  integer llen;
  const char *lhs = strVal(a1, &llen);
  integer rlen;
  const char *rhs = strVal(a2, &rlen);

  integer len = llen + rlen + 1;
  char buff[len];
  uniMove(buff, len, lhs, llen);
  uniMove(&buff[llen], len - llen, rhs, rlen);

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, buff, llen + rlen)};
}

ReturnStatus g__str_splice(heapPo h, termPo a1, termPo a2, termPo a3, termPo a4) {
  integer from = integerVal(a2);
  integer cnt = integerVal(a3);

  integer llen;
  const char *lhs = strVal(a1, &llen);
  integer rlen;
  const char *rhs = strVal(a4, &rlen);

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

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, buff, len)};
}

ReturnStatus g__str_start(heapPo h, termPo a1, termPo a2) {
  integer llen;
  const char *lhs = strVal(a1, &llen);
  integer rlen;
  const char *rhs = strVal(a2, &rlen);

  return (ReturnStatus) {.ret=Normal,
    .result=(uniIsPrefix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum)};
}

ReturnStatus g__str_end(heapPo h, termPo a1, termPo a2) {
  integer llen;
  const char *lhs = strVal(a1, &llen);
  integer rlen;
  const char *rhs = strVal(a2, &rlen);

  return (ReturnStatus) {.ret=Normal,
    .result=(uniIsSuffix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum)};
}

ReturnStatus g__str_multicat(heapPo h, termPo t) {
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

  ReturnStatus rt = {.ret=Normal,
    .result=(termPo) allocateString(h, buff, oLen)};
  closeIo(O_IO(strb));
  return rt;
}

ReturnStatus g__str_reverse(heapPo h, termPo a1) {
  integer len;
  const char *lhs = strVal(a1, &len);

  char buff[len];
  uniMove(buff, len, lhs, len);

  uniReverse(buff, len);

  return (ReturnStatus) {.ret=Normal,
    .result=(termPo) allocateString(h, buff, len)};
}
