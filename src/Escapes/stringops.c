//
// Created by Francis McCabe on 3/8/18.
//

#include <charsP.h>
#include <arithP.h>
#include <stringBuffer.h>
#include <assert.h>
#include <tpl.h>
#include <globals.h>
#include <alloca.h>
#include "arithmetic.h"
#include "consP.h"
#include "option.h"

static retCode flatten(strBufferPo str, termPo t);

typedef struct stringIterator_ *strIteratorPo;
typedef struct {
  normalPo t;
  integer off;
} IteratorPair;

typedef struct stringIterator_ {
  integer maxDepth;
  integer currDepth;
  char *chars;
  integer currIndex;
  integer currLimit;
  IteratorPair table[ZEROARRAYSIZE];
} StringIteratorRecord;

#define iteratorTableSize(D) (sizeof(StringIteratorRecord)+sizeof(IteratorPair)*D)

static integer termDepth(termPo t) {
  if (isNormalPo(t)) {
    normalPo ss = C_NORMAL(t);
    integer cx = termArity(ss);
    integer mx = -1;
    for (integer ix = 0; ix < cx; ix++) {
      integer elMx = termDepth(nthArg(ss, ix));
      if (elMx > mx)
        mx = elMx;
    }
    return mx + 1;
  } else if (isChars(t)) {
    return 1;
  } else
    return 0;
}

static void pushTerm(strIteratorPo it, normalPo t) {
  it->table[it->currDepth].t = t;
  it->table[it->currDepth].off = 0;
  it->currDepth++;
}

static logical hasMore(strIteratorPo it) {
  return it->currIndex < it->currLimit;
}

static logical advance(strIteratorPo it) {
  assert(it->currDepth > 0);

  while (it->currIndex >= it->currLimit && it->currDepth > 0) {
    while (it->currDepth > 0 && it->table[it->currDepth - 1].off >= termArity(it->table[it->currDepth - 1].t)) {
      it->currDepth--;
    }

    if (it->currDepth > 0) {
      normalPo curr = it->table[it->currDepth - 1].t;
      termPo child = nthArg(curr, it->table[it->currDepth - 1].off++);
      while (isNormalPo(child)) {
        pushTerm(it, C_NORMAL(child));
        child = nthArg(C_NORMAL(child), it->table[it->currDepth - 1].off++);
      }
      assert(isChars(child));
      it->chars = (char *) charsVal(child, &it->currLimit);
      it->currIndex = 0;
    }
  }
  return it->currIndex < it->currLimit;
}

static codePoint next(strIteratorPo it) {
  assert(hasMore(it));

  codePoint cp = nextCodePoint(it->chars, &it->currIndex, it->currLimit);
  advance(it);
  return cp;
}

void initStrIterator(strIteratorPo it, integer dp, termPo t) {
  it->maxDepth = dp;
  it->currDepth = 0;
  it->currIndex = 0;
  it->currLimit = 0;

  if (isChars(t)) {
    it->chars = (char *) charsVal(t, &it->currLimit);
  } else {
    pushTerm(it, C_NORMAL(t));
    advance(it);
  }
}

// This is pretty complex :(
ReturnStatus g__string_eq(processPo p, ptrPo tos) {
  integer d1 = termDepth(tos[0]);
  integer d2 = termDepth(tos[1]);
  strIteratorPo i1 = (strIteratorPo) (alloca(iteratorTableSize(d1)));
  strIteratorPo i2 = (strIteratorPo) (alloca(iteratorTableSize(d2)));

  initStrIterator(i1, d1, tos[0]);
  initStrIterator(i2, d2, tos[1]);

  while (hasMore(i1) && hasMore(i2)) {
    codePoint ch1 = next(i1);
    codePoint ch2 = next(i2);
    if (ch1 != ch2)
      return (ReturnStatus) {.ret=Ok, .result=(falseEnum)};
  }
  return (ReturnStatus) {.ret=Ok, .result=((!hasMore(i1) && !hasMore(i2)) ? trueEnum : falseEnum)};
}

ReturnStatus g__str_eq(processPo p, ptrPo tos) {
  charsPo Arg1 = C_CHARS(tos[0]);
  charsPo Arg2 = C_CHARS(tos[1]);

  logical eq = sameCharSeqs(Arg1, Arg2);

  return (ReturnStatus) {.ret=Ok, .result=(eq ? trueEnum : falseEnum)};
}

// Lexicographic comparison
ReturnStatus g__str_lt(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen, rlen;
  const char *lhs = charsVal(Arg1, &llen);
  const char *rhs = charsVal(Arg2, &rlen);

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
  const char *lhs = charsVal(Arg1, &llen);
  const char *rhs = charsVal(Arg2, &rlen);

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
  charsPo lhs = C_CHARS(tos[0]);

  if (lhs->hash == 0) {
    integer len;
    const char *str = charsVal(tos[0], &len);
    lhs->hash = uniNHash(str, len);
  }

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(processHeap(p), lhs->hash)};
}

ReturnStatus g__string_len(processPo p, ptrPo tos) {
  integer d1 = termDepth(tos[0]);
  strIteratorPo i1 = (strIteratorPo) (alloca(iteratorTableSize(d1)));
  initStrIterator(i1, d1, tos[0]);

  integer count = 0;

  while (hasMore(i1)) {
    count++;
    next(i1);
  }
  return (ReturnStatus) {.ret=Ok, .result=(termPo) allocateInteger(processHeap(p), count)};
}

ReturnStatus g__str_len(processPo p, ptrPo tos) {
  integer len;
  const char *str = charsVal(tos[0], &len);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(processHeap(p), len)};
}

ReturnStatus g__str2flt(processPo p, ptrPo tos) {
  integer len;
  const char *str = charsVal(tos[0], &len);
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
  const char *str = charsVal(tos[0], &len);
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
  const char *str = charsVal(tos[0], &len);
  char rnd[MAXLINE];

  strMsg(rnd, NumberOf(rnd), "%S%d", str, minimum(len, NumberOf(rnd) - INT64_DIGITS), randomInt());

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateChars(processHeap(p), rnd, uniStrLen(rnd))};
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
    .result=(termPo) allocateChars(processHeap(p), buff, oLen)};

  closeFile(O_IO(strb));
  return result;
}

ReturnStatus g__str_quote(processPo p, ptrPo tos) {
  termPo t = tos[0];

  strBufferPo strb = newStringBuffer();
  retCode ret = quoteChars(O_IO(strb), C_CHARS(t));

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus result = {.ret=ret,
    .result=(termPo) allocateChars(processHeap(p), buff, oLen)};

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

ReturnStatus g__str_format(processPo p, ptrPo tos) {
  integer fLen;
  const char *fmt = charsVal(tos[1], &fLen);
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
  const char *txt = charsVal(tos[0], &txtLen);
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
  termPo Arg1 = tos[0];
  charsPo str = C_CHARS(tos[0]);
  integer len = charsLength(str);
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

  termPo result = (termPo) allocateChars(processHeap(p), buff, oLen);

  closeFile(O_IO(strb));

  return (ReturnStatus) {.ret=Ok, .result=result};
}

ReturnStatus g__str_find(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  termPo Arg3 = tos[2];
  integer len;
  const char *str = charsVal(Arg1, &len);
  integer tlen;
  const char *tgt = charsVal(Arg2, &tlen);
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
  const char *str = charsVal(Arg1, &len);
  integer start = integerVal(Arg2);
  integer count = integerVal(Arg3);

  char buff[count + 1];
  uniNCpy(buff, count + 1, &str[start], count);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateChars(processHeap(p), buff, count)};
}

ReturnStatus g__str_hdtl(processPo p, ptrPo tos) {
  charsPo src = C_CHARS(tos[0]);
  integer len = charsLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);
  heapPo H = processHeap(p);

  integer offset = 0;
  codePoint ch;
  retCode ret = nxtPoint(str, &offset, len, &ch);

  if (ret == Ok) {
    termPo chCode = allocateInteger(H, ch);
    int mark = gcAddRoot(H, &chCode);
    termPo rest = allocateChars(H, &str[offset], len - offset);
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
  charsPo src = C_CHARS(tos[1]);
  integer len = charsLength(src);
  integer offset = 0;
  char str[len + 16];
  appendCodePoint(str, &offset, len + 16, (codePoint) ch);
  retCode ret = copyChars2Buff(src, &str[offset], len + 16);
  heapPo H = processHeap(p);

  return (ReturnStatus) {.ret=ret,
    .result=(termPo) allocateChars(processHeap(p), str, offset + len)};
}

ReturnStatus g__code2str(processPo p, ptrPo tos) {
  integer ch = integerVal(tos[0]);
  integer codeLength = 0;
  char str[16];
  appendCodePoint(str, &codeLength, NumberOf(str), (codePoint) ch);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateChars(processHeap(p), str, codeLength)};
}

ReturnStatus g__str_apnd(processPo p, ptrPo tos) {
  integer ch = integerVal(tos[1]);
  charsPo src = C_CHARS(tos[0]);
  integer len = charsLength(src);
  integer offset = len;
  char str[len + 16];
  copyChars2Buff(src, str, len + 16);
  heapPo H = processHeap(p);

  retCode ret = appendCodePoint(str, &offset, len + 16, (codePoint) integerVal(tos[0]));

  return (ReturnStatus) {.ret=ret,
    .result=(termPo) allocateChars(processHeap(p), str, offset)};
}

ReturnStatus g__str_back(processPo p, ptrPo tos) {
  charsPo src = C_CHARS(tos[0]);
  integer len = charsLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);
  heapPo H = processHeap(p);

  integer offset = len;
  codePoint ch;
  retCode ret = prevPoint(str, &offset, &ch);

  if (ret == Ok) {
    termPo chCode = allocateInteger(H, ch);
    int mark = gcAddRoot(H, (ptrPo) &chCode);
    termPo rest = allocateChars(H, str, offset);
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
  const char *str = charsVal(Arg1, &len);
  integer start = integerVal(Arg2);

  char buff[len];
  uniNCpy(buff, len, str, len);

  heapPo H = processHeap(p);
  normalPo pair = allocateTpl(H, 2);
  int root = gcAddRoot(H, (ptrPo) &pair);

  termPo lhs = (termPo) allocateChars(H, buff, start);
  setArg(pair, 0, lhs);

  termPo rhs = (termPo) allocateChars(H, &buff[start], len - start);
  setArg(pair, 1, rhs);

  gcReleaseRoot(H, root);
  return (ReturnStatus) {.ret=Ok, .result=(termPo) pair};
}

ReturnStatus g__str_concat(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen;
  const char *lhs = charsVal(Arg1, &llen);
  integer rlen;
  const char *rhs = charsVal(Arg2, &rlen);

  integer len = llen + rlen + 1;
  char buff[len];
  uniNCpy(buff, len, lhs, llen);
  uniNCpy(&buff[llen], len - llen, rhs, rlen);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateChars(processHeap(p), buff, llen + rlen)};
}

ReturnStatus g__str_splice(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  integer from = integerVal(tos[1]);
  integer cnt = integerVal(tos[2]);
  termPo Arg4 = tos[3];

  integer llen;
  const char *lhs = charsVal(Arg1, &llen);
  integer rlen;
  const char *rhs = charsVal(Arg4, &rlen);

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
  uniNCpy(buff, len, lhs, from);
  uniNCpy(&buff[from], len - from, rhs, rlen);
  uniNCpy(&buff[from + rlen], len - from - rlen, &lhs[from + cnt], llen - from - cnt);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateChars(processHeap(p), buff, len)};
}

ReturnStatus g__str_start(processPo p, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  integer llen;
  const char *lhs = charsVal(Arg1, &llen);
  integer rlen;
  const char *rhs = charsVal(Arg2, &rlen);

  return (ReturnStatus) {.ret=Ok,
    .result=(uniIsPrefix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum)};
}

retCode flatten(strBufferPo str, termPo t) {
  if (isNormalPo(t)) {
    normalPo ss = C_NORMAL(t);
    integer cx = termArity(ss);
    retCode ret = Ok;
    for (integer ix = 0; ret == Ok && ix < cx; ix++) {
      ret = flatten(str, nthArg(ss, ix));
    }
    return ret;
  } else if (isChars(t)) {
    integer len;
    const char *elTxt = charsVal(t, &len);
    return outText(O_IO(str), elTxt, len);
  } else
    return Error;
}

ReturnStatus g__str_multicat(processPo p, ptrPo tos) {
  strBufferPo strb = newStringBuffer();

  retCode ret = flatten(strb, tos[0]);

  integer oLen;
  const char *buff = getTextFromBuffer(strb, &oLen);

  ReturnStatus rt = {.ret=ret,
    .result=(termPo) allocateChars(processHeap(p), buff, oLen)};
  closeFile(O_IO(strb));
  return rt;
}

ReturnStatus g__str_fltn(processPo p, ptrPo tos) {
  strBufferPo str = newStringBuffer();

  retCode ret = flatten(str, tos[0]);

  if (ret == Ok) {
    integer oLen;
    const char *buff = getTextFromBuffer(str, &oLen);

    ReturnStatus rt = {.ret=Ok, .result=(termPo) allocateChars(processHeap(p), buff, oLen)};
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
  const char *lhs = charsVal(Arg1, &len);

  char buff[len];
  uniNCpy(buff, len, lhs, len);

  uniReverse(buff, len);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateChars(processHeap(p), buff, len)};
}
