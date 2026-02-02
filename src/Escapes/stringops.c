//
// Created by Francis McCabe on 3/8/18.
//

#include <stringsP.h>
#include <arithP.h>
#include <assert.h>
#include <ctype.h>
#include <tpl.h>
#include <globals.h>
#include <stdlib.h>
#include "arithmetic.h"
#include "consP.h"
#include "option.h"
#include "charP.h"
#include "errorCodes.h"

ValueReturn s__chr_eq(enginePo P, termPo l, termPo r)
{
  return normalReturn(charVal(l)==charVal(r)?trueEnum:falseEnum);
}

ReturnStatus g__chr_eq(enginePo P)
{
  codePoint lhs = charVal(popVal(P));
  codePoint rhs = charVal(popVal(P));
  pshVal(P, lhs == rhs ? trueEnum : falseEnum);
  return Normal;
}

ValueReturn s__chr_lt(enginePo P, termPo l, termPo r)
{
  return normalReturn(charVal(l)<charVal(r)?trueEnum:falseEnum);
}

ReturnStatus g__chr_lt(enginePo P)
{
  codePoint lhs = charVal(popVal(P));
  codePoint rhs = charVal(popVal(P));
  pshVal(P, lhs < rhs ? trueEnum : falseEnum);
  return Normal;
}

ValueReturn s__chr_ge(enginePo P, termPo l, termPo r)
{
  return normalReturn(charVal(l)>=charVal(r)?trueEnum:falseEnum);
}

ReturnStatus g__chr_ge(enginePo P)
{
  codePoint lhs = charVal(popVal(P));
  codePoint rhs = charVal(popVal(P));
  pshVal(P, lhs >= rhs ? trueEnum : falseEnum);
  return Normal;
}

ValueReturn s__chr_hash(enginePo P, termPo l)
{
  return normalReturn(makeInteger(charVal(l)));
}

ReturnStatus g__chr_hash(enginePo P)
{
  codePoint lhs = charVal(popVal(P));
  pshVal(P, makeInteger(lhs));
  return Normal;
}

ValueReturn s__chr_quote(enginePo P, termPo c)
{
  strBufferPo strb = newStringBuffer();
  qtChar(O_IO(strb), charVal(c));

  termPo s = allocateFromStrBuffer(processHeap(P), strb);

  closeIo(O_IO(strb));
  return normalReturn(s);
}

ReturnStatus g__chr_quote(enginePo P)
{
  ValueReturn ret = s__chr_quote(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

// Support formatting of char values
ValueReturn s__chr_format(enginePo P, termPo c, termPo f)
{
  codePoint cp = charVal(c);
  integer fLen;
  const char* fmt = strVal(f, &fLen);
  strBufferPo strb = newStringBuffer();
  retCode ret;

  // Allowed formats are q, c, x, 0, 9
  if (fLen == 1){
    switch (fmt[0]){
    case 'q': {
      ret = qtChar(O_IO(strb), cp);
      break;
    }
    case 'c': {
      ret = outChar(O_IO(strb), cp);
      break;
    }
    case 'x': {
      ret = outMsg(O_IO(strb), "%x", (integer)cp);
      break;
    }
    default: {
      ret = outMsg(O_IO(strb), "%d", (integer)cp);
      break;
    }
    }

    if (ret == Ok){
      termPo r = allocateFromStrBuffer(processHeap(P), strb);
      closeIo(O_IO(strb));
      return normalReturn(r);
    }
    else{
      closeIo(O_IO(strb));
      return abnormalReturn(eINVAL);
    }
  }
  else{
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__chr_format(enginePo P)
{
  termPo c = popVal(P);
  termPo f = popVal(P);
  ValueReturn ret = s__chr_format(P, c, f);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_eq(enginePo P, termPo l, termPo r)
{
  stringPo s1 = C_STR(l);
  stringPo s2 = C_STR(r);

  logical eq = stringHash(s1) == stringHash(s2) && sameString(s1, s2);
  return normalReturn((eq ? trueEnum : falseEnum));
}

ReturnStatus g__str_eq(enginePo P)
{
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__str_eq(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

// Lexicographic comparison
ValueReturn s__str_lt(enginePo P, termPo l, termPo r)
{
  integer llen, rlen;
  const char* lhs = strVal(l, &llen);
  const char* rhs = strVal(r, &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen){
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2){
      return normalReturn(trueEnum);
    }
    else if (chl > ch2){
      return normalReturn(falseEnum);
    }
  }
  if (ri < rlen)
    // There is more on the right, so the left counts as being smaller
    return normalReturn(trueEnum);
  else
    return normalReturn(falseEnum);
}

ReturnStatus g__str_lt(enginePo P)
{
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__str_lt(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_ge(enginePo P, termPo l, termPo r)
{
  integer llen, rlen;
  const char* lhs = strVal(l, &llen);
  const char* rhs = strVal(r, &rlen);

  integer li = 0;
  integer ri = 0;

  while (li < llen && ri < rlen){
    codePoint chl = nextCodePoint(lhs, &li, llen);
    codePoint ch2 = nextCodePoint(rhs, &ri, rlen);

    if (chl < ch2){
      return normalReturn(falseEnum);
    }
    else if (chl > ch2){
      return normalReturn(trueEnum);
    }
  }
  if (li <= llen){
    // There is more on the left, so it counts as being bigger
    return normalReturn(trueEnum);
  }
  else{
    return normalReturn(falseEnum);
  }
}

ReturnStatus g__str_ge(enginePo P)
{
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__str_ge(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_hash(enginePo P, termPo s)
{
  stringPo lhs = C_STR(s);

  if (lhs->hash == 0){
    integer len;
    const char* str = strVal(s, &len);
    lhs->hash = uniNHash(str, len);
  }

  return normalReturn(makeInteger(lhs->hash));
}

ReturnStatus g__str_hash(enginePo P)
{
  termPo r = popVal(P);
  ValueReturn ret = s__str_hash(P, r);
  pshVal(P, ret.value);
  return ret.status;
}

static retCode skipBlanks(const char* txt, integer len, integer* pos)
{
  while ((*pos) < len && isspace(txt[*pos])) (*pos)++;
  if (*pos == len)
    return Ok;
  else
    return Error;
}

ValueReturn s__str2flt(enginePo P, termPo s)
{
  integer len;
  const char* str = strVal(s, &len);
  double flt;

  switch (parseDouble(str, len, &flt)){
  case Ok:
    return normalReturn(makeFloat(flt));
  default:
  case Error:
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__str2flt(enginePo P)
{
  termPo r = popVal(P);
  ValueReturn ret = s__str2flt(P, r);
  pshVal(P, ret.value);
  return ret.status;
}

ReturnStatus g__str2int(enginePo P)
{
  integer len;
  const char* str = strVal(popVal(P), &len);
  integer ix;

  integer pos = 0;
  switch (parseInteger(str, &pos, len, &ix)){
  case Ok:
    if (skipBlanks(str, len, &pos) == Ok){
      pshVal(P, makeInteger(ix));
      return Normal;
    }
  default:
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__str_charat(enginePo P)
{
  integer len;
  const char* str = strVal(popVal(P), &len);
  integer ix = integerVal(popVal(P));

  if (ix < 0 || ix >= len){
    pshVal(P, eRANGE);
    return Abnormal;
  }
  else{
    codePoint cp;
    retCode ret = uniCharAt(str, len, ix, &cp);
    if (ret == Ok){
      pshVal(P, makeChar(cp));
      return Normal;
    }
    else{
      pshVal(P, eRANGE);
      return Abnormal;
    }
  }
}

ReturnStatus g__str_gen(enginePo P)
{
  integer len;
  const char* str = strVal(popVal(P), &len);
  char rnd[MAXLINE];

  strMsg(rnd, NumberOf(rnd), "%S%d", str, minimum(len, NumberOf(rnd) - INT64_DIGITS), randomInt());

  pshVal(P, allocateString(processHeap(P), rnd, uniStrLen(rnd)));
  return Normal;
}

ReturnStatus g__stringOf(enginePo P)
{
  termPo a1 = popVal(P);
  integer depth = integerVal(popVal(P));

  strBufferPo strb = newStringBuffer();
  dispTerm(O_IO(strb), a1, 0, depth, False);

  integer oLen;
  const char* buff = getTextFromBuffer(strb, &oLen);

  pshVal(P, allocateString(processHeap(P), buff, oLen));

  closeIo(O_IO(strb));
  return Normal;
}

ReturnStatus g__str_quote(enginePo P)
{
  strBufferPo strb = newStringBuffer();
  quoteStrg(O_IO(strb), C_STR(popVal(P)));

  integer oLen;
  const char* buff = getTextFromBuffer(strb, &oLen);

  pshVal(P, allocateString(processHeap(P), buff, oLen));
  closeIo(O_IO(strb));
  return Normal;
}

typedef enum
{
  alignLeft,
  alignCenter,
  alignRight
} Alignment;

typedef enum
{
  leftToRight,
  rightToLeft
} SrcAlign;

ReturnStatus g__str_format(enginePo P)
{
  termPo a1 = popVal(P);
  termPo a2 = popVal(P);
  integer fLen;
  const char* fmt = strVal(a2, &fLen);
  Alignment alignment = alignLeft;
  SrcAlign sourceAlign = leftToRight;
  integer width = 0;
  integer srcWidth = 0;
  codePoint pad = ' ';

  integer fPos = 0;

  // How much of the source are we taking?
  parseInteger(fmt, &fPos, fLen, &srcWidth);

  if (srcWidth < 0){
    sourceAlign = rightToLeft;
    srcWidth = -srcWidth;
  }

  // What is the output alignment?
  codePoint f = nextCodePoint(fmt, &fPos, fLen);
  switch (f){
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
  default: ;
  }

  // What is the output width?
  parseInteger(fmt, &fPos, fLen, &width);

  if (width < 0){
    width = -width;
  }

  if (fPos < fLen){
    pad = nextCodePoint(fmt, &fPos, fLen);
  }

  strBufferPo strb = newStringBuffer();

  integer txtLen = 0;
  const char* txt = strVal(a1, &txtLen);
  integer txtPos = 0;

  integer txtQ = (width == 0 ? txtLen : minimum(width, txtLen));

  if (srcWidth != 0)
    txtQ = minimum(txtQ, srcWidth);

  switch (sourceAlign){
  default:
  case leftToRight:
    txtPos = 0;
    break;
  case rightToLeft:
    txtPos = maximum(txtLen - txtQ, 0);
    break;
  }

  switch (alignment){
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

ReturnStatus g__explode(enginePo P)
{
  stringPo str = C_STR(popVal(P));
  integer len = strLength(str);
  char buffer[len + 1];

  copyChars2Buff(str, buffer, len + 1);
  heapPo h = processHeap(P);
  termPo list = (termPo)nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(h, (ptrPo)&list);
  gcAddRoot(h, &el);

  integer pos = len;
  retCode ret = Ok;
  while (ret == Ok && pos > 0){
    codePoint cp;
    ret = prevPoint(buffer, &pos, &cp);
    if (ret == Ok){
      el = makeChar(cp);
      list = (termPo)allocateCons(h, el, list);
    }
  }

  gcReleaseRoot(h, root);

  assert(consLength(list) == countCodePoints(buffer, 0, len));

  pshVal(P, list);
  return Normal;
}

void dS(termPo w)
{
  termPo s = w;

  while (isCons(s)){
    integer cp = integerVal(consHead(C_NORMAL(s)));
    outChar(logFile, (codePoint)cp);
    s = consTail(C_NORMAL(s));
  }
  outStr(logFile, "\n");
  flushOut();
}

ReturnStatus g__implode(enginePo P)
{
  termPo list = popVal(P);

  integer size = 1;
  for (termPo lst = list; isCons(lst); lst = consTail(C_NORMAL(lst))){
    normalPo en = C_NORMAL(lst);
    size += codePointSize(charVal(consHead(en)));
  }

  char buff[MAXLINE];
  char* buffer = (size > MAXLINE ? (char*)malloc(sizeof(char) * size) : buff);
  integer pos = 0;

  while (isCons(list)){
    normalPo pr = C_NORMAL(list);
    codePoint ch = charVal(consHead(pr));
    appendCodePoint(buffer, &pos, size, ch);
    list = consTail(pr);
  }

  termPo result = (termPo)allocateString(processHeap(P), buffer, pos);

  if (buffer != buff)
    free(buffer);
  pshVal(P, result);
  return Normal;
}

ReturnStatus g__str_find(enginePo P)
{
  integer len;
  const char* str = strVal(popVal(P), &len);
  integer tlen;
  const char* tgt = strVal(popVal(P), &tlen);
  integer start = integerVal(popVal(P));

  integer found = uniSearch(str, len, start, tgt, tlen);
  pshVal(P, makeInteger(found));
  return Normal;
}

ReturnStatus g__sub_str(enginePo P)
{
  integer len;
  const char* str = strVal(popVal(P), &len);
  integer start = integerVal(popVal(P));
  integer count = integerVal(popVal(P));

  count = minimum(count, len - start);

  char buff[count + 1];
  uniMove(buff, count + 1, &str[start], count);
  pshVal(P, allocateString(processHeap(P), buff, count));
  return Normal;
}

ReturnStatus g__str_hdtl(enginePo P)
{
  stringPo src = C_STR(popVal(P));
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);

  integer offset = 0;
  codePoint ch;
  retCode ret = nxtPoint(str, &offset, len, &ch);
  heapPo h = processHeap(P);

  if (ret == Ok){
    termPo chCode = makeChar(ch);
    int mark = gcAddRoot(h, &chCode);
    termPo rest = allocateString(h, &str[offset], len - offset);
    gcAddRoot(h, &rest);
    normalPo pair = allocateTpl(h, 2);
    setArg(pair, 0, chCode);
    setArg(pair, 1, rest);
    gcReleaseRoot(h, mark);
    pshVal(P, (termPo)wrapSome(h, (termPo)pair));
    return Normal;
  }
  else{
    pshVal(P, noneEnum);
    return Normal;
  }
}

ReturnStatus g__str_cons(enginePo P)
{
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

ReturnStatus g__code2str(enginePo P)
{
  codePoint ch = charVal(popVal(P));
  integer codeLength = 0;
  char str[16];
  appendCodePoint(str, &codeLength, NumberOf(str), (codePoint)ch);

  pshVal(P, allocateString(processHeap(P), str, codeLength));
  return Normal;
}

ReturnStatus g__str_apnd(enginePo P)
{
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

ReturnStatus g__str_set(enginePo P)
{
  integer len;
  const char* src = strVal(popVal(P), &len);
  integer off = integerVal(popVal(P));
  codePoint ch = charVal(popVal(P));
  integer offset = minimum(off, len + 2);
  char str[len + 16];
  uniNCpy(str, len + 16, src, offset);
  integer srcOffset = offset;
  appendCodePoint(str, &offset, len + 16, ch);
  nextCodePoint(src, &srcOffset, len);
  uniNCpy(&str[offset], len + 16 - offset, &src[srcOffset], len - srcOffset);

  pshVal(P, allocateCString(processHeap(P), str));
  return Normal;
}

ReturnStatus g__str_drop(enginePo P)
{
  integer len;
  const char* src = strVal(popVal(P), &len);
  integer offset = integerVal(popVal(P));
  char str[len + 16];
  uniNCpy(str, len + 16, src, offset);
  integer srcOffset = offset;
  nextCodePoint(src, &offset, len);
  uniNCpy(&str[srcOffset], len + 16 - srcOffset, &src[offset], len - offset);

  pshVal(P, allocateCString(processHeap(P), str));
  return Normal;
}

ReturnStatus g__str_back(enginePo P)
{
  stringPo src = C_STR(popVal(P));
  integer len = strLength(src);
  char str[len + 1];
  copyChars2Buff(src, str, len + 1);

  integer offset = len;
  codePoint ch;
  retCode ret = prevPoint(str, &offset, &ch);

  if (ret == Ok){
    termPo chCode = makeChar(ch);
    heapPo h = processHeap(P);
    int mark = gcAddRoot(h, (ptrPo)&chCode);
    termPo rest = allocateString(h, str, offset);
    gcAddRoot(h, &rest);
    normalPo pair = allocateTpl(h, 2);
    setArg(pair, 0, rest);
    setArg(pair, 1, chCode);
    gcReleaseRoot(h, mark);
    pshVal(P, (termPo)pair);
    return Normal;
  }
  else{
    pshVal(P, eNOTFND);
    return Abnormal;
  }
}

ReturnStatus g__str_split(enginePo P)
{
  integer len;
  const char* str = strVal(popVal(P), &len);
  integer start = integerVal(popVal(P));

  char buff[len];
  uniMove(buff, len, str, len);
  heapPo h = processHeap(P);
  normalPo pair = allocateTpl(h, 2);
  int root = gcAddRoot(h, (ptrPo)&pair);

  termPo lhs = (termPo)allocateString(h, buff, start);
  setArg(pair, 0, lhs);

  termPo rhs = (termPo)allocateString(h, &buff[start], len - start);
  setArg(pair, 1, rhs);

  gcReleaseRoot(h, root);
  pshVal(P, (termPo)pair);
  return Normal;
}

ReturnStatus g__str_concat(enginePo P)
{
  integer llen;
  const char* lhs = strVal(popVal(P), &llen);
  integer rlen;
  const char* rhs = strVal(popVal(P), &rlen);

  integer len = llen + rlen + 1;
  char buff[len];
  uniMove(buff, len, lhs, llen);
  uniMove(&buff[llen], len - llen, rhs, rlen);

  pshVal(P, allocateString(processHeap(P), buff, llen + rlen));
  return Normal;
}

ReturnStatus g__str_splice(enginePo P)
{
  integer llen;
  const char* lhs = strVal(popVal(P), &llen);
  integer from = integerVal(popVal(P));
  integer cnt = integerVal(popVal(P));
  integer rlen;
  const char* rhs = strVal(popVal(P), &rlen);

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

ReturnStatus g__str_start(enginePo P)
{
  integer llen;
  const char* lhs = strVal(popVal(P), &llen);
  integer rlen;
  const char* rhs = strVal(popVal(P), &rlen);
  pshVal(P, (uniIsPrefix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum));
  return Normal;
}

ReturnStatus g__str_end(enginePo P)
{
  integer llen;
  const char* lhs = strVal(popVal(P), &llen);
  integer rlen;
  const char* rhs = strVal(popVal(P), &rlen);

  pshVal(P, (uniIsSuffix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum));
  return Normal;
}

ReturnStatus g__str_multicat(enginePo P)
{
  termPo t = popVal(P);
  strBufferPo strb = newStringBuffer();

  retCode ret = Ok;
  while (ret == Ok && isCons(t)){
    normalPo c = C_NORMAL(t);
    integer len;
    const char* elTxt = strVal(consHead(c), &len);
    ret = outText(O_IO(strb), elTxt, len);
    t = consTail(c);
  }
  integer oLen;
  const char* buff = getTextFromBuffer(strb, &oLen);
  pshVal(P, allocateString(processHeap(P), buff, oLen));
  closeIo(O_IO(strb));
  return Normal;
}

ReturnStatus g__str_reverse(enginePo P)
{
  integer len;
  const char* lhs = strVal(popVal(P), &len);

  char buff[len];
  uniMove(buff, len, lhs, len);

  uniReverse(buff, len);

  pshVal(P, allocateString(processHeap(P), buff, len));

  return Normal;
}
