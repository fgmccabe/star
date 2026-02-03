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

ValueReturn s__str2int(enginePo P, termPo s)
{
  integer len;
  const char* str = strVal(s, &len);
  integer ix;

  integer pos = 0;
  switch (parseInteger(str, &pos, len, &ix)){
  case Ok:
    if (skipBlanks(str, len, &pos) == Ok){
      return normalReturn(makeInteger(ix));
    }
  default:
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__str2int(enginePo P)
{
  termPo r = popVal(P);
  ValueReturn ret = s__str2int(P, r);
  pshVal(P, ret.value);
  return ret.status;
}


ValueReturn s__str_charat(enginePo P, termPo s, termPo i)
{
  integer len;
  const char* str = strVal(s, &len);
  integer ix = integerVal(i);

  if (ix < 0 || ix >= len){
    return abnormalReturn(eRANGE);
  }
  else{
    codePoint cp;
    retCode ret = uniCharAt(str, len, ix, &cp);
    if (ret == Ok){
      return normalReturn(makeChar(cp));
    }
    else{
      return abnormalReturn(eRANGE);
    }
  }
}

ReturnStatus g__str_charat(enginePo P)
{
  termPo s = popVal(P);
  termPo i = popVal(P);

  ValueReturn ret = s__str_charat(P, s, i);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_gen(enginePo P, termPo s)
{
  integer len;
  const char* str = strVal(s, &len);
  char rnd[MAXLINE];

  strMsg(rnd, NumberOf(rnd), "%S%d", str, minimum(len, NumberOf(rnd) - INT64_DIGITS), randomInt());

  return normalReturn(allocateString(processHeap(P), rnd, uniStrLen(rnd)));
}

ReturnStatus g__str_gen(enginePo P)
{
  termPo p = popVal(P);

  ValueReturn ret = s__str_gen(P, p);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__stringOf(enginePo P, termPo t, termPo d)
{
  integer depth = integerVal(d);

  strBufferPo strb = newStringBuffer();
  dispTerm(O_IO(strb), t, 0, depth, False);

  integer oLen;
  const char* buff = getTextFromBuffer(strb, &oLen);

  termPo text = allocateString(processHeap(P), buff, oLen);

  closeIo(O_IO(strb));
  return normalReturn(text);
}

ReturnStatus g__stringOf(enginePo P)
{
  termPo t = popVal(P);
  termPo d = popVal(P);

  ValueReturn ret = s__stringOf(P, t, d);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_quote(enginePo P, termPo s)
{
  strBufferPo strb = newStringBuffer();
  quoteStrg(O_IO(strb), C_STR(s));

  integer oLen;
  const char* buff = getTextFromBuffer(strb, &oLen);

  termPo text = allocateString(processHeap(P), buff, oLen);
  closeIo(O_IO(strb));
  return normalReturn(text);
}

ReturnStatus g__str_quote(enginePo P)
{
  termPo t = popVal(P);

  ValueReturn ret = s__str_quote(P, t);
  pshVal(P, ret.value);
  return ret.status;
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

ValueReturn s__str_format(enginePo P, termPo s, termPo f)
{
  integer fLen;
  const char* fmt = strVal(f, &fLen);
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
  codePoint fc = nextCodePoint(fmt, &fPos, fLen);
  switch (fc){
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
  const char* txt = strVal(s, &txtLen);
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

  termPo text = allocateFromStrBuffer(processHeap(P), strb);

  closeIo(O_IO(strb));
  return normalReturn(text);
}

ReturnStatus g__str_format(enginePo P)
{
  termPo s = popVal(P);
  termPo f = popVal(P);

  ValueReturn ret = s__str_format(P, s, f);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__explode(enginePo P, termPo s)
{
  stringPo str = C_STR(s);
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

  return normalReturn(list);
}

ReturnStatus g__explode(enginePo P)
{
  termPo s = popVal(P);

  ValueReturn ret = s__explode(P, s);
  pshVal(P, ret.value);
  return ret.status;
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

ValueReturn s__implode(enginePo P, termPo list)
{
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
  return normalReturn(result);
}

ReturnStatus g__implode(enginePo P)
{
  termPo s = popVal(P);

  ValueReturn ret = s__implode(P, s);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_find(enginePo P, termPo s, termPo t, termPo f)
{
  integer len;
  const char* str = strVal(s, &len);
  integer tlen;
  const char* tgt = strVal(t, &tlen);
  integer start = integerVal(f);

  integer found = uniSearch(str, len, start, tgt, tlen);
  return normalReturn(makeInteger(found));
}

ReturnStatus g__str_find(enginePo P)
{
  termPo s = popVal(P);
  termPo t = popVal(P);
  termPo f = popVal(P);

  ValueReturn ret = s__str_find(P, s, t, f);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__sub_str(enginePo P, termPo s, termPo f, termPo c)
{
  integer len;
  const char* str = strVal(s, &len);
  integer start = integerVal(f);
  integer count = integerVal(c);

  count = minimum(count, len - start);

  char buff[count + 1];
  uniMove(buff, count + 1, &str[start], count);
  return normalReturn(allocateString(processHeap(P), buff, count));
}

ReturnStatus g__sub_str(enginePo P)
{
  termPo s = popVal(P);
  termPo f = popVal(P);
  termPo c = popVal(P);

  ValueReturn ret = s__sub_str(P, s, f, c);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_hdtl(enginePo P, termPo s)
{
  stringPo src = C_STR(s);
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
    return normalReturn((termPo)wrapSome(h, (termPo)pair));
  }
  else{
    return normalReturn(noneEnum);
  }
}

ReturnStatus g__str_hdtl(enginePo P)
{
  termPo s = popVal(P);

  ValueReturn ret = s__str_hdtl(P, s);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_cons(enginePo P, termPo c, termPo s)
{
  codePoint ch = charVal(c);
  stringPo src = C_STR(s);
  integer len = strLength(src);
  integer offset = 0;
  char str[len + 16];
  appendCodePoint(str, &offset, len + 16, ch);
  copyChars2Buff(src, &str[offset], len + 16);
  return normalReturn(allocateString(processHeap(P), str, offset + len));
}

ReturnStatus g__str_cons(enginePo P)
{
  termPo c = popVal(P);
  termPo s = popVal(P);

  ValueReturn ret = s__str_cons(P, c, s);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__code2str(enginePo P, termPo c)
{
  codePoint ch = charVal(c);
  integer codeLength = 0;
  char str[16];
  appendCodePoint(str, &codeLength, NumberOf(str), (codePoint)ch);

  return normalReturn(allocateString(processHeap(P), str, codeLength));
}

ReturnStatus g__code2str(enginePo P)
{
  termPo c = popVal(P);

  ValueReturn ret = s__code2str(P, c);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_apnd(enginePo P, termPo s, termPo c)
{
  stringPo src = C_STR(s);
  codePoint ch = charVal(c);
  integer len = strLength(src);
  integer offset = len;
  char str[len + 16];
  copyChars2Buff(src, str, len + 16);

  appendCodePoint(str, &offset, len + 16, ch);
  return normalReturn(allocateString(processHeap(P), str, offset));
}

ReturnStatus g__str_apnd(enginePo P)
{
  termPo s = popVal(P);
  termPo c = popVal(P);

  ValueReturn ret = s__str_apnd(P, s, c);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_set(enginePo P, termPo s, termPo o, termPo c)
{
  integer len;
  const char* src = strVal(s, &len);
  integer off = integerVal(o);
  codePoint ch = charVal(c);
  integer offset = minimum(off, len + 2);
  char str[len + 16];
  uniNCpy(str, len + 16, src, offset);
  integer srcOffset = offset;
  appendCodePoint(str, &offset, len + 16, ch);
  nextCodePoint(src, &srcOffset, len);
  uniNCpy(&str[offset], len + 16 - offset, &src[srcOffset], len - srcOffset);

  return normalReturn(allocateCString(processHeap(P), str));
}

ReturnStatus g__str_set(enginePo P)
{
  termPo s = popVal(P);
  termPo f = popVal(P);
  termPo c = popVal(P);

  ValueReturn ret = s__str_set(P, s, f, c);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_drop(enginePo P, termPo s, termPo o)
{
  integer len;
  const char* src = strVal(s, &len);
  integer offset = integerVal(o);
  char str[len + 16];
  uniNCpy(str, len + 16, src, offset);
  integer srcOffset = offset;
  nextCodePoint(src, &offset, len);
  uniNCpy(&str[srcOffset], len + 16 - srcOffset, &src[offset], len - offset);

  return normalReturn(allocateCString(processHeap(P), str));
}

ReturnStatus g__str_drop(enginePo P)
{
  termPo s = popVal(P);
  termPo o = popVal(P);

  ValueReturn ret = s__str_drop(P, s, o);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_back(enginePo P, termPo s)
{
  stringPo src = C_STR(s);
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
    return normalReturn((termPo)pair);
  }
  else{
    return abnormalReturn(eNOTFND);
  }
}

ReturnStatus g__str_back(enginePo P)
{
  termPo s = popVal(P);

  ValueReturn ret = s__str_back(P, s);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_split(enginePo P, termPo s, termPo o)
{
  integer len;
  const char* str = strVal(s, &len);
  integer start = integerVal(o);

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
  return normalReturn((termPo)pair);
}

ReturnStatus g__str_split(enginePo P)
{
  termPo s = popVal(P);
  termPo o = popVal(P);

  ValueReturn ret = s__str_split(P, s, o);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_concat(enginePo P, termPo f, termPo s)
{
  integer llen;
  const char* lhs = strVal(f, &llen);
  integer rlen;
  const char* rhs = strVal(s, &rlen);

  integer len = llen + rlen + 1;
  char buff[len];
  uniMove(buff, len, lhs, llen);
  uniMove(&buff[llen], len - llen, rhs, rlen);

  return normalReturn(allocateString(processHeap(P), buff, llen + rlen));
}

ReturnStatus g__str_concat(enginePo P)
{
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__str_concat(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_splice(enginePo P, termPo l, termPo f, termPo c, termPo r)
{
  integer llen;
  const char* lhs = strVal(l, &llen);
  integer from = integerVal(f);
  integer cnt = integerVal(c);
  integer rlen;
  const char* rhs = strVal(r, &rlen);

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

  return normalReturn(allocateString(processHeap(P), buff, len));
}

ReturnStatus g__str_splice(enginePo P)
{
  termPo l = popVal(P);
  termPo f = popVal(P);
  termPo c = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__str_splice(P, l, f, c, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_start(enginePo P, termPo s, termPo p)
{
  integer llen;
  const char* lhs = strVal(s, &llen);
  integer rlen;
  const char* rhs = strVal(p, &rlen);
  return normalReturn(uniIsPrefix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum);
}

ReturnStatus g__str_start(enginePo P)
{
  termPo l = popVal(P);
  termPo p = popVal(P);

  ValueReturn ret = s__str_start(P, l, p);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_end(enginePo P, termPo l, termPo r)
{
  integer llen;
  const char* lhs = strVal(l, &llen);
  integer rlen;
  const char* rhs = strVal(r, &rlen);

  return normalReturn(uniIsSuffix(lhs, llen, rhs, rlen) ? trueEnum : falseEnum);
}

ReturnStatus g__str_end(enginePo P)
{
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__str_end(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_multicat(enginePo P, termPo t)
{
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
  termPo text = allocateString(processHeap(P), buff, oLen);
  closeIo(O_IO(strb));
  return normalReturn(text);
}

ReturnStatus g__str_multicat(enginePo P)
{
  termPo l = popVal(P);

  ValueReturn ret = s__str_multicat(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str_reverse(enginePo P, termPo s)
{
  integer len;
  const char* lhs = strVal(s, &len);

  char buff[len];
  uniMove(buff, len, lhs, len);

  uniReverse(buff, len);

  return normalReturn(allocateString(processHeap(P), buff, len));
}

ReturnStatus g__str_reverse(enginePo P)
{
  termPo l = popVal(P);

  ValueReturn ret = s__str_reverse(P, l);
  pshVal(P, ret.value);
  return ret.status;
}
