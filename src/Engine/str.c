//
// Created by Francis McCabe on 3/4/18.
//

#include <assert.h>
#include <string.h>
#include "strP.h"

static long strSize(specialClassPo cl, termPo o);
static termPo strCopy(specialClassPo cl, termPo dst, termPo src);
static termPo strScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical strCmp(specialClassPo cl, termPo o1, termPo o2);
static integer strHash(specialClassPo cl, termPo o);
static retCode strDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass StringClass = {
  .clss = Null,
  .sizeFun = strSize,
  .copyFun = strCopy,
  .scanFun = strScan,
  .compFun = strCmp,
  .hashFun = strHash,
  .dispFun = strDisp
};

clssPo stringClass = (clssPo) &StringClass;

void initStr() {
  StringClass.clss = specialClass;
}

stringPo C_STR(termPo t) {
  assert(hasClass(t, stringClass));
  return (stringPo) t;
}

const char *stringVal(termPo t, integer *size) {
  stringPo str = C_STR(t);
  *size = str->length;
  return str->txt;
}

termPo allocateString(heapPo H, const char *txt, long length) {
  stringPo str = (stringPo) allocateObject(H, stringClass, StringCellCount(length));

  str->clss = stringClass;
  str->hash = 0;
  str->length = length;

  memmove(str->txt, txt, length * sizeof(char));

  return (termPo) str;
}

termPo allocateCString(heapPo H, const char *txt) {
  return allocateString(H, txt, uniStrLen(txt));
}

long strSize(specialClassPo cl, termPo o) {
  return StringCellCount(C_STR(o)->length);
}

termPo strCopy(specialClassPo cl, termPo dst, termPo src) {
  stringPo si = C_STR(src);
  stringPo di = (stringPo) dst;
  *di = *si;

  memcpy(di->txt, si->txt, si->length);

  return ((termPo) di) + StringCellCount(si->length);
}

termPo strScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  stringPo str = C_STR(o);

  return o + StringCellCount(str->length);
}

logical strCmp(specialClassPo cl, termPo o1, termPo o2) {
  integer l1, l2;
  const char *tx1 = stringVal(o1, &l1);
  const char *tx2 = stringVal(o2, &l2);

  return uniSame(tx1, l1, tx2, l2);
}

logical sameString(stringPo s1, stringPo s2) {
  return uniSame(s1->txt, s1->length, s2->txt, s2->length);
}

static retCode qtChar(ioPo f, codePoint ch) {
  retCode ret;
  switch (ch) {
    case '\a':
      ret = outStr(f, "\\a");
      break;
    case '\b':
      ret = outStr(f, "\\b");
      break;
    case '\x7f':
      ret = outStr(f, "\\d");
      break;
    case '\x1b':
      ret = outStr(f, "\\e");
      break;
    case '\f':
      ret = outStr(f, "\\f");
      break;
    case '\n':
      ret = outStr(f, "\\n");
      break;
    case '\r':
      ret = outStr(f, "\\r");
      break;
    case '\t':
      ret = outStr(f, "\\t");
      break;
    case '\v':
      ret = outStr(f, "\\v");
      break;
    case '\\':
      ret = outStr(f, "\\\\");
      break;
    case '\"':
      ret = outStr(f, "\\\"");
      break;
    default:
      ret = outChar(f, ch);
  }
  return ret;
}

static retCode cpDisp(codePoint ch, integer ix, void *cl) {
  return qtChar(O_IO(cl), ch);
}

retCode strDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  stringPo str = C_STR(t);

  retCode ret = outChar(out, '"');

  if (ret == Ok)
    ret = processUnicodes(str->txt, str->length, cpDisp, out);

  if (ret == Ok)
    ret = outChar(out, '"');
  return ret;
}

integer strHash(specialClassPo cl, termPo o) {
  return stringHash(C_STR(o));
}

integer stringHash(stringPo str) {
  if (str->hash == 0) {
    str->hash = uniNHash(str->txt, str->length);
  }
  return str->hash;
}

integer stringLength(stringPo str) {
  return str->length;
}

retCode copyString2Buff(stringPo str, char *buffer, integer buffLen) {
  if (str->length <= buffLen) {
    uniNCpy(buffer, buffLen, str->txt, str->length);
    return Ok;
  } else
    return Fail;
}
