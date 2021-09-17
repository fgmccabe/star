//
// Created by Francis McCabe on 3/4/18.
//

#include <assert.h>
#include <string.h>
#include "charsP.h"

static long chrsSize(specialClassPo cl, termPo o);
static termPo chrsCopy(specialClassPo cl, termPo dst, termPo src);
static termPo chrsScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical chrsCmp(specialClassPo cl, termPo o1, termPo o2);
static integer chrsHash(specialClassPo cl, termPo o);
static retCode chrsDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo chrsFinalizer(specialClassPo class, termPo o);

SpecialClass CharsClass = {
  .clss = Null,
  .sizeFun = chrsSize,
  .copyFun = chrsCopy,
  .scanFun = chrsScan,
  .finalizer = chrsFinalizer,
  .compFun = chrsCmp,
  .hashFun = chrsHash,
  .dispFun = chrsDisp
};

clssPo charsClass = (clssPo) &CharsClass;

void initChars() {
  CharsClass.clss = specialClass;
}

charsPo C_CHARS(termPo t) {
  assert(hasClass(t, charsClass));
  return (charsPo) t;
}

const char *charsVal(termPo o, integer *size) {
  charsPo str = C_CHARS(o);
  *size = str->length;
  return str->txt;
}

termPo allocateChars(heapPo H, const char *txt, long length) {
  charsPo str = (charsPo) allocateObject(H, charsClass, CharsCellCount(length));

  str->clss = charsClass;
  str->hash = 0;
  str->length = length;

  memmove(str->txt, txt, length * sizeof(char));

  return (termPo) str;
}

termPo allocateCString(heapPo H, const char *txt) {
  return allocateChars(H, txt, uniStrLen(txt));
}

termPo allocateFromStrBuffer(strBufferPo bffr, heapPo H) {
  integer oLen;
  const char *buff = getTextFromBuffer(bffr, &oLen);

  return allocateChars(H, buff, oLen);
}

long chrsSize(specialClassPo cl, termPo o) {
  return CharsCellCount(C_CHARS(o)->length);
}

termPo chrsCopy(specialClassPo cl, termPo dst, termPo src) {
  charsPo si = C_CHARS(src);
  charsPo di = (charsPo) dst;
  *di = *si;

  memcpy(di->txt, si->txt, si->length);

  return ((termPo) di) + CharsCellCount(si->length);
}

termPo chrsScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  charsPo str = C_CHARS(o);

  return o + CharsCellCount(str->length);
}

termPo chrsFinalizer(specialClassPo class, termPo o) {
  charsPo str = C_CHARS(o);

  return o + CharsCellCount(str->length);
}

logical chrsCmp(specialClassPo cl, termPo o1, termPo o2) {
  integer l1, l2;
  const char *tx1 = charsVal(o1, &l1);
  const char *tx2 = charsVal(o2, &l2);

  return uniSame(tx1, l1, tx2, l2);
}

logical sameCharSeqs(charsPo s1, charsPo s2) {
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

retCode chrsDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  charsPo str = C_CHARS(t);

  retCode ret = outStr(out, "0\"");

  if (ret == Ok)
    ret = processUnicodes(str->txt, str->length, cpDisp, out);

  if (ret == Ok)
    ret = outChar(out, '"');
  return ret;
}

retCode quoteChars(ioPo out, charsPo str) {
  return processUnicodes(str->txt, str->length, cpDisp, out);
}

integer chrsHash(specialClassPo cl, termPo o) {
  return charsHash(C_CHARS(o));
}

integer charsHash(charsPo str) {
  if (str->hash == 0) {
    str->hash = uniNHash(str->txt, str->length);
  }
  return str->hash;
}

integer charsLength(charsPo str) {
  return str->length;
}

retCode copyChars2Buff(charsPo str, char *buffer, integer buffLen) {
  if (str->length <= buffLen) {
    uniNCpy(buffer, buffLen, str->txt, str->length);
    return Ok;
  } else
    return Fail;
}
