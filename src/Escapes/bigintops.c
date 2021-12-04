//
// Created by Francis McCabe on 1/6/18.
// Arithmetic on big numbers


#include <math.h>
#include <strings.h>
#include <tpl.h>
#include <stdlib.h>
#include <errno.h>
#include <globals.h>
#include "ooio.h"
#include "engine.h"
#include "arithP.h"
#include "bignumP.h"
#include "errorCodes.h"
#include "option.h"
#include "consP.h"

ReturnStatus g__big_plus(processPo p, heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longAdd(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  termPo Rs = (termPo) allocateBignum(h, cC, sum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__big_minus(processPo p, heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longSubtract(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  termPo Rs = (termPo) allocateBignum(h, cC, sum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__big_times(processPo p, heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);
  integer pS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 prod[pS];
  integer cC = longMultiply(prod, pS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  termPo Rs = (termPo) allocateBignum(h, cC, prod);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__big_div(processPo p, heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);
  integer qS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 quot[qS];
  uint32 rem[qS];

  integer qC = qS;
  integer rC = qS;

  retCode ret = longDivide(quot, &qC, rem, &rC, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  if (ret == Ok) {
    termPo Qt = (termPo) allocateBignum(h, qC, quot);
    int root = gcAddRoot(h, &Qt);

    termPo Rt = (termPo) allocateBignum(h, rC, rem);
    gcAddRoot(h, &Rt);
    termPo Rs = (termPo) allocatePair(h, Qt, Rt);
    gcReleaseRoot(h, root);
    return (ReturnStatus) {.ret=Ok, .result=Rs};
  } else {
    return (ReturnStatus) {.ret=Error, .result=voidEnum};
  }
}

ReturnStatus g__big_format(heapPo h, termPo a1, termPo a2) {
  bignumPo bg = C_BIGNUM(a1);
  integer bgCount = bigCount(bg);
  uint32 *bgData = bigDigits(bg);

  integer fmtLen;
  const char *fmt = strVal(a2, &fmtLen);

  integer bufLen = bgCount * 16;
  char buff[bufLen];

  integer resLen = longFormat(bgData, bgCount, fmt, fmtLen, buff, bufLen);

  if (resLen >= 0) {
    return (ReturnStatus) {.result = (termPo) allocateString(h, buff, resLen), .ret=Ok};
  } else
    return liberror(h, "_big_format", eINVAL);
}

ReturnStatus g__big2str(heapPo h, termPo a1) {
  bignumPo bg = C_BIGNUM(a1);
  integer bgCount = bigCount(bg);
  uint32 *bgData = bigDigits(bg);

  integer bufLen = bgCount * 16;
  char buff[bufLen];
  integer actual = textFromlong(buff, bufLen, bgData, bgCount);

  if (actual >= 0) {
    return (ReturnStatus) {.result = (termPo) allocateString(h, buff, actual), .ret=Ok};
  } else
    return liberror(h, "_big_format", eINVAL);
}

ReturnStatus g__str2big(heapPo h, termPo a1) {
  integer len;
  const char *str = strVal(a1, &len);
  integer gSize = len / 8;
  uint32 digits[gSize];

  integer bgSize = longFromText(str, len, digits, gSize);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) wrapSome(h, (termPo) allocateBignum(h, bgSize, digits))};
}

ReturnStatus g__big_hash(heapPo h, termPo a1) {
  bignumPo bg = C_BIGNUM(a1);
  termPo Rs = (termPo) allocateInteger(h, bignumHash(bg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__big_eq(heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);

  termPo Rs = (longEqual(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs)) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__big_lt(heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);

  switch (longCompare(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs))) {
    case smaller:
      return (ReturnStatus) {.ret=Ok, .result=trueEnum};
    default:
      return (ReturnStatus) {.ret=Ok, .result=falseEnum};
  }
}

ReturnStatus g__big_ge(heapPo h, termPo a1, termPo a2) {
  bignumPo lhs = C_BIGNUM(a1);
  bignumPo rhs = C_BIGNUM(a2);

  switch (longCompare(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs))) {
    case smaller:
      return (ReturnStatus) {.ret=Ok, .result=falseEnum};
    default:
      return (ReturnStatus) {.ret=Ok, .result=trueEnum};
  }
}

ReturnStatus g__int2big(heapPo h, termPo a1) {
  uint64 U = (uint64) integerVal(a1);

  uint32 uu[] = {U & ONES_MASK, (U >> 32) & ONES_MASK};
  return (ReturnStatus) {.ret=Ok, .result=allocateBignum(h, NumberOf(uu), uu)};
}

ReturnStatus g__big2ints(heapPo h, termPo a1) {
  bignumPo bg = C_BIGNUM(a1);
  integer count = bigCount(bg);
  uint32 digits[count];

  wordMove(digits, count, bigDigits(bg), count); // We copy in case of GC

  termPo list = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(h, (ptrPo) &list);
  gcAddRoot(h, &el);

  for (integer ix = 0; ix < count; ix++) {
    uint32 segment = digits[ix];

    el = (termPo) allocateInteger(h, segment);
    list = (termPo) allocateCons(h, el, list);
  }

  gcReleaseRoot(h, root);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) list};
}

ReturnStatus g__ints2big(heapPo h, termPo a1) {
  termPo list = a1;
  integer count = consLength(list);
  uint32 digits[count];

  for (integer ix = count - 1; ix >= 0 && isCons(list); ix--) {
    normalPo pr = C_NORMAL(list);
    digits[ix] = integerVal(consHead(pr));
    list = consTail(pr);
  }

  return (ReturnStatus) {.ret=Ok, .result=allocateBignum(h, count, digits)};
}
