//
// Created by Francis McCabe on 1/6/18.
// Arithmetic on big numbers

#include <strings.h>
#include <tpl.h>
#include <globals.h>
#include "escape.h"
#include "arithP.h"
#include "bignumP.h"
#include "errorCodes.h"
#include "option.h"
#include "consP.h"

ValueReturn s__big_plus(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longAdd(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_plus(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_plus(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_minus(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longSubtract(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_minus(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_minus(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_bitand(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer cS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 sum[cS];
  integer cC = longBitAnd(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_bitand(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_bitand(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_bitor(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer cS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 sum[cS];
  integer cC = longBitOr(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_bitor(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_bitor(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_bitxor(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer cS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 sum[cS];
  integer cC = longBitOr(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_bitxor(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_bitxor(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_bitnot(enginePo P, termPo l) {
  bignumPo lhs = C_BIGNUM(l);
  integer cS = bigCount(lhs) + 1;
  uint32 sum[cS];
  integer cC = longBitNot(sum, cS, bigDigits(lhs), bigCount(lhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_bitnot(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__big_bitnot(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_times(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longMultiply(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  return normalReturn(allocateBignum(processHeap(P), cC, sum));
}

ReturnStatus g__big_times(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__big_times(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_div(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer qS = bigCount(lhs) + bigCount(rhs) + 1;

  uint32 quot[qS];
  uint32 rem[qS];

  integer qC = qS;
  integer rC = qS;

  retCode ret = longDivide(quot, &qC, rem, &rC, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  if (ret == Ok) {
    heapPo h = processHeap(P);
    termPo Qt = allocateBignum(h, qC, quot);
    int root = gcAddRoot(h, &Qt);

    termPo Rt = allocateBignum(h, rC, rem);
    gcAddRoot(h, &Rt);
    termPo Rs = (termPo) allocatePair(h, Qt, Rt);
    gcReleaseRoot(h, root);
    return normalReturn(Rs);
  } else {
    return abnormalReturn(divZero);
  }
}

ReturnStatus g__big_div(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__big_div(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_gcd(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  integer qS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 gcd[qS];

  integer qC = qS;

  integer gC = longGCD(gcd, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  if (gC > 0) {
    return normalReturn(allocateBignum(processHeap(P), (uint32) gC, gcd));
  } else {
    return abnormalReturn(divZero);
  }
}

ReturnStatus g__big_gcd(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__big_gcd(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_format(enginePo P, termPo b, termPo fmt) {
  bignumPo bg = C_BIGNUM(b);
  uint32 bgCount = bigCount(bg);
  uint32 *bgData = bigDigits(bg);

  integer fmtLen;
  const char *format = strVal(fmt, &fmtLen);

  integer bufLen = bgCount * 16;
  char buff[bufLen];

  integer resLen = longFormat(bgData, bgCount, format, fmtLen, buff, bufLen);

  if (resLen >= 0) {
    return normalReturn(allocateString(processHeap(P), buff, resLen));
  } else
    return abnormalReturn(eINVAL);
}

ReturnStatus g__big_format(enginePo P) {
  termPo b = popVal(P);
  ValueReturn ret = s__big_format(P, b, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big2str(enginePo P, termPo b) {
  bignumPo bg = C_BIGNUM(b);
  uint32 bgCount = bigCount(bg);
  uint32 *bgData = bigDigits(bg);

  integer bufLen = bgCount * 16;
  char buff[bufLen];
  integer actual = textFromlong(buff, bufLen, bgData, bgCount);
  return normalReturn(allocateString(processHeap(P), buff, actual));
}

ReturnStatus g__big2str(enginePo P) {
  termPo b = popVal(P);

  ValueReturn ret = s__big2str(P, b);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__str2big(enginePo P, termPo st) {
  integer len;
  const char *text = strVal(st, &len);
  integer gSize = ((len + 7) / 8) + 1;
  uint32 digits[gSize];

  integer bgSize = longFromText(text, len, digits, gSize);

  if (bgSize > 0) {
    heapPo h = processHeap(P);
    return normalReturn(allocateBignum(h, bgSize, digits));
  } else {
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__str2big(enginePo P) {
  ValueReturn ret = s__str2big(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_hash(enginePo P, termPo b) {
  bignumPo bg = C_BIGNUM(b);
  return normalReturn(makeInteger(bignumHash(bg)));
}

ReturnStatus g__big_hash(enginePo P) {
  ValueReturn ret = s__big_hash(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_eq(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  return normalReturn(longEqual(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs)) ? trueEnum : falseEnum);
}

ReturnStatus g__big_eq(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_eq(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_lt(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  switch (longCompare(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs))) {
    case smaller:
      return normalReturn(trueEnum);
    default:
      return normalReturn(falseEnum);
  }
}

ReturnStatus g__big_lt(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_lt(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big_ge(enginePo P, termPo l, termPo r) {
  bignumPo lhs = C_BIGNUM(l);
  bignumPo rhs = C_BIGNUM(r);
  switch (longCompare(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs))) {
    case smaller:
      return normalReturn(falseEnum);
    default:
      return normalReturn(trueEnum);
  }
}

ReturnStatus g__big_ge(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__big_ge(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int2big(enginePo P, integer ix) {
  uint64 U = (uint64) ix;

  uint32 uu[] = {U & ONES_MASK, (U >> 32) & ONES_MASK};
  return normalReturn(allocateBignum(processHeap(P), NumberOf(uu), uu));
}

ReturnStatus g__int2big(enginePo P) {
  ValueReturn ret = s__int2big(P, integerVal(popVal(P)));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big2ints(enginePo P, termPo b) {
  bignumPo bg = C_BIGNUM(b);
  uint32 count = bigCount(bg);
  uint32 digits[count];

  wordMove(digits, count, bigDigits(bg), count); // We copy in case of GC

  termPo list = nilEnum;
  termPo el = voidEnum;
  heapPo h = processHeap(P);
  int root = gcAddRoot(h, (ptrPo) &list);
  gcAddRoot(h, &el);

  for (integer ix = 0; ix < count; ix++) {
    uint32 segment = digits[ix];

    el = makeInteger(segment);
    list = (termPo) allocateCons(h, el, list);
  }

  gcReleaseRoot(h, root);

  return normalReturn(list);
}

ReturnStatus g__big2ints(enginePo P) {
  termPo b = popVal(P);
  ValueReturn ret = s__big2ints(P, b);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__ints2big(enginePo P, termPo list) {
  integer count = consLength(list);
  uint32 digits[count];

  for (integer ix = count - 1; ix >= 0 && isCons(list); ix--) {
    normalPo pr = C_NORMAL(list);
    digits[ix] = integerVal(consHead(pr));
    list = consTail(pr);
  }

  return normalReturn(allocateBignum(processHeap(P), count, digits));
}

ReturnStatus g__ints2big(enginePo P) {
  ValueReturn ret = s__ints2big(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__big2int(enginePo P, termPo b) {
  bignumPo bg = C_BIGNUM(b);
  uint32 count = bigCount(bg);
  uint32 *digits = bigDigits(bg);

  switch (count) {
    case 0: {
      return normalReturn(makeInteger(0));
    }
    case 1: {
      return normalReturn(makeInteger((integer) digits[0]));
    }
    case 2: {
      uinteger lge = ((uint64) digits[0]) | (((uint64) digits[1]) << 32);
      return normalReturn(makeInteger((integer) lge));
    }
    default:
      return abnormalReturn(eRANGE);
  }
}

ReturnStatus g__big2int(enginePo P) {
  termPo b = popVal(P);
  ValueReturn ret = s__big2int(P, b);
  pshVal(P, ret.value);
  return ret.status;
}
