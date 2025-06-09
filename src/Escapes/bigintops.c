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

ReturnStatus g__big_plus(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longAdd(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  pshVal(P, allocateBignum(currentHeap, cC, sum));
  return Normal;
}

ReturnStatus g__big_minus(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer cS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 sum[cS];
  integer cC = longSubtract(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  pshVal(P, allocateBignum(currentHeap, cC, sum));
  return Normal;
}

ReturnStatus g__big_bitand(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer cS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 sum[cS];
  integer cC = longBitAnd(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  pshVal(P, allocateBignum(currentHeap, cC, sum));
  return Normal;
}

ReturnStatus g__big_bitor(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer cS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 sum[cS];
  integer cC = longBitOr(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  pshVal(P, allocateBignum(currentHeap, cC, sum));
  return Normal;
}

ReturnStatus g__big_bitxor(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer cS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 sum[cS];
  integer cC = longBitXor(sum, cS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  pshVal(P, allocateBignum(currentHeap, cC, sum));
  return Normal;
}

ReturnStatus g__big_bitnot(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  integer cS = bigCount(lhs) + 1;
  uint32 sum[cS];
  integer cC = longBitNot(sum, cS, bigDigits(lhs), bigCount(lhs));
  pshVal(P, allocateBignum(currentHeap, cC, sum));
  return Normal;
}

ReturnStatus g__big_times(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer pS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 prod[pS];
  integer cC = longMultiply(prod, pS, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  pshVal(P, allocateBignum(currentHeap, cC, prod));
  return Normal;
}

ReturnStatus g__big_div(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  uint32 qS = bigCount(lhs) + bigCount(rhs) + 1;
  uint32 quot[qS];
  uint32 rem[qS];

  integer qC = qS;
  integer rC = qS;

  retCode ret = longDivide(quot, &qC, rem, &rC, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  if (ret == Ok) {
    termPo Qt = allocateBignum(currentHeap, qC, quot);
    int root = gcAddRoot(currentHeap, &Qt);

    termPo Rt = allocateBignum(currentHeap, rC, rem);
    gcAddRoot(currentHeap, &Rt);
    termPo Rs = (termPo) allocatePair(currentHeap, Qt, Rt);
    gcReleaseRoot(currentHeap, root);
    pshVal(P, Rs);
    return Normal;
  } else {
    pshVal(P, divZero);
    return Abnormal;
  }
}

ReturnStatus g__big_gcd(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));
  integer qS = maximum(bigCount(lhs), bigCount(rhs)) + 1;
  uint32 gcd[qS];

  integer qC = qS;

  integer gC = longGCD(gcd, bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs));
  if (gC > 0) {
    pshVal(P, allocateBignum(currentHeap, (uint32) gC, gcd));
    return Normal;
  } else {
    pshVal(P, divZero);
    return Abnormal;
  }
}

ReturnStatus g__big_format(processPo P) {
  bignumPo bg = C_BIGNUM(popVal(P));
  uint32 bgCount = bigCount(bg);
  uint32 *bgData = bigDigits(bg);

  integer fmtLen;
  const char *fmt = strVal(popVal(P), &fmtLen);

  integer bufLen = bgCount * 16;
  char buff[bufLen];

  integer resLen = longFormat(bgData, bgCount, fmt, fmtLen, buff, bufLen);

  if (resLen >= 0) {
    pshVal(P, allocateString(currentHeap, buff, resLen));
    return Normal;
  } else
    pshVal(P, eINVAL);
  return Abnormal;
}

ReturnStatus g__big2str(processPo P) {
  bignumPo bg = C_BIGNUM(popVal(P));
  uint32 bgCount = bigCount(bg);
  uint32 *bgData = bigDigits(bg);

  integer bufLen = bgCount * 16;
  char buff[bufLen];
  integer actual = textFromlong(buff, bufLen, bgData, bgCount);
  pshVal(P, allocateString(currentHeap, buff, actual));
  return Normal;
}

ReturnStatus g__str2big(processPo P) {
  integer len;
  const char *str = strVal(popVal(P), &len);
  integer gSize = ((len + 7) / 8) + 1;
  uint32 digits[gSize];

  integer bgSize = longFromText(str, len, digits, gSize);

  if (bgSize > 0)
    pshVal(P, (termPo) wrapSome(currentHeap, allocateBignum(currentHeap, bgSize, digits)));
  else
    pshVal(P, noneEnum);

  return Normal;
}

ReturnStatus g__big_hash(processPo P) {
  bignumPo bg = C_BIGNUM(popVal(P));
  pshVal(P, makeInteger(bignumHash(bg)));
  return Normal;
}

ReturnStatus g__big_eq(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));

  pshVal(P, longEqual(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs)) ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__big_lt(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));

  switch (longCompare(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs))) {
    case smaller:
      pshVal(P, trueEnum);
      break;
    default:
      pshVal(P, falseEnum);
  }
  return Normal;
}

ReturnStatus g__big_ge(processPo P) {
  bignumPo lhs = C_BIGNUM(popVal(P));
  bignumPo rhs = C_BIGNUM(popVal(P));

  switch (longCompare(bigDigits(lhs), bigCount(lhs), bigDigits(rhs), bigCount(rhs))) {
    case smaller:
      pshVal(P, falseEnum);
      break;
    default:
      pshVal(P, trueEnum);
  }
  return Normal;
}

ReturnStatus g__int2big(processPo P) {
  uint64 U = (uint64) integerVal(popVal(P));

  uint32 uu[] = {U & ONES_MASK, (U >> 32) & ONES_MASK};
  pshVal(P, allocateBignum(currentHeap, NumberOf(uu), uu));
  return Normal;
}

ReturnStatus g__big2ints(processPo P) {
  bignumPo bg = C_BIGNUM(popVal(P));
  uint32 count = bigCount(bg);
  uint32 digits[count];

  wordMove(digits, count, bigDigits(bg), count); // We copy in case of GC

  termPo list = (termPo) nilEnum;
  termPo el = voidEnum;
  int root = gcAddRoot(currentHeap, (ptrPo) &list);
  gcAddRoot(currentHeap, &el);

  for (integer ix = 0; ix < count; ix++) {
    uint32 segment = digits[ix];

    el = makeInteger(segment);
    list = (termPo) allocateCons(currentHeap, el, list);
  }

  gcReleaseRoot(currentHeap, root);

  pshVal(P, list);
  return Normal;
}

ReturnStatus g__ints2big(processPo P) {
  termPo list = popVal(P);
  integer count = consLength(list);
  uint32 digits[count];

  for (integer ix = count - 1; ix >= 0 && isCons(list); ix--) {
    normalPo pr = C_NORMAL(list);
    digits[ix] = integerVal(consHead(pr));
    list = consTail(pr);
  }

  pshVal(P, allocateBignum(currentHeap, count, digits));
  return Normal;
}

ReturnStatus g__big2int(processPo P) {
  bignumPo bg = C_BIGNUM(popVal(P));
  uint32 count = bigCount(bg);
  uint32 *digits = bigDigits(bg);

  switch (count) {
    case 0: {
      pshVal(P, (termPo) wrapSome(currentHeap, makeInteger(0)));
      break;
    }
    case 1: {
      pshVal(P, (termPo) wrapSome(currentHeap, makeInteger((integer) digits[0])));
      break;
    }
    case 2: {
      uinteger lge = ((uint64) digits[0]) | (((uint64) digits[1]) << 32);
      pshVal(P, (termPo) wrapSome(currentHeap, makeInteger((integer) lge)));
      break;
    }
    default:
      pshVal(P, noneEnum);
      break;
  }
  return Normal;
}
