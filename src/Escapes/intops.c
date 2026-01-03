//
// Created by Francis McCabe on 1/6/18.
// Integer operation escapes
//

#include <strings.h>
#include <stdlib.h>
#include <globals.h>
#include "arithP.h"
#include "errorCodes.h"
#include "arithmetic.h"

ReturnStatus g__int_plus(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));
  pshVal(P, makeInteger(lhs + rhs));
  return Normal;
}

ReturnStatus g__int_minus(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));
  pshVal(P, makeInteger(lhs - rhs));
  return Normal;
}

ReturnStatus g__int_times(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));
  pshVal(P, makeInteger(lhs * rhs));
  return Normal;
}

ReturnStatus g__int_div(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  if (rhs == 0) {
    pshVal(P, divZero);
    return Abnormal;
  } else {
    pshVal(P, makeInteger(lhs / rhs));
    return Normal;
  }
}

ReturnStatus g__int_mod(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  if (rhs == 0) {
    pshVal(P, divZero);
    return Abnormal;
  } else {
    pshVal(P, makeInteger(lhs % rhs));
    return Normal;
  }
}

ReturnStatus g__int_gcd(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  integer gC = intGCD(lhs, rhs);

  if (gC > 0) {
    pshVal(P, makeInteger(gC));
    return Normal;
  } else {
    pshVal(P, divZero);
    return Abnormal;
  }
}

// Integer power: x^y
static integer intPow(integer x, integer y) {
  integer result = 1;
  assert(y >= 0);

  while (y > 0) {
    if ((y & 1) == 1)
      result *= x;
    x = x * x;
    y = y >> 1;
  }
  return result;
}

ReturnStatus g__int_pow(enginePo P) {
  integer x = integerVal(popVal(P));
  integer y = integerVal(popVal(P));

  if (y < 0) {
    pshVal(P, noValue);
    return Abnormal;
  } else {
    pshVal(P, makeInteger(intPow(x, y)));
    return Normal;
  }
}

ReturnStatus g__band(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, makeInteger(lhs & rhs));
  return Normal;
}

ReturnStatus g__basr(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, makeInteger(lhs >> rhs));
  return Normal;
}

ReturnStatus g__blsl(enginePo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs << rhs)));
  return Normal;
}

ReturnStatus g__blsr(enginePo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs >> rhs)));
  return Normal;
}

ReturnStatus g__bor(enginePo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs | rhs)));
  return Normal;
}

ReturnStatus g__bxor(enginePo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs ^ rhs)));
  return Normal;
}

ReturnStatus g__bnot(enginePo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (~lhs)));
  return Normal;
}

ReturnStatus g__nthb(enginePo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, (lhs & ((unsigned) 1 << rhs) ? trueEnum : falseEnum));
  return Normal;
}

ReturnStatus g__int_eq(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, lhs == rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__int_ge(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, lhs >= rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__int_lt(enginePo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, lhs < rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__int_abs(enginePo P) {
  termPo arg = popVal(P);
  integer lhs = integerVal(arg);
  pshVal(P, (lhs < 0 ? makeInteger(-lhs) : arg));
  return Normal;
}

ReturnStatus g__int_hash(enginePo P) {
  integer lhs = integerVal(popVal(P));
  pshVal(P, makeInteger(hash61(lhs)));
  return Normal;
}

ReturnStatus g__int_lg2(enginePo P) {
  integer lhs = integerVal(popVal(P));

  if (lhs <= 0) {
    pshVal(P, eRANGE);
    return Abnormal;
  } else {
    pshVal(P, makeInteger(lg2(lhs)));
    return Normal;
  }
}

ReturnStatus g__bcount(enginePo P) {
  pshVal(P, makeInteger(countBits(integerVal(popVal(P)))));
  return Normal;
}

ReturnStatus g__int2str(enginePo P) {
  integer ix = integerVal(popVal(P));
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  pshVal(P, allocateString(processHeap(P), buff, len));
  return Normal;
}

ReturnStatus g__int_format(enginePo P) {
  integer ix = integerVal(popVal(P));
  integer length;
  const char *fmt = strVal(popVal(P), &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    pshVal(P, allocateString(processHeap(P), buff, pos));
    return Normal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__int2flt(enginePo P) {
  integer ix = integerVal(popVal(P));
  double dx = (double) ix;
  if ((integer) dx == ix) { // Check coercion was safe
    termPo Rs = makeFloat((double) ix);
    pshVal(P, Rs);
    return Normal;
  }
  pshVal(P, eRANGE);
  return Abnormal;
}

ReturnStatus g__irand(enginePo P) {
  integer ix = integerVal(popVal(P));
  integer rnd = randomInt();
  pshVal(P, makeInteger(rnd % ix));
  return Normal;
}

ReturnStatus g__random(enginePo P) {
  double rnd = ((double) random()) / LARGE_INT32;
  pshVal(P, makeFloat(rnd));
  return Normal;
}

ReturnStatus g__seed(enginePo P) {
  unsigned int ix = (unsigned int) integerVal(popVal(P));

  srandom(ix);
  pshVal(P, unitEnum);
  return Normal;
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}
