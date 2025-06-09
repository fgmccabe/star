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

ReturnStatus g__int_plus(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));
  pshVal(P, makeInteger(lhs + rhs));
  return Normal;
}

ReturnStatus g__int_minus(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));
  pshVal(P, makeInteger(lhs - rhs));
  return Normal;
}

ReturnStatus g__int_times(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));
  pshVal(P, makeInteger(lhs * rhs));
  return Normal;
}

ReturnStatus g__int_div(processPo P) {
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

ReturnStatus g__int_mod(processPo P) {
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

ReturnStatus g__int_gcd(processPo P) {
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

ReturnStatus g__int_pow(processPo P) {
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

ReturnStatus g__band(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, makeInteger(lhs & rhs));
  return Normal;
}

ReturnStatus g__basr(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, makeInteger(lhs >> rhs));
  return Normal;
}

ReturnStatus g__blsl(processPo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs << rhs)));
  return Normal;
}

ReturnStatus g__blsr(processPo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs >> rhs)));
  return Normal;
}

ReturnStatus g__bor(processPo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs | rhs)));
  return Normal;
}

ReturnStatus g__bxor(processPo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (lhs ^ rhs)));
  return Normal;
}

ReturnStatus g__bnot(processPo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));

  pshVal(P, makeInteger((integer) (~lhs)));
  return Normal;
}

ReturnStatus g__nthb(processPo P) {
  uint64 lhs = (uint64) integerVal(popVal(P));
  uint64 rhs = (uint64) integerVal(popVal(P));

  pshVal(P, (lhs & ((unsigned) 1 << rhs) ? trueEnum : falseEnum));
  return Normal;
}

ReturnStatus g__int_eq(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, lhs == rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__int_ge(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, lhs >= rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__int_lt(processPo P) {
  integer lhs = integerVal(popVal(P));
  integer rhs = integerVal(popVal(P));

  pshVal(P, lhs < rhs ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__int_abs(processPo P) {
  termPo arg = popVal(P);
  integer lhs = integerVal(arg);
  pshVal(P, (lhs < 0 ? makeInteger(-lhs) : arg));
  return Normal;
}

ReturnStatus g__int_hash(processPo P) {
  integer lhs = integerVal(popVal(P));
  pshVal(P, makeInteger(hash61(lhs)));
  return Normal;
}

ReturnStatus g__int_lg2(processPo P) {
  integer lhs = integerVal(popVal(P));

  if (lhs <= 0) {
    pshVal(P, eRANGE);
    return Abnormal;
  } else {
    pshVal(P, makeInteger(lg2(lhs)));
    return Normal;
  }
}

ReturnStatus g__bcount(processPo P) {
  pshVal(P, makeInteger(countBits(integerVal(popVal(P)))));
  return Normal;
}

ReturnStatus g__int2str(processPo P) {
  integer ix = integerVal(popVal(P));
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  pshVal(P,allocateString(currentHeap, buff, len));
  return Normal;
}

ReturnStatus g__int_format(processPo P) {
  integer ix = integerVal(popVal(P));
  integer length;
  const char *fmt = strVal(popVal(P), &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    pshVal(P,allocateString(currentHeap, buff, pos));
    return Normal;
  } else{
    pshVal(P,eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__int2flt(processPo P) {
  integer ix = integerVal(popVal(P));
  termPo Rs = makeFloat((double) ix);
  pshVal(P,Rs);
  return Normal;
}

ReturnStatus g__irand(processPo P) {
  integer ix = integerVal(popVal(P));
  integer rnd = randomInt();
  pshVal(P, makeInteger(rnd%ix));
  return Normal;
}

ReturnStatus g__random(processPo P) {
  double rnd = ((double) random()) / LARGE_INT32;
  pshVal(P, makeFloat(rnd));
  return Normal;
}

ReturnStatus g__seed(processPo P) {
  unsigned int ix = (unsigned int)integerVal(popVal(P));

  srandom(ix);
  pshVal(P,unitEnum);
  return Normal;
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}
