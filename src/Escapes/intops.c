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

ValueReturn s__int_plus(enginePo P, termPo l, termPo r) {
  return normalReturn(makeInteger(integerVal(l) + integerVal(r)));
}

ValueReturn s__int_minus(enginePo P, termPo l, termPo r) {
  return normalReturn(makeInteger(integerVal(l) - integerVal(r)));
}

ValueReturn s__int_times(enginePo P, termPo l, termPo r) {
  return normalReturn(makeInteger(integerVal(l) * integerVal(r)));
}

ValueReturn s__int_div(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  if (rhs == 0)
    return abnormalReturn(divZero);
  else
    return normalReturn(makeInteger(lhs / rhs));
}

ValueReturn s__int_mod(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  if (rhs == 0)
    return abnormalReturn(divZero);
  else
    return normalReturn(makeInteger(lhs % rhs));
}

ValueReturn s__int_gcd(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  integer gC = intGCD(lhs, rhs);

  if (gC > 0) {
    return normalReturn(makeInteger(gC));
  } else {
    return abnormalReturn(divZero);
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

ValueReturn s__int_pow(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  if (rhs < 0) {
    return abnormalReturn(noValue);
  } else {
    return normalReturn(makeInteger(intPow(lhs,rhs)));
  }
}

ValueReturn s__band(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeInteger(lhs & rhs));
}

ValueReturn s__basr(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeInteger(lhs >> rhs));
}

ValueReturn s__blsl(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeInteger(lhs << rhs));
}

ValueReturn s__blsr(enginePo P, termPo l, termPo r) {
  uint64 lhs = (uint64) integerVal(l);
  uint64 rhs = (uint64) integerVal(r);

  return normalReturn(makeInteger((integer)(lhs >> rhs)));
}

ValueReturn s__bor(enginePo P, termPo l, termPo r) {
  uint64 lhs = (uint64) integerVal(l);
  uint64 rhs = (uint64) integerVal(r);

  return normalReturn(makeInteger((integer)(lhs | rhs)));
}

ValueReturn s__bxor(enginePo P, termPo l, termPo r) {
  uint64 lhs = (uint64) integerVal(l);
  uint64 rhs = (uint64) integerVal(r);

  return normalReturn(makeInteger((integer)(lhs ^ rhs)));
}

ValueReturn s__bnot(enginePo P, termPo l) {
  uint64 lhs = (uint64) integerVal(l);

  return normalReturn(makeInteger((integer)(~lhs)));
}

ValueReturn s__nthb(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn((lhs & ((unsigned) 1 << rhs) ? trueEnum : falseEnum));
}

ValueReturn s__int_eq(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(lhs==rhs ? trueEnum : falseEnum);
}

ValueReturn s__int_ge(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(lhs>=rhs ? trueEnum : falseEnum);
}

ValueReturn s__int_lt(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(lhs<rhs ? trueEnum : falseEnum);
}

ValueReturn s__int_abs(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  return normalReturn(lhs < 0 ? makeInteger(-lhs) : l);
}

ValueReturn s__int_hash(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  return normalReturn(makeInteger(hash61(lhs)));
}

ValueReturn s__int_lg2(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  if (lhs <= 0)
    return abnormalReturn(eRANGE);
  else

    return normalReturn(makeInteger(lg2(lhs)));
}

ValueReturn s__bcount(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  return normalReturn(makeInteger(countBits(lhs)));
}

ValueReturn s__int2str(enginePo P, termPo l) {
  integer lhs = integerVal(l);
  char buff[64];

  integer len = int2StrByBase(buff, lhs, 0, 10);
  return normalReturn(allocateString(processHeap(P), buff, len));
}

ValueReturn s__int_format(enginePo P, termPo l, termPo r) {
  integer ix = integerVal(l);
  integer length;
  const char *fmt = strVal(r, &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    return normalReturn(allocateString(processHeap(P), buff, pos));
  } else {
    return abnormalReturn(eINVAL);
  }
}

ValueReturn s__int2flt(enginePo P, termPo l) {
  integer ix = integerVal(l);
  double dx = (double) ix;
  if ((integer) dx == ix) {
    // Check coercion was safe
    return normalReturn(makeFloat(processHeap(P),dx));
  }
  return abnormalReturn(eRANGE);
}

ValueReturn s__irand(enginePo P, termPo l) {
  integer ix = integerVal(l);
  integer rnd = randomInt();
  return normalReturn(makeInteger(rnd % ix));
}

ValueReturn s__random(enginePo P) {
  return normalReturn(makeFloat(processHeap(P),((double) random()) / LARGE_INT32));
}

ValueReturn s__seed(enginePo P, termPo l) {
  unsigned int ix = (unsigned int) integerVal(l);

  srandom(ix);
  return normalReturn(unitEnum);
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}
