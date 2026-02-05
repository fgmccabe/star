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

ReturnStatus g__int_plus(enginePo P) {
  ValueReturn ret = s__int_plus(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_minus(enginePo P, termPo l, termPo r) {
  return normalReturn(makeInteger(integerVal(l) - integerVal(r)));
}

ReturnStatus g__int_minus(enginePo P) {
  ValueReturn ret = s__int_minus(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_times(enginePo P, termPo l, termPo r) {
  return normalReturn(makeInteger(integerVal(l) * integerVal(r)));
}

ReturnStatus g__int_times(enginePo P) {
  ValueReturn ret = s__int_times(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_div(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  if (rhs == 0)
    return abnormalReturn(divZero);
  else
    return normalReturn(makeInteger(lhs / rhs));
}

ReturnStatus g__int_div(enginePo P) {
  ValueReturn ret = s__int_div(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_mod(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  if (rhs == 0)
    return abnormalReturn(divZero);
  else
    return normalReturn(makeInteger(lhs % rhs));
}

ReturnStatus g__int_mod(enginePo P) {
  ValueReturn ret = s__int_mod(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
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

ReturnStatus g__int_gcd(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__int_gcd(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
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

ReturnStatus g__int_pow(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__int_pow(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__band(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeInteger(lhs & rhs));
}

ReturnStatus g__band(enginePo P) {
  ValueReturn ret = s__band(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__basr(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeInteger(lhs >> rhs));
}

ReturnStatus g__basr(enginePo P) {
  ValueReturn ret = s__basr(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__blsl(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeInteger(lhs << rhs));
}

ReturnStatus g__blsl(enginePo P) {
  ValueReturn ret = s__blsl(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__blsr(enginePo P, termPo l, termPo r) {
  uint64 lhs = (uint64) integerVal(l);
  uint64 rhs = (uint64) integerVal(r);

  return normalReturn(makeInteger((integer)(lhs >> rhs)));
}

ReturnStatus g__blsr(enginePo P) {
  ValueReturn ret = s__blsr(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__bor(enginePo P, termPo l, termPo r) {
  uint64 lhs = (uint64) integerVal(l);
  uint64 rhs = (uint64) integerVal(r);

  return normalReturn(makeInteger((integer)(lhs | rhs)));
}

ReturnStatus g__bor(enginePo P) {
  ValueReturn ret = s__bor(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__bxor(enginePo P, termPo l, termPo r) {
  uint64 lhs = (uint64) integerVal(l);
  uint64 rhs = (uint64) integerVal(r);

  return normalReturn(makeInteger((integer)(lhs ^ rhs)));
}

ReturnStatus g__bxor(enginePo P) {
  ValueReturn ret = s__bxor(P, popVal(P), popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__bnot(enginePo P, termPo l) {
  uint64 lhs = (uint64) integerVal(l);

  return normalReturn(makeInteger((integer)(~lhs)));
}

ReturnStatus g__bnot(enginePo P) {
  ValueReturn ret = s__bnot(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__nthb(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn((lhs & ((unsigned) 1 << rhs) ? trueEnum : falseEnum));
}

ReturnStatus g__nthb(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__nthb(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_eq(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(lhs==rhs ? trueEnum : falseEnum);
}

ReturnStatus g__int_eq(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__int_eq(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_ge(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(lhs>=rhs ? trueEnum : falseEnum);
}

ReturnStatus g__int_ge(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__int_ge(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_lt(enginePo P, termPo l, termPo r) {
  integer lhs = integerVal(l);
  integer rhs = integerVal(r);

  return normalReturn(lhs<rhs ? trueEnum : falseEnum);
}

ReturnStatus g__int_lt(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__int_lt(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_abs(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  return normalReturn(lhs < 0 ? makeInteger(-lhs) : l);
}

ReturnStatus g__int_abs(enginePo P) {
  ValueReturn ret = s__int_abs(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_hash(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  return normalReturn(makeInteger(hash61(lhs)));
}

ReturnStatus g__int_hash(enginePo P) {
  ValueReturn ret = s__int_hash(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int_lg2(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  if (lhs <= 0)
    return abnormalReturn(eRANGE);
  else

    return normalReturn(makeInteger(lg2(lhs)));
}

ReturnStatus g__int_lg2(enginePo P) {
  ValueReturn ret = s__int_hash(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__bcount(enginePo P, termPo l) {
  integer lhs = integerVal(l);

  return normalReturn(makeInteger(countBits(lhs)));
}

ReturnStatus g__bcount(enginePo P) {
  ValueReturn ret = s__bcount(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int2str(enginePo P, termPo l) {
  integer lhs = integerVal(l);
  char buff[64];

  integer len = int2StrByBase(buff, lhs, 0, 10);
  return normalReturn(allocateString(processHeap(P), buff, len));
}

ReturnStatus g__int2str(enginePo P) {
  termPo lhs = popVal(P);
  ValueReturn ret = s__int2str(P, lhs);
  pshVal(P, ret.value);
  return ret.status;
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

ReturnStatus g__int_format(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__int_format(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int2flt(enginePo P, termPo l) {
  integer ix = integerVal(l);
  double dx = (double) ix;
  if ((integer) dx == ix) {
    // Check coercion was safe
    return normalReturn(makeFloat(processHeap(P),(double) dx));
  }
  return abnormalReturn(eRANGE);
}

ReturnStatus g__int2flt(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__int2flt(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__irand(enginePo P, termPo l) {
  integer ix = integerVal(l);
  integer rnd = randomInt();
  return normalReturn(makeInteger(rnd % ix));
}

ReturnStatus g__irand(enginePo P) {
  ValueReturn ret = s__irand(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__random(enginePo P) {
  return normalReturn(makeFloat(processHeap(P),((double) random()) / LARGE_INT32));
}

ReturnStatus g__random(enginePo P) {
  ValueReturn ret = s__random(P);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__seed(enginePo P, termPo l) {
  unsigned int ix = (unsigned int) integerVal(l);

  srandom(ix);
  return normalReturn(unitEnum);
}

ReturnStatus g__seed(enginePo P) {
  ValueReturn ret = s__seed(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}
