//
// Created by Francis McCabe on 1/6/18.
// Integer operation escapes
//


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
#include "arithmetic.h"
#include "consP.h"

ReturnStatus g__int_plus(processPo p, heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateInteger(h, integerVal(a1) + integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_minus(processPo p, heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateInteger(h, integerVal(a1) - integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_times(processPo p, heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateInteger(h, integerVal(a1) * integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_div(processPo p, heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateInteger(h, integerVal(a1) / integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_mod(processPo p, heapPo h, termPo a1, termPo a2) {
  integer denom = integerVal(a1);
  integer numerator = integerVal(a2);

  integer reslt = denom % numerator;

  termPo Rs = (termPo) allocateInteger(h, reslt);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}


ReturnStatus g__int_gcd(heapPo h, termPo a1, termPo a2) {
  integer lhs = integerVal(a1);
  integer rhs = integerVal(a2);

  uinteger gC = intGCD(absolute(lhs), absolute(rhs));
  logical sign = (lhs >= 0) ^ (rhs >= 0);

  if (gC > 0) {
    termPo g = (termPo) allocateInteger(h, (sign ? gC : -gC));

    return (ReturnStatus) {.ret=Ok, .result=g};
  } else {
    return (ReturnStatus) {.ret=Error, .result=voidEnum};
  }
}

ReturnStatus g__band(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (termPo) allocateInteger(h, (integer) (Lhs & Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__basr(heapPo h, termPo a1, termPo a2) {
  integer Lhs = integerVal(a1);
  integer Rhs = integerVal(a2);

  termPo Rs = (termPo) allocateInteger(h, Lhs >> Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__blsl(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (termPo) allocateInteger(h, (integer) (Lhs << Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__blsr(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (termPo) allocateInteger(h, (integer) (Lhs >> Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bor(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (termPo) allocateInteger(h, (integer) (Lhs | Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bxor(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (termPo) allocateInteger(h, (integer) (Lhs ^ Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bnot(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateInteger(h, ~(unsigned) integerVal(arg1));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__nthb(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (Lhs & ((unsigned) 1 << Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_eq(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (integerVal(a1) == integerVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_ge(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (integerVal(a1) >= integerVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_lt(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (integerVal(a1) < integerVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_abs(heapPo h, termPo a1) {
  integer Arg = integerVal(a1);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(h, -Arg) : a1);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_hash(heapPo h, termPo Lhs) {
  integer Arg = integerVal(Lhs);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(h, hash64(Arg)) : Lhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

static integer countBits(integer ix) {
  uinteger ux = (unsigned) ix;

  static uinteger SK5 = 0x5555555555555555;
  static uinteger SK3 = 0x3333333333333333;
  static uinteger SKF0 = 0x0F0F0F0F0F0F0F0F;
  static uinteger SKFF = 0x00FF00FF00FF00FF;
  static uinteger SKFF16 = 0x0000FFFF0000FFFF;
  static uinteger SKFF32 = 0x00000000FFFFFFFF;

  ux = (ux & SK5) + ((ux >> 1u) & SK5);
  ux = (ux & SK3) + ((ux >> 2u) & SK3);
  ux = (ux & SKF0) + ((ux >> 4u) & SKF0);
  ux = (ux & SKFF) + ((ux >> 8u) & SKFF);
  ux = (ux & SKFF16) + ((ux >> 16u) & SKFF16);
  ux = (ux & SKFF32) + ((ux >> 32u) & SKFF32);
  return (integer) ux;
}

ReturnStatus g__bcount(heapPo h, termPo arg1) {
  integer Arg = integerVal(arg1);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(h, countBits(Arg))};
}

ReturnStatus g__int2str(heapPo h, termPo arg1) {
  integer ix = integerVal(arg1);
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  termPo str = (termPo) allocateString(h, buff, len);

  return (ReturnStatus) {.result = str, .ret=Ok};
}

ReturnStatus g__int_format(heapPo h, termPo a1, termPo a2) {
  integer ix = integerVal(a1);
  integer length;
  const char *fmt = strVal(a2, &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    return (ReturnStatus) {.result = (termPo) allocateString(h, buff, pos), .ret=Ok};
  } else
    return liberror(h, "_int_format", eINVAL);
}

ReturnStatus g__int2flt(heapPo h, termPo arg1) {
  integer Arg = integerVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, (double) Arg);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__irand(heapPo h, termPo arg1) {
  integer mx = integerVal(arg1);
  integer rnd = randomInt();

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(h, rnd % mx)};
}

ReturnStatus g__random(heapPo h) {
  double rnd = ((double) random()) / LARGE_INT32;

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateFloat(h, rnd)};
}

ReturnStatus g__seed(termPo arg1) {
  srandom((unsigned int) integerVal(arg1));
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}
