//
// Created by Francis McCabe on 1/6/18.
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

ReturnStatus g__flt_eq(heapPo h, termPo a1, termPo a2, termPo a3) {
  termPo Rs = (nearlyEqual(floatVal(a1), floatVal(a2), floatVal(a3)) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_ge(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (floatVal(a1) >= floatVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_lt(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (floatVal(a1) < floatVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_plus(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateFloat(h, floatVal(a1) + floatVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_minus(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateFloat(h, floatVal(a1) - floatVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_times(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateFloat(h, floatVal(a1) * floatVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_div(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateFloat(h, floatVal(a1) / floatVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_mod(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateFloat(h, fmod(floatVal(a1), floatVal(a2)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_pwr(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) allocateFloat(h, pow(floatVal(a1), floatVal(a2)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_abs(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = (Arg < 0 ? (termPo) allocateFloat(h, -Arg) : arg1);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_exp(heapPo h, termPo arg1) {
  double x = floatVal(arg1);

  errno = 0;    /* clear errno prior to computation */
  double ans = exp(x);    /* allow for checks of the answer */

  if (errno != 0) {
    if (errno == EDOM || errno == ERANGE)
      return liberror(h, "_exp", eRANGE);
    else
      return liberror(h, "_exp", eINVAL);
  } else {
    return (ReturnStatus) {.ret=Ok,
      .result=(termPo) allocateFloat(h, ans)};
  }
}

ReturnStatus g__ldexp(heapPo h, termPo a1, termPo a2) {
  double x = floatVal(a1);
  integer e = integerVal(a2);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateFloat(h, ldexp(x, (int) e))};
}

ReturnStatus g__frexp(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  int exp;
  double frac = frexp(Arg, &exp);
  heapPo H = h;

  termPo man = (termPo) allocateFloat(H, frac);
  int root = gcAddRoot(H, &man);
  termPo ex = (termPo) allocateInteger(H, (integer) exp);
  gcAddRoot(H, &ex);
  termPo Rs = (termPo) allocatePair(H, man, ex);
  gcReleaseRoot(H, root);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__modf(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  double intgrl;
  double frac = modf(Arg, &intgrl);
  heapPo H = h;

  termPo man = (termPo) allocateFloat(H, frac);
  int root = gcAddRoot(H, &man);
  termPo ex = (termPo) allocateInteger(H, (integer) intgrl);
  gcAddRoot(H, &ex);
  termPo Rs = (termPo) allocatePair(H, man, ex);
  gcReleaseRoot(H, root);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int2flt(heapPo h, termPo arg1) {
  integer Arg = integerVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, (double) Arg);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt2int(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateInteger(h, (integer) floatVal(arg1));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bits_float(heapPo h, termPo arg1) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, Arg.Dx);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__float_bits(heapPo h, termPo arg1) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(arg1);
  termPo Rs = (termPo) allocateInteger(h, Arg.Ix);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt2str(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  char buff[64];

  retCode ret = formatDouble(buff, NumberOf(buff), Arg, general, 0, False);
  if (ret == Ok) {
    return (ReturnStatus) {.ret=Ok,
      .result = (termPo) allocateString(h, buff, uniStrLen(buff))};
  } else
    return liberror(h, "_fltstr", eINVAL);
}

ReturnStatus g__flt_format(heapPo h, termPo a1, termPo a2) {
  integer length;
  const char *fmt = strVal(a2, &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedFloat(floatVal(a1), buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    return (ReturnStatus) {.result = (termPo) allocateString(h, buff, uniStrLen(buff)), .ret=Ok};
  } else
    return liberror(h, "_flt_format", eINVAL);
}

ReturnStatus g__flt_hash(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);

  termPo Rs = (termPo) allocateInteger(h, floatHash(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_cos(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, cos(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_sin(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, sin(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_tan(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, tan(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_acos(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, acos(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_asin(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, asin(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_atan(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, atan(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_floor(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, floor(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_ceil(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, ceil(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_trunc(heapPo h, termPo arg1) {
  termPo Rs = (termPo) allocateFloat(h, trunc(floatVal(arg1)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_integral(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);

  return (ReturnStatus) {.ret=Ok,
    .result=(floor(Arg) == Arg ? trueEnum : falseEnum)};
}

ReturnStatus g_log(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, log(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_log10(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, log10(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_sqrt(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = (termPo) allocateFloat(h, sqrt(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_pi(heapPo h) {
  termPo Rs = (termPo) allocateFloat(h, M_PI);

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

// Arithmetic on big numbers

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
