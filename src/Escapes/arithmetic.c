//
// Created by Francis McCabe on 1/6/18.
//


#include <math.h>
#include <chars.h>
#include <tpl.h>
#include <stdlib.h>
#include <errno.h>
#include <globals.h>
#include "ooio.h"
#include "engine.h"
#include "arithP.h"
#include "errorCodes.h"
#include "arithmetic.h"

ReturnStatus g__int_plus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) + integerVal(Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_minus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) - integerVal(Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_times(processPo p, ptrPo tos) {
  integer Lhs = integerVal(tos[0]);
  integer Rhs = integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs * Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_div(processPo p, ptrPo tos) {
  integer Lhs = integerVal(tos[0]);
  integer Rhs = integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs / Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_mod(processPo p, ptrPo tos) {
  integer denom = integerVal(tos[0]);
  integer numerator = integerVal(tos[1]);

  integer reslt = denom % numerator;

  termPo Rs = (termPo) allocateInteger(processHeap(p), reslt);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__band(processPo p, ptrPo tos) {
  uint64 Lhs = (uint64) integerVal(tos[0]);
  uint64 Rhs = (uint64) integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs & Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__basr(processPo p, ptrPo tos) {
  integer Lhs = integerVal(tos[0]);
  integer Rhs = integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs >> Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__blsl(processPo p, ptrPo tos) {
  uint64 Lhs = (uint64) integerVal(tos[0]);
  uint64 Rhs = (uint64) integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs << Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__blsr(processPo p, ptrPo tos) {
  uint64 Lhs = (uint64) integerVal(tos[0]);
  uint64 Rhs = (uint64) integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs >> Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bor(processPo p, ptrPo tos) {
  uint64 Lhs = (uint64) integerVal(tos[0]);
  uint64 Rhs = (uint64) integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs | Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bxor(processPo p, ptrPo tos) {
  uint64 Lhs = (uint64) integerVal(tos[0]);
  uint64 Rhs = (uint64) integerVal(tos[1]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), Lhs ^ Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bnot(processPo p, ptrPo tos) {
  termPo Rs = (termPo) allocateInteger(processHeap(p), ~(unsigned) integerVal(tos[0]));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__nthb(processPo p, ptrPo tos) {
  uint64 Lhs = (uint64) integerVal(tos[0]);
  uint64 Rhs = (uint64) integerVal(tos[1]);

  termPo Rs = (Lhs & ((unsigned) 1 << Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_eq(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (integerVal(Lhs) == integerVal(Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_ge(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (integerVal(Lhs) >= integerVal(Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_lt(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (integerVal(Lhs) < integerVal(Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_abs(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer Arg = integerVal(Lhs);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), -Arg) : Lhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_hash(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer Arg = integerVal(Lhs);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), hash64(Arg)) : Lhs);

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
  return ux;
}

ReturnStatus g__bcount(processPo p, ptrPo tos) {
  integer Arg = integerVal(tos[0]);

  return (ReturnStatus) {.ret=Ok,
            .result=(termPo) allocateInteger(processHeap(p), countBits(Arg))};
}

ReturnStatus g__int2str(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer ix = integerVal(Lhs);
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  termPo str = (termPo) allocateChars(processHeap(p), buff, len);

  return (ReturnStatus){.result = str, .ret=Ok};
}

ReturnStatus g__int_format(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer ix = integerVal(Lhs);
  integer length;
  const char *fmt = charsVal(tos[1], &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    return (ReturnStatus){.result = (termPo) allocateChars(processHeap(p), buff, pos), .ret=Ok};
  } else
    return liberror(p, "_int_format", eINVAL);
}

ReturnStatus g__flt_eq(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  termPo Eps = tos[2];

  termPo Rs = (nearlyEqual(floatVal(Lhs), floatVal(Rhs), floatVal(Eps)) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_ge(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (floatVal(Lhs) >= floatVal(Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_lt(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (floatVal(Lhs) < floatVal(Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_plus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) + floatVal(Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_minus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) - floatVal(Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_times(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) * floatVal(Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_div(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) / floatVal(Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_mod(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), fmod(floatVal(Lhs), floatVal(Rhs)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_pwr(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), pow(floatVal(Lhs), floatVal(Rhs)));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt_abs(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (Arg < 0 ? (termPo) allocateFloat(processHeap(p), -Arg) : tos[0]);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_exp(processPo P, ptrPo tos) {
  double x = floatVal(tos[0]);

  errno = 0;    /* clear errno prior to computation */
  double ans = exp(x);    /* allow for checks of the answer */

  if (errno != 0) {
    if (errno == EDOM || errno == ERANGE)
      return liberror(P, "_exp", eRANGE);
    else
      return liberror(P, "_exp", eINVAL);
  } else {
    return (ReturnStatus){.ret=Ok,
              .result=(termPo) allocateFloat(processHeap(P), ans)};
  }
}

ReturnStatus g__ldexp(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  double x = floatVal(Lhs);
  integer e = integerVal(Rhs);

  return (ReturnStatus) {.ret=Ok,
            .result=(termPo) allocateFloat(processHeap(p), ldexp(x, (int) e))};
}

ReturnStatus g__frexp(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  int exp;
  double frac = frexp(Arg, &exp);
  heapPo H = processHeap(p);

  termPo man = (termPo) allocateFloat(H, frac);
  int root = gcAddRoot(H, &man);
  termPo ex = (termPo) allocateInteger(H, (integer) exp);
  gcAddRoot(H, &ex);
  termPo Rs = (termPo) allocatePair(H, man, ex);
  gcReleaseRoot(H, root);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__modf(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  double Arg = floatVal(Lhs);
  double intgrl;
  double frac = modf(Arg, &intgrl);
  heapPo H = processHeap(p);

  termPo man = (termPo) allocateFloat(H, frac);
  int root = gcAddRoot(H, &man);
  termPo ex = (termPo) allocateInteger(H, (integer) intgrl);
  gcAddRoot(H, &ex);
  termPo Rs = (termPo) allocatePair(H, man, ex);
  gcReleaseRoot(H, root);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int2flt(processPo p, ptrPo tos) {
  integer Arg = integerVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), (double) Arg);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt2int(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateInteger(processHeap(p), (integer) Arg);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bits_float(processPo p, ptrPo tos) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), Arg.Dx);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__float_bits(processPo p, ptrPo tos) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(tos[0]);
  termPo Rs = (termPo) allocateInteger(processHeap(p), Arg.Ix);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__flt2str(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  double Arg = floatVal(Lhs);
  char buff[64];

  retCode ret = formatDouble(buff, NumberOf(buff), Arg, general, 0, False);
  if (ret == Ok) {
    return (ReturnStatus) {.ret=Ok,
              .result = (termPo) allocateChars(processHeap(p), buff, uniStrLen(buff))};
  } else
    return liberror(p, "_fltstr", eINVAL);
}

ReturnStatus g__flt_format(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  double Arg = floatVal(Lhs);
  integer length;
  const char *fmt = charsVal(Rhs, &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedFloat(Arg, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    return (ReturnStatus) {.result = (termPo) allocateChars(processHeap(p), buff, uniStrLen(buff)), .ret=Ok};
  } else
    return liberror(p, "_flt_format", eINVAL);
}

ReturnStatus g__flt_hash(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), floatHash(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_cos(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), cos(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_sin(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), sin(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_tan(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), tan(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_acos(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), acos(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_asin(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), asin(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_atan(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), atan(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_floor(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), floor(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_ceil(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), ceil(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_trunc(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), trunc(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_integral(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);

  return (ReturnStatus) {.ret=Ok,
            .result=(floor(Arg) == Arg ? trueEnum : falseEnum)};
}

ReturnStatus g_log(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), log(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_log10(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), log10(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_sqrt(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), sqrt(Arg));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g_pi(processPo p, ptrPo tos) {
  termPo Rs = (termPo) allocateFloat(processHeap(p), M_PI);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__irand(processPo P, ptrPo tos) {
  integer mx = integerVal(tos[0]);
  integer rnd = randomInt();

  return (ReturnStatus) {.ret=Ok,
            .result=(termPo) allocateInteger(processHeap(P), rnd % mx)};
}

ReturnStatus g__random(processPo P, ptrPo tos) {
  double rnd = random()/(double)LARGE_INT32;

  return (ReturnStatus) {.ret=Ok,
            .result=(termPo) allocateFloat(processHeap(P), rnd)};
}

ReturnStatus g__seed(processPo P,ptrPo tos){
  srandom((unsigned int)integerVal(tos[0]));
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}


