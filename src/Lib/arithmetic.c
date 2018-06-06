//
// Created by Francis McCabe on 1/6/18.
//


#include <math.h>
#include <str.h>
#include <tpl.h>
#include <stdlib.h>
#include <bsd/stdlib.h>
#include <errno.h>
#include "ooio.h"
#include "engine.h"
#include "arithP.h"
#include "errorCodes.h"
#include "arithmetic.h"

ReturnStatus g__int_plus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) + integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_minus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) - integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_times(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) * integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_div(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) / integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_mod(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) % integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__band(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) & integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__basr(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) >> integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__blsl(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) << integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__blsr(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), ((unsigned) integerVal(Lhs)) >> integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__bor(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) | integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__bxor(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) ^ integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__bnot(processPo p, ptrPo tos) {
  termPo Rs = (termPo) allocateInteger(processHeap(p), ~integerVal(tos[0]));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__nthb(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  uinteger ix = (unsigned) integerVal(Lhs);
  byte b = (byte) integerVal(Rhs);

  termPo Rs = (ix & (1 << b) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_eq(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (integerVal(Lhs) == integerVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_ge(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (integerVal(Lhs) >= integerVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_lt(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (integerVal(Lhs) < integerVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_abs(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer Arg = integerVal(Lhs);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), -Arg) : Lhs);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_hash(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer Arg = integerVal(Lhs);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), hash64(Arg)) : Lhs);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int2str(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer ix = integerVal(Lhs);
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  termPo str = (termPo) allocateString(processHeap(p), buff, len);

  ReturnStatus rtn = {.rslt = str, .ret=Ok};
  return rtn;
}

ReturnStatus g__int_format(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer ix = integerVal(Lhs);
  integer length;
  const char *fmt = stringVal(tos[1], &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    ReturnStatus rtn = {.rslt = (termPo) allocateString(processHeap(p), buff, uniStrLen(buff)), .ret=Ok};
    return rtn;
  } else
    return liberror(p, "_int_format", eINVAL);
}

ReturnStatus g__flt_eq(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (floatVal(Lhs) == floatVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_ge(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (floatVal(Lhs) >= floatVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_lt(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (floatVal(Lhs) < floatVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_plus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) + floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_minus(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) - floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_times(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) * floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_div(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) / floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_mod(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), fmod(floatVal(Lhs), floatVal(Rhs)));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_pwr(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), pow(floatVal(Lhs), floatVal(Rhs)));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_abs(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (Arg < 0 ? (termPo) allocateFloat(processHeap(p), -Arg) : tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__exp(processPo P, ptrPo tos) {
  double x = floatVal(tos[0]);

  errno = 0;    /* clear errno prior to computation */
  double ans = exp(x);    /* allow for checks of the answer */

  if (errno != 0) {
    if (errno == EDOM || errno == ERANGE)
      return liberror(P, "_exp", eRANGE);
    else
      return liberror(P, "_exp", eINVAL);
  } else {
    ReturnStatus rt = {.ret=Ok, .rslt=(termPo) allocateFloat(processHeap(P), ans)};
    return rt;
  }
}

ReturnStatus g__ldexp(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  double x = floatVal(Lhs);
  integer e = integerVal(Rhs);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) allocateFloat(processHeap(p), ldexp(x, (int) e))};

  return ret;
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

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
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

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int2flt(processPo p, ptrPo tos) {
  integer Arg = integerVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), (double) Arg);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt2int(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateInteger(processHeap(p), (integer) Arg);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt2str(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  double Arg = floatVal(Lhs);
  char buff[64];

  retCode ret = formatDouble(buff, NumberOf(buff), Arg, general, 0, False);
  if (ret == Ok) {
    ReturnStatus rtn = {.ret=Ok, .rslt = (termPo) allocateString(processHeap(p), buff, uniStrLen(buff))};
    return rtn;
  } else
    return liberror(p, "_fltstr", eINVAL);
}

ReturnStatus g__flt_format(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  double Arg = floatVal(Lhs);
  integer length;
  const char *fmt = stringVal(Rhs, &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedFloat(Arg, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    ReturnStatus rtn = {.rslt = (termPo) allocateString(processHeap(p), buff, uniStrLen(buff)), .ret=Ok};
    return rtn;
  } else
    return liberror(p, "_flt_format", eINVAL);
}

ReturnStatus g__flt_hash(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);

  termPo Rs = (termPo) allocateInteger(processHeap(p), floatHash(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_cos(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), cos(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_sin(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), sin(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_tan(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), tan(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_acos(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), acos(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_asin(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), asin(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_atan(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), atan(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_floor(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), floor(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_ceil(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), ceil(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_trunc(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), trunc(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_integral(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=(floor(Arg) == Arg ? trueEnum : falseEnum)};

  return ret;
}

ReturnStatus g_log(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), log(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_log10(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), log10(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_sqrt(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (termPo) allocateFloat(processHeap(p), sqrt(Arg));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g_pi(processPo p, ptrPo tos) {
  termPo Rs = (termPo) allocateFloat(processHeap(p), M_PI);
  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__irand(processPo P, ptrPo tos) {
  integer mx = integerVal(tos[0]);
  integer rnd = randomInt();

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) allocateInteger(processHeap(P), rnd % mx)};
  return ret;
}

integer randomInt() {
  integer rnd;

  arc4random_buf((void *) &rnd, sizeof(rnd));
  return rnd;
}
