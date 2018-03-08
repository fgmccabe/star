//
// Created by Francis McCabe on 1/6/18.
//


#include <math.h>
#include <str.h>
#include <tpl.h>
#include "ooio.h"
#include "engine.h"
#include "arithP.h"
#include "errorCodes.h"

ReturnStatus g__int_plus(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) + integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_minus(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) - integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_times(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) * integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_div(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) / integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_mod(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) % integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__band(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) & integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__basr(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) >> integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__blsl(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) << integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__blsr(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), ((unsigned) integerVal(Lhs)) >> integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__bor(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) | integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__bxor(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateInteger(processHeap(p), integerVal(Lhs) ^ integerVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__bnot(processPo p, ptrPo tos) {
  termPo Rs = (termPo) allocateInteger(processHeap(p), ~integerVal(tos[0]));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_eq(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (integerVal(Lhs) == integerVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_ge(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (integerVal(Lhs) >= integerVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_lt(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (integerVal(Lhs) < integerVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int_abs(processPo p, ptrPo tos) {
  integer Arg = integerVal(tos[0]);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), -Arg) : tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int2str(processPo p, ptrPo tos) {
  integer ix = integerVal(tos[0]);
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  termPo str = allocateString(processHeap(p), buff, len);

  ReturnStatus rtn = {.rslt = str, .ret=Ok};
  return rtn;
}

ReturnStatus g__flt_eq(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (floatVal(Lhs) == floatVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_ge(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (floatVal(Lhs) >= floatVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_lt(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (floatVal(Lhs) < floatVal(Rhs) ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_plus(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) + floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_minus(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) - floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_times(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) * floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_div(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), floatVal(Lhs) / floatVal(Rhs));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_mod(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

  termPo Rs = (termPo) allocateFloat(processHeap(p), fmod(floatVal(Lhs), floatVal(Rhs)));

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt_pwr(processPo p, ptrPo tos) {
  termPo Rhs = tos[0];
  termPo Lhs = tos[1];

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

ReturnStatus g__ldexp(processPo p, ptrPo tos) {
  double x = floatVal(tos[1]);
  integer e = integerVal(tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) allocateFloat(processHeap(p), ldexp(x, (int) e))};

  return ret;
}

ReturnStatus g__frexp(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  int exp;
  double frac = frexp(Arg, &exp);

  termPo man = (termPo) allocateFloat(processHeap(p), frac);
  int root = gcAddRoot(&man);
  termPo ex = (termPo) allocateInteger(processHeap(p), (integer) exp);
  gcAddRoot(&ex);
  termPo Rs = allocatePair(processHeap(p), man, ex);
  gcReleaseRoot(root);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__modf(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  double intgrl;
  double frac = modf(Arg, &intgrl);

  termPo man = (termPo) allocateFloat(processHeap(p), frac);
  int root = gcAddRoot(&man);
  termPo ex = (termPo) allocateInteger(processHeap(p), (integer) intgrl);
  gcAddRoot(&ex);
  termPo Rs = allocatePair(processHeap(p), man, ex);
  gcReleaseRoot(root);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__int2flt(processPo p, ptrPo tos) {
  integer Arg = integerVal(tos[0]);
  termPo Rs = (Arg < 0 ? (termPo) allocateFloat(processHeap(p), (double) Arg) : tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt2int(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), (integer) Arg) : tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

  return ret;
}

ReturnStatus g__flt2str(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  char buff[64];

  retCode ret = formatDouble(buff, NumberOf(buff), Arg, general, 0, False);
  if (ret == Ok) {
    ReturnStatus rtn = {.ret=Ok, .rslt = allocateString(processHeap(p), buff, uniStrLen(buff))};
    return rtn;
  } else
    return liberror(p, "_fltstr", eINVAL);
}

ReturnStatus g__flt_format(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  integer length;
  const char *fmt = stringVal(tos[0], &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedFloat(Arg, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    ReturnStatus rtn = {.rslt = allocateString(processHeap(p), buff, uniStrLen(buff)),.ret=Ok};
    return rtn;
  } else
    return liberror(p, "_flt_format", eINVAL);
}

ReturnStatus g__flt_hash(processPo p, ptrPo tos) {
  double Arg = floatVal(tos[0]);
  termPo Rs = (Arg < 0 ? (termPo) allocateInteger(processHeap(p), (integer) (Arg)) : tos[0]);

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

  termPo Rs = (floor(Arg) == Arg ? trueEnum : falseEnum);

  ReturnStatus ret = {.ret=Ok, .rslt=Rs};

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
