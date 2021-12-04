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
#include "consP.h"

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
