//
// Created by Francis McCabe on 1/6/18.
//


#include <math.h>
#include <strings.h>
#include <tpl.h>
#include <errno.h>
#include "char.h"
#include "arithP.h"
#include "errorCodes.h"

ReturnStatus g__flt_eq(heapPo h, termPo a1, termPo a2) {
  double fuzz = floatVal(a2) / 1.0e20;
  termPo Rs = (nearlyEqual(floatVal(a1), floatVal(a2), fuzz) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_ge(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (floatVal(a1) >= floatVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_lt(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (floatVal(a1) < floatVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_plus(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (termPo) makeFloat(floatVal(a1) + floatVal(a2));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_minus(heapPo h, termPo a1, termPo a2) {
  termPo Rs = makeFloat(floatVal(a1) - floatVal(a2));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_times(heapPo h, termPo a1, termPo a2) {
  termPo Rs = makeFloat(floatVal(a1) * floatVal(a2));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_div(heapPo h, termPo xc, termPo a1, termPo a2) {
  double denom = floatVal(a2);

  if (denom == 0.0) {
    return (ReturnStatus) {.ret=Abnormal, .result=divZero};
  } else {
    termPo Rs = makeFloat(floatVal(a1) / denom);

    return (ReturnStatus) {.ret=Normal, .result=Rs};
  }
}

ReturnStatus g__flt_mod(heapPo h, termPo xc, termPo a1, termPo a2) {
  double denom = floatVal(a2);

  if (denom == 0.0) {
    return (ReturnStatus) {.ret=Abnormal, .result=divZero};
  } else {
    termPo Rs = makeFloat(fmod(floatVal(a1), floatVal(a2)));

    return (ReturnStatus) {.ret=Normal, .result=Rs};
  }
}

ReturnStatus g__flt_pwr(heapPo h, termPo a1, termPo a2) {
  termPo Rs = makeFloat(pow(floatVal(a1), floatVal(a2)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt_abs(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = (Arg < 0 ? makeFloat(-Arg) : arg1);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_exp(heapPo h, termPo xc, termPo arg1) {
  double x = floatVal(arg1);

  errno = 0;    /* clear errno prior to computation */
  double ans = exp(x);    /* allow for checks of the answer */

  if (errno != 0) {
    if (errno == EDOM || errno == ERANGE)
      return (ReturnStatus) {.ret=Abnormal, .result=eRANGE};
    else
      return (ReturnStatus) {.ret=Abnormal, .result=eINVAL};
  } else {
    return (ReturnStatus) {.ret=Normal, .result=makeFloat(ans)};
  }
}

ReturnStatus g__ldexp(heapPo h, termPo a1, termPo a2) {
  double x = floatVal(a1);
  integer e = integerVal(a2);

  return (ReturnStatus) {.ret=Normal,
    .result=makeFloat(ldexp(x, (int) e))};
}

ReturnStatus g__frexp(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  int exp;
  double frac = frexp(Arg, &exp);
  heapPo H = h;

  termPo man = makeFloat(frac);
  int root = gcAddRoot(H, &man);
  termPo ex = makeInteger((integer) exp);
  gcAddRoot(H, &ex);
  termPo Rs = (termPo) allocatePair(H, man, ex);
  gcReleaseRoot(H, root);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__modf(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  double intgrl;
  double frac = modf(Arg, &intgrl);
  heapPo H = h;

  termPo man = makeFloat(frac);
  int root = gcAddRoot(H, &man);
  termPo ex = makeInteger((integer) intgrl);
  gcAddRoot(H, &ex);
  termPo Rs = (termPo) allocatePair(H, man, ex);
  gcReleaseRoot(H, root);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt2int(heapPo h, termPo arg1) {
  termPo Rs = makeInteger((integer) floatVal(arg1));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__bits_float(heapPo h, termPo arg1) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(arg1);
  termPo Rs = makeFloat(Arg.Dx);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__float_bits(heapPo h, termPo arg1) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(arg1);
  termPo Rs = makeInteger(Arg.Ix);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g__flt2str(heapPo h, termPo arg1, termPo p, termPo m, termPo s) {
  double Arg = floatVal(arg1);
  int precision = (int) integerVal(p);
  codePoint mdc = charVal(m);
  FloatDisplayMode mode = (mdc == 'f' ? fractional : mdc == 's' ? scientific : general);
  char buff[64];

  formatDouble(buff, NumberOf(buff), Arg, mode, precision, s == trueEnum ? True : False);

  return (ReturnStatus) {.ret=Normal,
    .result = (termPo) allocateString(h, buff, uniStrLen(buff))};
}

ReturnStatus g__flt_format(heapPo h, termPo a1, termPo a2) {
  integer length;
  const char *fmt = strVal(a2, &length);
  char buff[64];
  integer pos = 0;

  formattedFloat(floatVal(a1), buff, &pos, NumberOf(buff), fmt, length);

  return (ReturnStatus) {.ret=Normal, .result = (termPo) allocateString(h, buff, uniStrLen(buff))};
}

ReturnStatus g__flt_hash(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);

  termPo Rs = makeInteger(floatHash(Arg));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_cos(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = makeFloat(cos(Arg));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_sin(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(sin(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_tan(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(tan(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_acos(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(acos(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_asin(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(asin(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_atan(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(atan(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_floor(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(floor(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_ceil(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(ceil(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_trunc(heapPo h, termPo arg1) {
  termPo Rs = makeFloat(trunc(floatVal(arg1)));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_integral(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);

  return (ReturnStatus) {.ret=Normal,
    .result=(floor(Arg) == Arg ? trueEnum : falseEnum)};
}

ReturnStatus g_log(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = makeFloat(log(Arg));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_log10(heapPo h, termPo arg1) {
  double Arg = floatVal(arg1);
  termPo Rs = makeFloat(log10(Arg));

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}

ReturnStatus g_sqrt(heapPo h, termPo xc, termPo arg1) {
  double Arg = floatVal(arg1);

  if (Arg < 0.0)
    return (ReturnStatus) {.ret=Abnormal, .result=eRANGE};
  else {
    return (ReturnStatus) {.ret=Normal, .result=makeFloat(sqrt(Arg))};
  }
}

ReturnStatus g_pi(heapPo h) {
  termPo Rs = makeFloat(M_PI);

  return (ReturnStatus) {.ret=Normal, .result=Rs};
}
