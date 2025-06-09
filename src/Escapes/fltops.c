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
#include "stack.h"
#include "escape.h"

ReturnStatus g__flt_eq(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  double fuzz = rhs / 1.0e20;
  termPo Rs = (nearlyEqual(lhs, rhs, fuzz) ? trueEnum : falseEnum);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_ge(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (lhs >= rhs ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;;
}

ReturnStatus g__flt_lt(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (lhs < rhs ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_plus(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (termPo) makeFloat(lhs + rhs);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_minus(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = makeFloat(lhs - rhs);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_times(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = makeFloat(lhs * rhs);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_div(processPo P) {
  double numer = floatVal(popVal(P));
  double denom = floatVal(popVal(P));

  if (denom == 0.0) {
    pshVal(P, divZero);
    return Abnormal;
  } else {
    pshVal(P, makeFloat(numer / denom));
    return Normal;
  }
}

ReturnStatus g__flt_mod(processPo P) {
  double numer = floatVal(popVal(P));
  double denom = floatVal(popVal(P));

  if (denom == 0.0) {
    pshVal(P, divZero);
    return Abnormal;
  } else {
    pshVal(P, makeFloat(fmod(numer, denom)));
    return Normal;
  }
}

ReturnStatus g__flt_pwr(processPo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = makeFloat(pow(lhs, rhs));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_abs(processPo P) {
  termPo arg = popVal(P);
  double Arg = floatVal(arg);
  termPo Rs = (Arg < 0 ? makeFloat(-Arg) : arg);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_exp(processPo P) {
  double x = floatVal(popVal(P));

  errno = 0; /* clear errno prior to computation */
  double ans = exp(x); /* allow for checks of the answer */

  if (errno != 0) {
    if (errno == EDOM || errno == ERANGE) {
      pshVal(P, eRANGE);
      return Abnormal;
    } else {
      pshVal(P, eINVAL);
      return Abnormal;
    }
  } else {
    pshVal(P, makeFloat(ans));
    return Normal;
  }
}

ReturnStatus g__ldexp(processPo P) {
  double x = floatVal(popVal(P));
  integer e = integerVal(popVal(P));
  pshVal(P, makeFloat(ldexp(x, (int) e)));
  return Normal;
}

ReturnStatus g__frexp(processPo P) {
  double Arg = floatVal(popVal(P));
  int exp;
  double frac = frexp(Arg, &exp);

  termPo man = makeFloat(frac);
  int root = gcAddRoot(currentHeap, &man);
  termPo ex = makeInteger((integer) exp);
  gcAddRoot(currentHeap, &ex);
  termPo Rs = (termPo) allocatePair(currentHeap, man, ex);
  gcReleaseRoot(currentHeap, root);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__modf(processPo P) {
  double Arg = floatVal(popVal(P));
  double intgrl;
  double frac = modf(Arg, &intgrl);

  termPo man = makeFloat(frac);
  int root = gcAddRoot(currentHeap, &man);
  termPo ex = makeInteger((integer) intgrl);
  gcAddRoot(currentHeap, &ex);
  termPo Rs = (termPo) allocatePair(currentHeap, man, ex);
  gcReleaseRoot(currentHeap, root);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt2int(processPo P) {
  termPo Rs = makeInteger((integer) floatVal(popVal(P)));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__bits_float(processPo P) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(popVal(P));
  termPo Rs = makeFloat(Arg.Dx);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__float_bits(processPo P) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(popVal(P));
  termPo Rs = makeInteger(Arg.Ix);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt2str(processPo P) {
  double Arg = floatVal(popVal(P));
  int precision = (int) integerVal(popVal(P));
  codePoint mdc = charVal(popVal(P));
  FloatDisplayMode mode = (mdc == 'f' ? fractional : mdc == 's' ? scientific : general);
  char buff[64];

  formatDouble(buff, NumberOf(buff), Arg, mode, precision, popVal(P) == trueEnum ? True : False);

  pshVal(P, (termPo) allocateString(currentHeap, buff, uniStrLen(buff)));
  return Normal;
}

ReturnStatus g__flt_format(processPo P) {
  double arg = floatVal(popVal(P));
  integer length;
  const char *fmt = strVal(popVal(P), &length);
  char buff[64];
  integer pos = 0;

  if (formattedFloat(arg, buff, &pos, NumberOf(buff), fmt, length) == Ok) {
    pshVal(P, (termPo) allocateString(currentHeap, buff, uniStrLen(buff)));
    return Normal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__flt_hash(processPo P) {
  double Arg = floatVal(popVal(P));

  termPo Rs = makeInteger(floatHash(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_cos(processPo P) {
  double Arg = floatVal(popVal(P));
  termPo Rs = makeFloat(cos(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_sin(processPo P) {
  termPo Rs = makeFloat(sin(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_tan(processPo P) {
  termPo Rs = makeFloat(tan(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_acos(processPo P) {
  termPo Rs = makeFloat(acos(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_asin(processPo P) {
  termPo Rs = makeFloat(asin(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_atan(processPo P) {
  termPo Rs = makeFloat(atan(floatVal(popVal(P))));
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_floor(processPo P) {
  termPo Rs = makeFloat(floor(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_ceil(processPo P) {
  termPo Rs = makeFloat(ceil(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_trunc(processPo P) {
  termPo Rs = makeFloat(trunc(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_integral(processPo P) {
  double Arg = floatVal(popVal(P));

  pshVal(P, floor(Arg) == Arg ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g_log(processPo P) {
  double Arg = floatVal(popVal(P));
  termPo Rs = makeFloat(log(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_log10(processPo P) {
  double Arg = floatVal(popVal(P));
  termPo Rs = makeFloat(log10(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_sqrt(processPo P) {
  double Arg = floatVal(popVal(P));

  if (Arg >= 0.0){
    pshVal(P,makeFloat(sqrt(Arg)));
    return Normal;
  }
  else{
    pshVal(P,eRANGE);
    return Abnormal;
  }
}

ReturnStatus g_pi(processPo P) {
  termPo Rs = makeFloat(M_PI);

  pshVal(P, Rs);
  return Normal;
}
