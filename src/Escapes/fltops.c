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

ReturnStatus g__flt_eq(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (lhs == rhs ? trueEnum : falseEnum);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_ge(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (lhs >= rhs ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;;
}

ReturnStatus g__flt_lt(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (lhs < rhs ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_plus(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = (termPo) makeFloat(lhs + rhs);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_minus(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = makeFloat(lhs - rhs);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_times(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = makeFloat(lhs * rhs);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_div(enginePo P) {
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

ReturnStatus g__flt_mod(enginePo P) {
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

ReturnStatus g__flt_pwr(enginePo P) {
  double lhs = floatVal(popVal(P));
  double rhs = floatVal(popVal(P));
  termPo Rs = makeFloat(pow(lhs, rhs));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt_abs(enginePo P) {
  termPo arg = popVal(P);
  double Arg = floatVal(arg);
  termPo Rs = (Arg < 0 ? makeFloat(-Arg) : arg);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_exp(enginePo P) {
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

ReturnStatus g__ldexp(enginePo P) {
  double x = floatVal(popVal(P));
  integer e = integerVal(popVal(P));
  pshVal(P, makeFloat(ldexp(x, (int) e)));
  return Normal;
}

ReturnStatus g__frexp(enginePo P) {
  double Arg = floatVal(popVal(P));
  int exp;
  double frac = frexp(Arg, &exp);
  termPo man = makeFloat(frac);

  heapPo h = processHeap(P);
  int root = gcAddRoot(h, &man);
  termPo ex = makeInteger((integer) exp);
  gcAddRoot(h, &ex);
  termPo Rs = (termPo) allocatePair(h, man, ex);
  gcReleaseRoot(h, root);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__modf(enginePo P) {
  double Arg = floatVal(popVal(P));
  double intgrl;
  double frac = modf(Arg, &intgrl);

  termPo man = makeFloat(frac);
  heapPo h = processHeap(P);
  int root = gcAddRoot(h, &man);
  termPo ex = makeInteger((integer) intgrl);
  gcAddRoot(h, &ex);
  termPo Rs = (termPo) allocatePair(h, man, ex);
  gcReleaseRoot(h, root);
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt2int(enginePo P) {
  double dx = floatVal(popVal(P));

  if (floor(dx) == dx) {
    integer ix = (integer) dx;
    if ((double) ix == dx) {
      pshVal(P, makeInteger(ix));
      return Normal;
    } else {
      pshVal(P, eRANGE);
      return Abnormal;
    }
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__bits_float(enginePo P) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(popVal(P));
  termPo Rs = makeFloat(Arg.Dx);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__float_bits(enginePo P) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(popVal(P));
  termPo Rs = makeInteger(Arg.Ix);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__flt2str(enginePo P) {
  double Arg = floatVal(popVal(P));
  int precision = (int) integerVal(popVal(P));
  codePoint mdc = charVal(popVal(P));
  FloatDisplayMode mode = (mdc == 'f' ? fractional : mdc == 's' ? scientific : general);
  char buff[64];

  formatDouble(buff, NumberOf(buff), Arg, mode, precision, popVal(P) == trueEnum ? True : False);

  pshVal(P, (termPo) allocateString(processHeap(P), buff, uniStrLen(buff)));
  return Normal;
}

ReturnStatus g__flt_format(enginePo P) {
  double arg = floatVal(popVal(P));
  integer length;
  const char *fmt = strVal(popVal(P), &length);
  char buff[64];
  integer pos = 0;

  if (formattedFloat(arg, buff, &pos, NumberOf(buff), fmt, length) == Ok) {
    pshVal(P, (termPo) allocateString(processHeap(P), buff, uniStrLen(buff)));
    return Normal;
  } else {
    pshVal(P, eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__flt_hash(enginePo P) {
  double Arg = floatVal(popVal(P));

  termPo Rs = makeInteger(floatHash(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_cos(enginePo P) {
  double Arg = floatVal(popVal(P));
  termPo Rs = makeFloat(cos(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_sin(enginePo P) {
  termPo Rs = makeFloat(sin(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_tan(enginePo P) {
  termPo Rs = makeFloat(tan(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_acos(enginePo P) {
  termPo Rs = makeFloat(acos(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_asin(enginePo P) {
  termPo Rs = makeFloat(asin(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_atan(enginePo P) {
  termPo Rs = makeFloat(atan(floatVal(popVal(P))));
  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_floor(enginePo P) {
  termPo Rs = makeFloat(floor(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_ceil(enginePo P) {
  termPo Rs = makeFloat(ceil(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_trunc(enginePo P) {
  termPo Rs = makeFloat(trunc(floatVal(popVal(P))));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_integral(enginePo P) {
  double Arg = floatVal(popVal(P));

  pshVal(P, floor(Arg) == Arg ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__ln(enginePo P) {
  double Arg = floatVal(popVal(P));
  termPo Rs = makeFloat(log(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__lg10(enginePo P) {
  double Arg = floatVal(popVal(P));
  termPo Rs = makeFloat(log10(Arg));

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g_sqrt(enginePo P) {
  double Arg = floatVal(popVal(P));

  if (Arg >= 0.0) {
    pshVal(P, makeFloat(sqrt(Arg)));
    return Normal;
  } else {
    pshVal(P, eRANGE);
    return Abnormal;
  }
}

ReturnStatus g_pi(enginePo P) {
  termPo Rs = makeFloat(M_PI);

  pshVal(P, Rs);
  return Normal;
}
