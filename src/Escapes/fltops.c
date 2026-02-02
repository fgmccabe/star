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

ValueReturn s_flt_eq(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(lhs == rhs ? trueEnum : falseEnum);
}

ReturnStatus g__flt_eq(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_eq(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_ge(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(lhs >= rhs ? trueEnum : falseEnum);
}

ReturnStatus g__flt_ge(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_ge(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_lt(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(lhs < rhs ? trueEnum : falseEnum);
}

ReturnStatus g__flt_lt(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_eq(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_plus(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(makeFloat(lhs+rhs));
}

ReturnStatus g__flt_plus(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_plus(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_minus(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(makeFloat(lhs-rhs));
}

ReturnStatus g__flt_minus(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_minus(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_times(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(makeFloat(lhs*rhs));
}

ReturnStatus g__flt_times(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_times(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_div(enginePo P, termPo l, termPo r) {
  double numer = floatVal(l);
  double denom = floatVal(r);

  if (denom == 0.0) {
    return abnormalReturn(divZero);
  } else {
    return normalReturn(makeFloat(numer/denom));
  }
}

ReturnStatus g__flt_div(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_div(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_mod(enginePo P, termPo l, termPo r) {
  double numer = floatVal(l);
  double denom = floatVal(r);

  if (denom == 0.0) {
    return abnormalReturn(divZero);
  } else {
    return normalReturn(makeFloat(fmod(numer,denom)));
  }
}

ReturnStatus g__flt_mod(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_mod(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt_pwr(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);

  return normalReturn(makeFloat(pow(lhs,rhs)));
}

ReturnStatus g__flt_pwr(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_flt_pwr(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ReturnStatus g__flt_abs(enginePo P) {
  termPo arg = popVal(P);
  double Arg = floatVal(arg);
  termPo Rs = (Arg < 0 ? makeFloat(-Arg) : arg);

  pshVal(P, Rs);
  return Normal;
}

ValueReturn s_exp(enginePo P, termPo l) {
  double x = floatVal(l);

  errno = 0; /* clear errno prior to computation */
  double ans = exp(x); /* allow for checks of the answer */

  if (errno != 0) {
    if (errno == EDOM || errno == ERANGE) {
      return abnormalReturn(eRANGE);
    } else {
      return abnormalReturn(eINVAL);
    }
  } else {
    return normalReturn(makeFloat(ans));
  }
}

ReturnStatus g_exp(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_exp(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_ldexp(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeFloat(ldexp(lhs,rhs)));
}

ReturnStatus g__ldexp(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s_ldexp(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_frexp(enginePo P, termPo l) {
  double lhs = floatVal(l);
  int exp;
  double frac = frexp(lhs, &exp);
  termPo man = makeFloat(frac);

  heapPo h = processHeap(P);
  int root = gcAddRoot(h, &man);
  termPo ex = makeInteger((integer) exp);
  gcAddRoot(h, &ex);
  termPo Rs = (termPo) allocatePair(h, man, ex);
  gcReleaseRoot(h, root);
  return normalReturn(Rs);
}

ReturnStatus g__frexp(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_frexp(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_modf(enginePo P, termPo l) {
  double Arg = floatVal(l);
  double intgrl;
  double frac = modf(Arg, &intgrl);

  termPo man = makeFloat(frac);
  heapPo h = processHeap(P);
  int root = gcAddRoot(h, &man);
  termPo ex = makeInteger((integer) intgrl);
  gcAddRoot(h, &ex);
  termPo Rs = (termPo) allocatePair(h, man, ex);
  gcReleaseRoot(h, root);

  return normalReturn(Rs);
}

ReturnStatus g__modf(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_modf(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_flt2int(enginePo P, termPo l) {
  double dx = floatVal(l);

  if (floor(dx) == dx) {
    integer ix = (integer) dx;
    if ((double) ix == dx) {
      return normalReturn(makeInteger(ix));
    } else {
      return abnormalReturn(eRANGE);
    }
  } else {
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__flt2int(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_flt2int(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__bits_float(enginePo P, termPo l) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(l);
  return normalReturn(makeFloat(Arg.Dx));
}

ReturnStatus g__bits_float(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__bits_float(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__float_bits(enginePo P, termPo l) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(l);
  return normalReturn(makeInteger(Arg.Ix));
}

ReturnStatus g__float_bits(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__float_bits(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__flt2str(enginePo P, termPo l, termPo r, termPo s) {
  double Arg = floatVal(l);

  int precision = (int) integerVal(r);
  codePoint mdc = charVal(s);
  FloatDisplayMode mode = (mdc == 'f' ? fractional : mdc == 's' ? scientific : general);
  char buff[64];

  formatDouble(buff, NumberOf(buff), Arg, mode, precision, popVal(P) == trueEnum ? True : False);

  return normalReturn((termPo) allocateString(processHeap(P), buff, uniStrLen(buff)));
}

ReturnStatus g__flt2str(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  termPo s = popVal(P);
  ValueReturn ret = s__flt2str(P, l, r, s);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__flt_format(enginePo P, termPo l, termPo r) {
  double arg = floatVal(l);
  integer length;
  const char *fmt = strVal(r, &length);
  char buff[64];
  integer pos = 0;

  if (formattedFloat(arg, buff, &pos, NumberOf(buff), fmt, length) == Ok)
    return normalReturn((termPo) allocateString(processHeap(P), buff, uniStrLen(buff)));
  else
    return abnormalReturn(eINVAL);
}

ReturnStatus g__flt_format(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);
  ValueReturn ret = s__flt_format(P, l, r);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__flt_hash(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeInteger(floatHash(Arg)));
}

ReturnStatus g__flt_hash(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__flt_hash(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_cos(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(cos(Arg)));
}

ReturnStatus g_cos(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_cos(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_sin(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(sin(Arg)));
}

ReturnStatus g_sin(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_sin(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_tan(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(tan(Arg)));
}

ReturnStatus g_tan(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_tan(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_acos(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(acos(Arg)));
}

ReturnStatus g_acos(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_acos(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_asin(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(asin(Arg)));
}

ReturnStatus g_asin(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_asin(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_atan(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(atan(Arg)));
}

ReturnStatus g_atan(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_atan(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_floor(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(floor(Arg)));
}

ReturnStatus g_floor(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_floor(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_ceil(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(ceil(Arg)));
}

ReturnStatus g_ceil(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_ceil(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_trunc(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(trunc(Arg)));
}

ReturnStatus g_trunc(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_trunc(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_integral(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(floor(Arg) == Arg ? trueEnum : falseEnum);
}

ReturnStatus g_integral(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_integral(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__ln(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(log(Arg)));
}

ReturnStatus g__ln(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__ln(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__lg10(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(log10(Arg)));
}

ReturnStatus g__lg10(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__lg10(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_sqrt(enginePo P, termPo l) {
  double Arg = floatVal(l);
  if (Arg >= 0.0)
    return normalReturn(makeFloat(sqrt(Arg)));
  else
    return abnormalReturn(eRANGE);
}

ReturnStatus g_sqrt(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s_sqrt(P, l);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s_pi(enginePo P) {
  return normalReturn(makeFloat(M_PI));
}

ReturnStatus g_pi(enginePo P) {
  ValueReturn ret = s_pi(P);
  pshVal(P, ret.value);
  return ret.status;
}
