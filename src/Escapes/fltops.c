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

ValueReturn s__flt_eq(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(lhs == rhs ? trueEnum : falseEnum);
}

ValueReturn s__flt_ge(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(lhs >= rhs ? trueEnum : falseEnum);
}

ValueReturn s__flt_lt(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(lhs < rhs ? trueEnum : falseEnum);
}

ValueReturn s__flt_plus(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(makeFloat(processHeap(P),lhs+rhs));
}

ValueReturn s__flt_minus(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(makeFloat(processHeap(P),lhs-rhs));
}

ValueReturn s__flt_times(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);
  return normalReturn(makeFloat(processHeap(P),lhs*rhs));
}

ValueReturn s__flt_div(enginePo P, termPo l, termPo r) {
  double numer = floatVal(l);
  double denom = floatVal(r);

  if (denom == 0.0) {
    return abnormalReturn(divZero);
  } else {
    return normalReturn(makeFloat(processHeap(P),numer/denom));
  }
}

ValueReturn s__flt_mod(enginePo P, termPo l, termPo r) {
  double numer = floatVal(l);
  double denom = floatVal(r);

  if (denom == 0.0) {
    return abnormalReturn(divZero);
  } else {
    return normalReturn(makeFloat(processHeap(P),fmod(numer,denom)));
  }
}

ValueReturn s__flt_pwr(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  double rhs = floatVal(r);

  return normalReturn(makeFloat(processHeap(P),pow(lhs,rhs)));
}

ValueReturn s__flt_abs(enginePo P, termPo l) {
  double lhs = floatVal(l);

  return normalReturn(lhs < 0 ? makeFloat(processHeap(P),-lhs) : l);
}

ValueReturn s__exp(enginePo P, termPo l) {
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
    return normalReturn(makeFloat(processHeap(P),ans));
  }
}

ValueReturn s__ldexp(enginePo P, termPo l, termPo r) {
  double lhs = floatVal(l);
  integer rhs = integerVal(r);

  return normalReturn(makeFloat(processHeap(P),ldexp(lhs,rhs)));
}

ValueReturn s__frexp(enginePo P, termPo l) {
  double lhs = floatVal(l);
  int exp;
  double frac = frexp(lhs, &exp);
  termPo man = makeFloat(processHeap(P), frac);

  heapPo h = processHeap(P);
  int root = gcAddRoot(h, &man);
  termPo ex = makeInteger((integer) exp);
  gcAddRoot(h, &ex);
  termPo Rs = (termPo) allocatePair(h, man, ex);
  gcReleaseRoot(h, root);
  return normalReturn(Rs);
}

ValueReturn s__modf(enginePo P, termPo l) {
  double Arg = floatVal(l);
  double intgrl;
  double frac = modf(Arg, &intgrl);

  termPo man = makeFloat(processHeap(P), frac);
  heapPo h = processHeap(P);
  int root = gcAddRoot(h, &man);
  termPo ex = makeInteger((integer) intgrl);
  gcAddRoot(h, &ex);
  termPo Rs = (termPo) allocatePair(h, man, ex);
  gcReleaseRoot(h, root);

  return normalReturn(Rs);
}

ValueReturn s__flt2int(enginePo P, termPo l) {
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

ValueReturn s__bits_float(enginePo P, termPo l) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Ix = integerVal(l);
  return normalReturn(makeFloat(processHeap(P),Arg.Dx));
}

ValueReturn s__float_bits(enginePo P, termPo l) {
  union {
    integer Ix;
    double Dx;
  } Arg;
  Arg.Dx = floatVal(l);
  return normalReturn(makeInteger(Arg.Ix));
}

ValueReturn s__flt2str(enginePo P, termPo l, termPo r, termPo s, termPo a) {
  double Arg = floatVal(l);

  int precision = (int) integerVal(r);
  codePoint mdc = charVal(s);
  FloatDisplayMode mode = (mdc == 'f' ? fractional : mdc == 's' ? scientific : general);
  char buff[64];

  formatDouble(buff, NumberOf(buff), Arg, mode, precision, a == trueEnum ? True : False);

  return normalReturn((termPo) allocateString(processHeap(P), buff, uniStrLen(buff)));
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

ValueReturn s__flt_hash(enginePo P, termPo l) {
  return normalReturn(makeInteger(floatHash(C_FLOAT(l))));
}

ValueReturn s_cos(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),cos(Arg)));
}

ValueReturn s_sin(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),sin(Arg)));
}

ValueReturn s_tan(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),tan(Arg)));
}

ValueReturn s_acos(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),acos(Arg)));
}

ValueReturn s_asin(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),asin(Arg)));
}

ValueReturn s_atan(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),atan(Arg)));
}

ValueReturn s_floor(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),floor(Arg)));
}

ValueReturn s_ceil(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),ceil(Arg)));
}

ValueReturn s_trunc(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),trunc(Arg)));
}

ValueReturn s_integral(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(floor(Arg) == Arg ? trueEnum : falseEnum);
}

ValueReturn s__ln(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),log(Arg)));
}

ValueReturn s__lg10(enginePo P, termPo l) {
  double Arg = floatVal(l);
  return normalReturn(makeFloat(processHeap(P),log10(Arg)));
}

ValueReturn s__sqrt(enginePo P, termPo l) {
  double Arg = floatVal(l);
  if (Arg >= 0.0)
    return normalReturn(makeFloat(processHeap(P),sqrt(Arg)));
  else
    return abnormalReturn(eRANGE);
}

ValueReturn s_pi(enginePo P) {
  return normalReturn(makeFloat(processHeap(P),M_PI));
}
