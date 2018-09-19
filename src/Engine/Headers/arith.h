//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_ARITH_H
#define STAR_ARITH_H

#include "term.h"

// Integer structure
typedef struct integer_term *intPo;
extern clssPo integerClass;

extern intPo C_INT(termPo t);

static inline logical isInteger(termPo p) {
  return hasClass(p, integerClass);
}

extern const integer integerVal(termPo o);

extern integer integerHash(intPo ix);

// Float structure
typedef struct float_term *fltPo;

extern clssPo floatClass;

static inline logical isFloat(termPo p) {
  return hasClass(p, floatClass);
}

extern const double floatVal(termPo o);

extern integer floatHash(double dx);

extern logical nearlyEqual(double dx1, double dx2, double eps);

extern fltPo C_FLT(termPo t);

#define MIN_NORMAL ((double)0x0010000000000000L)

extern void initArith();

#endif //STAR_ARITH_H
