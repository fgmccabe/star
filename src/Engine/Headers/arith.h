//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_ARITH_H
#define STAR_ARITH_H

#include "term.h"

// Integer structure
extern clssPo integerClass;

logical isInteger(termPo t);

integer integerVal(termPo o);

// Float structure

extern clssPo floatClass;

static inline logical isFloat(termPo p) {
  return hasClass(p, floatClass);
}

extern double floatVal(termPo o);

extern integer floatHash(double dx);

extern logical nearlyEqual(double dx1, double dx2, double eps);

#define MIN_NORMAL ((double)0x0010000000000000L)

extern void initArith();

#endif //STAR_ARITH_H
