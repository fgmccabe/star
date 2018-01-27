//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_ARITH_H
#define CAFE_ARITH_H

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

extern integer floatHash(fltPo ix);

extern fltPo C_FLT(termPo t);

#endif //CAFE_ARITH_H
