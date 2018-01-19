//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_ARITH_H_H
#define CAFE_ARITH_H_H

#include "engine.h"
#include "termP.h"

// Integer structure
typedef struct int_struct *intPo;

extern intPo C_INT(termPo t);

extern clssPo integerClass;

extern termPo allocateInteger(heapPo H, int64);

extern termPo allocateLocalInteger(processPo P, int64 ix);

static inline logical isInteger(termPo p) {
  return hasClass(p, integerClass);
}

extern const int64 integerVal(termPo o);

extern integer integerHash(intPo ix);

// Float structure
typedef struct float_struct *fltPo;

extern clssPo floatClass;

extern termPo allocateFloat(heapPo H, double dx);

extern termPo allocateLocalFloat(processPo P, double dx);

static inline logical isFloat(termPo p) {
  return hasClass(p, floatClass);
}

extern const double floatVal(termPo o);

extern integer floatHash(fltPo ix);

#endif //CAFE_ARITH_H_H
