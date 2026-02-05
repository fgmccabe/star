//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_ARITH_H
#define STAR_ARITH_H

#include "term.h"
#include "heap.h"
#include <assert.h>

// Integer structure
static inline logical isInteger(termPo t) {
  return pointerTag(t) == intTg;
}

static inline integer integerVal(termPo o) {
  assert(isInteger(o));
  return ptrPayload(o);
}

static inline termPo makeInteger(integer x) {
  return ((termPo) ((x << 2ul) | intTg));
}

typedef struct float_record_ *floatPo;

floatPo C_FLOAT(termPo t);

// Float structure
logical isFloat(termPo p);

static inline uinteger float_bits(double d) {
  union {
    uinteger ix;
    double dx;
  } U;
  U.dx = d;
  return U.ix;
}

static inline double bits_float(uinteger i) {
  union {
    uinteger ix;
    double dx;
  } U;
  U.ix = i;
  return U.dx;
}

termPo makeFloat(heapPo H, double dx);

double floatVal(termPo flt);

integer floatHash(floatPo f);

#define MIN_NORMAL ((double)0x0010000000000000L)

extern void initArith();

#endif //STAR_ARITH_H
