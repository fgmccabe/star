//
// Created by Francis McCabe on 6/18/17.
//

#ifndef STAR_ARITH_H
#define STAR_ARITH_H

#include "term.h"
#include <assert.h>

// Integer structure
static inline logical isInteger(termPo t) {
  return pointerTag(t) == intTg;
}

static inline integer integerVal(termPo o) {
#ifdef TRACEEXEC
  assert(isInteger(o));
#endif
  return ptrPayload(o);
}

static inline termPo makeInteger(integer x) {
  return ((termPo) ((x << 2ul) | intTg));
}

// Float structure
static inline logical isFloat(termPo p) {
  return pointerTag(p) == fltTg;
}

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

static inline double floatVal(termPo o) {
  assert (isFloat(o));

  return bits_float(ptrPayload(o));
}

static inline termPo makeFloat(double dx) {
  uint64 bits = float_bits(dx);
  uint64 mask = (((uint64)-1)<<2u);
  return ((termPo) ((bits&mask) | fltTg));
}

static inline integer floatHash(double dx) {
  return hash61(((uinteger) dx) >> 2ul);
}

extern logical nearlyEqual(double dx1, double dx2, double eps);

#define MIN_NORMAL ((double)0x0010000000000000L)

extern void initArith();

#endif //STAR_ARITH_H
