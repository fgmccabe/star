//
// Created by Francis McCabe on 11/11/23.
//

#ifndef STAR_NORMAL_H
#define STAR_NORMAL_H

#include "term.h"
#include "labels.h"

typedef struct normal_term* normalPo;

logical isNormalPo(termPo t);

labelPo termLbl(normalPo t);

integer termSize(normalPo t);

int32 termArity(normalPo term);

#ifndef NDEBUG
static inline normalPo C_NORMAL(termPo term) {
  assert(isNormalPo(term));
  return (normalPo)term;
}
#else
#define C_NORMAL(t) ((normalPo) (t))
#endif

termPo nthArg(normalPo term, int32 ix);

void setArg(normalPo term, integer ix, termPo arg);

typedef retCode (*normalProc)(termPo term, void* cl);

retCode walkNormal(termPo t, normalProc proc, void* cl);
#endif //STAR_NORMAL_H
