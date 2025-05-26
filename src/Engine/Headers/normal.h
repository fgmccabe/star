//
// Created by Francis McCabe on 11/11/23.
//

#ifndef STAR_NORMAL_H
#define STAR_NORMAL_H

#include "term.h"
#include "labels.h"

typedef struct normal_term *normalPo;

#ifndef NDEBUG
#define C_NORMAL(c) (assert(isNormalPo(c)),  (normalPo) (c))
#else
#define C_NORMAL(t) ((normalPo) (t))
#endif

logical isNormalPo(termPo t);

labelPo termLbl(normalPo t);

integer termSize(normalPo t);

int32 termArity(normalPo term);

termPo nthArg(normalPo term, int32 ix);

void setArg(normalPo term, integer ix, termPo arg);

typedef retCode (*normalProc)(termPo term, void *cl);

retCode walkNormal(termPo t, normalProc proc, void *cl);

extern labelPo trueLbl;
extern labelPo falseLbl;

#ifndef NDEBUG
static inline logical isTrueEnum(termPo t) {
  if (isNormalPo(t)) {
    normalPo n = C_NORMAL(t);
    return termLbl(n) == trueLbl;
  } else
    return False;
}
#else
static inline logical isTrueEnum(termPo t){
  clssPo cls = t->clss;
  return (labelPo)cls == trueLbl;
}
#endif
#endif //STAR_NORMAL_H
