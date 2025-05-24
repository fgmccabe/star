//
// Created by Francis McCabe on 4/11/23.
//

#ifndef STAR_CLOSURE_H
#define STAR_CLOSURE_H

#include "term.h"
#include "labels.h"

typedef struct closure_record_ *closurePo;

extern clssPo closureClass;

static inline closurePo C_CLOSURE(termPo t) {
  assert(hasClass(t, closureClass));
  return (closurePo) t;
}

static inline logical isClosure(termPo t) {
  return hasClass(t, closureClass);
}

extern labelPo closureLabel(closurePo cl);
extern termPo closureFree(closurePo cl);

#endif //STAR_CLOSURE_H
