//
// Created by Francis McCabe on 4/11/23.
//

#ifndef STAR_CLOSURE_H
#define STAR_CLOSURE_H

#include "term.h"
#include "labels.h"

typedef struct closure_record_ *closurePo;

extern builtinClassPo closureClass;
extern int32 closureIndex;

static inline closurePo C_CLOSURE(termPo t) {
  assert(checkIndex(t, closureIndex));
  return (closurePo) t;
}

static inline logical isClosure(termPo t) {
  return hasIndex(t, closureIndex);
}

extern labelPo closureLabel(closurePo cl);
extern termPo closureFree(closurePo cl);

#endif //STAR_CLOSURE_H
