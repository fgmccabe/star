//
// Created by Francis McCabe on 11/11/23.
//

#ifndef STAR_NORMALP_H
#define STAR_NORMALP_H

#include "normal.h"
#include "labels.h"

typedef struct normal_term {
  TermHead head;
  termPo args[ZEROARRAYSIZE];
} NormalTerm;

static inline termPo nthElem(normalPo term, int32 ix) {
  return term->args[ix];
}

static void inline setNth(normalPo term, int32 ix, termPo arg) {
  term->args[ix] = arg;
}

#define NormalCellCount(arity) CellCount(sizeof(NormalTerm)+(arity)*sizeof(termPo))

#endif //STAR_NORMALP_H
