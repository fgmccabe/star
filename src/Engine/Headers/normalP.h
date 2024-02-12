//
// Created by Francis McCabe on 11/11/23.
//

#ifndef STAR_NORMALP_H
#define STAR_NORMALP_H

#include "normal.h"
#include "labels.h"

typedef struct normal_term {
  labelPo lbl;                // Overlays clss - because it is the term's class
  termPo args[ZEROARRAYSIZE];
} NormalTerm;


static inline termPo nthElem(normalPo term, integer ix) {
  return term->args[ix];
}

#define NormalCellCount(arity) CellCount(sizeof(Normal)+(arity)*sizeof(termPo))

#endif //STAR_NORMALP_H
