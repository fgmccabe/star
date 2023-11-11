//
// Created by Francis McCabe on 11/11/23.
//

#ifndef STAR_NORMAL_H
#define STAR_NORMAL_H

#include "term.h"
#include "labels.h"

typedef struct normal_term *normalPo;

normalPo C_NORMAL(termPo t);

logical isNormalPo(termPo t);

labelPo termLbl(normalPo t);

integer termSize(normalPo t);

integer termArity(normalPo term);

termPo nthArg(normalPo term, integer ix);

void setArg(normalPo term, integer ix, termPo arg);

#endif //STAR_NORMAL_H
