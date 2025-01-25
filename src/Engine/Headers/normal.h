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

int32 termArity(normalPo term);

termPo nthArg(normalPo term, int32 ix);

void setArg(normalPo term, integer ix, termPo arg);

typedef retCode (*normalProc)(termPo term,void *cl);

retCode walkNormal(termPo t,normalProc proc,void *cl);

#endif //STAR_NORMAL_H
