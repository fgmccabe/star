//
// Created by Francis McCabe on 2/19/22.
//

#ifndef STAR_IDEAL_H
#define STAR_IDEAL_H

#include "heap.h"
#include "termP.h"

extern termPo hNilEnum;

retCode dispIdeal(ioPo out, termPo t, integer precision, integer depth, logical alt);

logical isIdealEmpty(termPo t);
logical isIdealTree(termPo t);
void initIdeal();

#endif //STAR_IDEAL_H
