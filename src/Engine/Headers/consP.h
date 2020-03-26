//
// Created by Francis McCabe on 3/20/20.
//

#ifndef STAR_CONSP_H
#define STAR_CONSP_H

#include "cons.h"
#include "heap.h"
#include "termP.h"

normalPo allocateCons(heapPo H, termPo lhs, termPo rhs);

void updateConsHead(termPo cns,termPo h);
void updateConsTail(termPo cns,termPo t);
void initCons();

#endif //STAR_CONSP_H
