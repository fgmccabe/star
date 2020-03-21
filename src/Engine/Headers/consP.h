//
// Created by Francis McCabe on 3/20/20.
//

#ifndef STAR_CONSP_H
#define STAR_CONSP_H

#include "cons.h"
#include "heap.h"
#include "termP.h"

extern normalPo allocateCons(heapPo H, termPo lhs, termPo rhs);

void initCons();

#endif //STAR_CONSP_H
