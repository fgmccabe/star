//
// Created by Francis McCabe on 6/20/17.
//

#ifndef CAFE_TPL_H
#define CAFE_TPL_H


#include "engine.h"
#include "term.h"

extern labelPo pairLbl,consLbl;

extern termPo allocatePair(heapPo H,termPo lhs,termPo rhs);

extern termPo allocateTpl(heapPo H,integer count);

extern termPo allocateCons(heapPo H,termPo lhs,termPo rhs);

#endif //CAFE_TPL_H
