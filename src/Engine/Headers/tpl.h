//
// Created by Francis McCabe on 6/20/17.
//

#ifndef CAFE_TPL_H
#define CAFE_TPL_H

#include "engine.h"
#include "term.h"

extern normalPo allocatePair(heapPo H, termPo lhs, termPo rhs);

extern normalPo allocateTpl(heapPo H, integer count);

#endif //CAFE_TPL_H
