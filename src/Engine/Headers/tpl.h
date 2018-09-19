//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_TPL_H
#define STAR_TPL_H

#include "engine.h"
#include "term.h"

extern normalPo allocatePair(heapPo H, termPo lhs, termPo rhs);

extern normalPo allocateTpl(heapPo H, integer count);

#endif //STAR_TPL_H
