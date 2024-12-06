//
// Created by Francis McCabe on 12/5/24.
//

#ifndef STAR_SINGLE_H
#define STAR_SINGLE_H

#include "term.h"
#include "heap.h"
#include "closure.h"

typedef struct single_rec_ *singlePo;

extern clssPo singleClass;

singlePo C_SINGLE(termPo t);

singlePo singleVar(heapPo H);
termPo singleVal(singlePo v);
termPo setSingle(singlePo v, termPo e);
logical singleIsSet(singlePo single);
closurePo singleLam(singlePo single);

#endif //STAR_SINGLE_H
