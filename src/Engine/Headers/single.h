//
// Created by Francis McCabe on 9/10/21.
//

#ifndef STAR_SINGLE_H
#define STAR_SINGLE_H

#include "stack.h"
typedef struct single_record *singlePo;

extern clssPo singleClass;

extern singlePo C_SINGLE(termPo t);

singlePo makeSingle(heapPo H);

logical singleHasValue(singlePo t);

retCode setSingle(singlePo ft, termPo val);
termPo getSingle(singlePo f);
#endif //STAR_SINGLE_H
