//
// Created by Francis McCabe on 4/21/25.
//

#ifndef STAR_RESULT_H
#define STAR_RESULT_H

#include "term.h"
#include "heap.h"
#include "labelsP.h"

normalPo wrapResult(heapPo H, termPo lhs);
normalPo wrapAbnormal(heapPo H, termPo lhs);

logical isResult(termPo t);
logical isAbnormal(termPo t);

termPo resultValue(termPo t);
termPo abnormalValue(termPo t);

#endif //STAR_RESULT_H
