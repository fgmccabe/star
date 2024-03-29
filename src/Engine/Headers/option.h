//
// Created by Francis McCabe on 6/29/20.
//

#ifndef STAR_OPTION_H
#define STAR_OPTION_H

#include "term.h"
#include "heap.h"
#include "labelsP.h"

normalPo wrapSome(heapPo H, termPo lhs);
termPo unwrapSome(normalPo p);

extern termPo noneEnum;
extern labelPo someCons;

logical isSome(termPo t);
logical isNone(termPo t);

#endif //STAR_OPTION_H
