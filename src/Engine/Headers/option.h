//
// Created by Francis McCabe on 6/29/20.
//

#ifndef STAR_OPTION_H
#define STAR_OPTION_H

#include "term.h"

normalPo wrapSome(heapPo H, termPo lhs);
termPo unwrapSome(normalPo p);

extern labelPo noneEnum;
extern labelPo someCons;

logical isSome(termPo t);
logical isNone(termPo t);

#endif //STAR_OPTION_H
