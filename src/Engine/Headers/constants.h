//
// Created by Francis McCabe on 3/26/25.
//

#ifndef STAR_CONSTANTS_H
#define STAR_CONSTANTS_H

#include "heap.h"
#include "term.h"

int32 constantLiteral(termPo t);
int32 defineConstantLiteral(termPo t);
termPo getConstant(int32 key);

#endif //STAR_CONSTANTS_H
