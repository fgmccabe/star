//
// Created by Francis McCabe on 4/11/23.
//

#ifndef STAR_CLOSURE_H
#define STAR_CLOSURE_H

#include "term.h"
#include "labels.h"

typedef struct closure_record_ *closurePo;

extern closurePo C_CLOSURE(termPo t);

extern logical isClosure(termPo t);

extern labelPo closureLabel(closurePo cl);
extern termPo closureFree(closurePo cl);

#endif //STAR_CLOSURE_H
