//
// Created by Francis McCabe on 3/22/20.
//

#ifndef STAR_TOPSORT_H
#define STAR_TOPSORT_H

#include "lifo.h"

typedef objectPo (*findRefProc)(void * def, void *cl, integer ix);

lifoPo topSort(lifoPo defs, findRefProc findRef, void *cl);

#endif //STAR_TOPSORT_H
