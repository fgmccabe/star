//
// Created by Francis McCabe on 3/22/20.
//

#ifndef STAR_TOPSORT_H
#define STAR_TOPSORT_H

#include "object.h"
#include "vector.h"

typedef vectorPo (*findRefs)(objectPo def, void *cl);
vectorPo topSort(vectorPo defs, findRefs ref, void *cl);

#endif //STAR_TOPSORT_H
