//
// Created by Francis McCabe on 7/19/20.
//

#ifndef STAR_ARRAY_H
#define STAR_ARRAY_H

#include "integer.h"
#include "retcode.h"

typedef struct array_ *arrayPo;
typedef retCode (*arrayElProc)(void *entry, integer ix, void *cl);

integer arrayCount(arrayPo ar);

arrayPo allocArray(int elSize, integer initial, logical growable);
arrayPo fixedArray(int elSize, integer initial, void *data);
retCode appendEntry(arrayPo ar,void *el);
void *nthEntry(arrayPo ar,integer ix);
retCode dropEntry(arrayPo ar,integer ix);
arrayPo eraseArray(arrayPo ar);
retCode processArrayElements(arrayPo ar, arrayElProc proc, void *cl);

#endif //STAR_ARRAY_H
