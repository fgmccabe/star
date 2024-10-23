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
retCode reserveRoom(arrayPo ar, integer count);
retCode appendEntry(arrayPo ar,void *el);
void *nthEntry(arrayPo ar,integer ix);
void setNth(arrayPo ar, integer ix, void *el);
retCode dropEntry(arrayPo ar,integer ix);
arrayPo eraseArray(arrayPo ar, arrayElProc eraser, void *cl);
retCode processArrayElements(arrayPo ar, arrayElProc proc, void *cl);

typedef comparison (*compareEls)(arrayPo ar, integer ix, integer iy, void *cl);
retCode sortArray(arrayPo ar, compareEls compare, void *cl);

retCode copyOutData(arrayPo ar, void *buffer, integer buffSize);

#endif //STAR_ARRAY_H
