//
// Created by Francis McCabe on 7/19/20.
//

#ifndef STAR_ARRAY_H
#define STAR_ARRAY_H

#include "utils.h"

typedef struct array_ *arrayPo;
typedef retCode (*arrayElProc)(void *entry, int32 ix, void *cl);
typedef void * (*arrayDataCopy)(void *src, int32 size);

int32 arrayCount(arrayPo ar);

arrayPo allocArray(int elSize, int32 initial, logical growable);

arrayPo fixedCopy(arrayPo src, arrayDataCopy copier, void (*release)(arrayPo ar));
retCode appendEntry(arrayPo ar,void *el);
retCode insertEntry(arrayPo ar, int32 ix, void *el);
void *newEntry(arrayPo ar);
void *nthEntry(arrayPo ar, int32 ix);
retCode dropEntry(arrayPo ar, int32 ix);
arrayPo eraseArray(arrayPo ar, arrayElProc eraser, void *cl);
retCode processArray(arrayPo ar, arrayElProc proc, void *cl);

typedef comparison (*compareEls)(arrayPo ar, int32 ix, int32 iy, void *cl);
retCode sortArray(arrayPo ar, compareEls compare, void *cl);

retCode copyOutData(arrayPo ar, void *buffer, int32 buffSize);

#endif //STAR_ARRAY_H
