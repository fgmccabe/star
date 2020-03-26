//
// Created by Francis McCabe on 3/4/18.
//

#ifndef STAR_ARRAY_H
#define STAR_ARRAY_H

#include "term.h"
#include "heap.h"

typedef struct _list_slice_ *arrayPo;

extern clssPo listClass;

extern arrayPo C_ARRAY(termPo t);

typedef enum{
  safeAlloc,
  fastAlloc
} allocSafety;

extern arrayPo allocateArray(heapPo H, integer length);

extern arrayPo createArray(heapPo H, integer capacity);

extern termPo sliceArray(heapPo H, arrayPo list, integer from, integer to);

extern arrayPo spliceArray(heapPo H, arrayPo list, integer from, integer to, arrayPo rep);

extern arrayPo appendToArray(heapPo H, arrayPo list, termPo el);

extern arrayPo concatArray(heapPo H, arrayPo l1, arrayPo l2);

extern arrayPo flattenArray(heapPo H, arrayPo l);

extern arrayPo reverseArray(heapPo H, arrayPo l1);

extern arrayPo insertArrayEl(heapPo H, arrayPo list, integer px, termPo vl);

extern arrayPo replaceArrayEl(heapPo H, arrayPo list, integer px, termPo vl);

extern arrayPo removeArrayEl(heapPo H, arrayPo list, integer px);

// Use with caution!

extern arrayPo prependToArray(heapPo H, arrayPo list, termPo el);

typedef retCode (*arrayProc)(termPo el, integer ix, void *cl);

extern retCode processArray(arrayPo list, arrayProc p, void *cl);

extern integer arraySize(arrayPo list);
extern termPo nthEl(arrayPo list, integer ix);
extern void setNthEl(arrayPo list, integer ix, termPo el);

#endif //STAR_ARRAY_H
