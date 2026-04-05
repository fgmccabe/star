#ifndef _HEAP_H_
#define _HEAP_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "labels.h"
#include "normal.h"

typedef struct heap_ *heapPo;

void initHeap(long heapSize);

extern heapPo globalHeap;

extern integer numAllocated;
extern integer totalAllocated;

termPo allocateObject(heapPo H, int32 index, integer amnt);
normalPo allocateUnary(heapPo h, int32 index, termPo arg);
normalPo allocateBinary(heapPo h, int32 index, termPo left, termPo right);

normalPo allocateStruct(heapPo H, labelPo lbl);
retCode enoughRoom(heapPo H, labelPo lbl);
retCode reserveSpace(heapPo H, integer amnt);

int gcAddRoot(heapPo H, ptrPo addr);
void gcReleaseRoot(heapPo H, int mark);

#define PTRSZE (sizeof(void*))

#define ALIGNPTR(count, size) ((((count)+(size)-1)/(size))*(size))
#define CellCount(size) ((integer)(ALIGNPTR(size,PTRSZE)/PTRSZE))
#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)
#endif

#endif
