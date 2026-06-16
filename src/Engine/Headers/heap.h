#ifndef _HEAP_H_
#define _HEAP_H_

#include "config.h"
#include "ooio.h"
#include "term.h"
#include "labels.h"
#include "normal.h"


void initHeap(long heapSize);

extern integer numAllocated;
extern integer totalAllocated;

termPo allocateObject(int32 index, integer amnt);
normalPo allocateUnary(int32 index, termPo arg);
normalPo allocateBinary(int32 index, termPo left, termPo right);

normalPo allocateStruct(labelPo lbl);
retCode enoughRoom(labelPo lbl);
retCode reserveSpace(integer amnt);

int gcAddRoot(ptrPo addr);
void gcReleaseRoot(int mark);

#define PTRSZE (sizeof(void*))

#define ALIGNPTR(count, size) ((((count)+(size)-1)/(size))*(size))
#define CellCount(size) ((integer)(ALIGNPTR(size,PTRSZE)/PTRSZE))
#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)
#endif

#endif
