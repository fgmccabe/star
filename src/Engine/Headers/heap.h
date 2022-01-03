#ifndef _HEAP_H_
#define _HEAP_H_

#include "config.h"
#include "engineOptions.h"
#include "ooio.h"
#include "term.h"

typedef struct heap_ *heapPo;

extern void initHeap(long heapSize);

extern heapPo globalHeap;

#ifdef TRACEMEM
extern integer numAllocated;
extern integer totalAllocated;
#endif

extern termPo allocateObject(heapPo H, clssPo clss, integer amnt);

extern normalPo allocateStruct(heapPo H, labelPo lbl);
extern retCode enoughRoom(heapPo H,labelPo lbl);
extern retCode reserveSpace(heapPo H, integer amnt);

extern int gcAddRoot(heapPo H, ptrPo addr);
extern void gcReleaseRoot(heapPo H, int mark);

#define PTRSZE (sizeof(void*))

#define ALIGNPTR(count, size) ((((count)+(size)-1)/(size))*(size))
#define CellCount(size) ((integer)(ALIGNPTR(size,PTRSZE)/PTRSZE))
#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)
#endif

#endif
