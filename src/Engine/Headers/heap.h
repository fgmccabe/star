#ifndef _HEAP_H_
#define _HEAP_H_

#include "config.h"
#include "ooio.h"
#include "term.h"

typedef struct _heap_ *heapPo;

extern void initHeap(long heapSize);

extern heapPo currHeap;

extern termPo allocateObject(clssPo clss, size_t amnt);

extern normalPo allocateStruct(labelPo lbl);

extern retCode reserveSpace(size_t amnt);

extern void markRoot(ptrPo addr);

extern int gcAddRoot(ptrPo addr);
extern void gcReleaseRoot(int mark);
extern retCode gc(size_t amnt);

extern logical compare_and_swap(normalPo cl, int64 expect, int64 repl);

#define PTRSZE (sizeof(void*))

#define ALIGNPTR(count, size) ((((count)+(size)-1)/(size))*(size))
#define CellCount(size) (ALIGNPTR(size,PTRSZE)/PTRSZE)
#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)
#endif

#endif
