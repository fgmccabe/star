#ifndef _HEAP_H_
#define _HEAP_H_

#include "config.h"
#include "ooio.h"
#include "term.h"

typedef struct _heap_ *heapPo;

extern void initHeap(long heapSize);

extern heapPo currHeap;

extern termPo allocateEnum(heapPo H, char *nm, int64 arity);

extern termPo allocateObject(heapPo H, termPo cons);

extern normalPo allocateStruct(heapPo H, labelPo lbl);

extern retCode reserveSpace(heapPo H, size_t amnt);

extern void markRoot(heapPo heap,ptrPo addr);

extern logical compare_and_swap(normalPo cl, int64 expect, int64 repl);

#define PTRSZE (sizeof(void*))

#define ALIGNPTR(count, size) (((count+size-1)/size)*(size))
#define CellCount(size) (ALIGNPTR(size,PTRSZE)/PTRSZE)
#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)
#endif

#endif
