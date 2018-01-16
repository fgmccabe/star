#ifndef _HEAP_H_
#define _HEAP_H_

#include "config.h"
#include "ooio.h"
#include "code.h"

typedef struct _heap_ *heapPo;

extern void initHeap(long heapSize);

extern heapPo currHeap;

typedef struct term_record **ptrPo, *termPo;      /* pointer to a structured value */

typedef struct class_record *clssPo;

extern termPo allocateStruct(heapPo H, clssPo class, integer size);

extern retCode reserveSpace(heapPo H, size_t amnt);

extern void markRoot(heapPo heap,ptrPo addr);

extern logical compare_and_swap(termPo cl, int64 expect, int64 repl);

#define PTRSZE (sizeof(void*))

#define ALIGNPTR(count, size) (((count+size-1)/size)*(size))
#define CellCount(size) (ALIGNPTR(size,PTRSZE)/PTRSZE)
#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((ptrI)ptr)+size-1)/size)*(size)==(ptrI)ptr)
#endif

#endif
