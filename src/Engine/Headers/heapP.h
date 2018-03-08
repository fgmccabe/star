#ifndef _HEAP_P_H_
#define _HEAP_P_H_

#include "heap.h"
#include "lockvarP.h"

// Are we allocating from the lower or upper half?
typedef enum {
  lowerHalf, upperHalf
} AllocMode;

typedef struct _heap_ {
  ptrPo start;
  ptrPo curr;
  ptrPo limit;
  ptrPo outerLimit;      /* The real */
  ptrPo base;
  ptrPo old;
  AllocMode allocMode;
  LockRecord heapLock;
} HeapRecord;

extern HeapRecord heap, oldHeap;

typedef struct _closure_ *envPo;
typedef struct _stack_frame_ *framePo;

extern void collect(framePo stack, void *site, int amnt);

// Change these if HeapRecord is changed
#define H_CURR_OFFSET (sizeof(void*))
#define H_LIMIT_OFFSET (sizeof(void*))

#define AddressOf(tp, field) ((long)(void*)(&((tp*)0)->field))

extern void lockHeap(heapPo H);
extern void releaseHeapLock(heapPo H);

#endif
