#ifndef _HEAP_P_H_
#define _HEAP_P_H_

#include "heap.h"

// Are we allocating from the lower or upper half?
typedef enum {
  lowerHalf, upperHalf
} AllocMode;

typedef struct _heap_ {
  uint64 *start;
  uint64 *curr;
  uint64 *limit;
  uint64 *outerLimit;			/* The real */
  uint64 *base;
  uint64 *old;
  AllocMode allocMode;
} HeapRecord;

extern HeapRecord heap, oldHeap;

typedef struct _closure_ *envPo;
typedef struct _stack_frame_ *framePo;

// This is pretty machine specific to the x86-64

extern void collect(framePo stack,void *site,int amnt);

// Change these if HeapRecord is changed
#define H_CURR_OFFSET (sizeof(void*))
#define H_LIMIT_OFFSET (sizeof(void*))

#define AddressOf(tp,field) ((long)(void*)(&((tp*)0)->field))

#endif
