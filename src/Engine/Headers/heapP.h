#ifndef _HEAP_P_H_
#define _HEAP_P_H_

#include <turm.h>
#include "heap.h"
#include "lockvarP.h"

// Are we allocating from the lower or upper half?
typedef enum {
  lowerHalf, upperHalf
} AllocMode;

#ifndef MAX_ROOT
#define MAX_ROOT 128
#endif

typedef struct _heap_ {
  termPo start;
  termPo curr;
  termPo limit;
  termPo base;
  termPo split;
  termPo outerLimit;      /* The real */
  termPo old;
  AllocMode allocMode;
  LockRecord heapLock;
  ptrPo roots[MAX_ROOT];
  int topRoot;
  processPo owner;
} HeapRecord;

extern HeapRecord heap;

typedef struct _stack_frame_ *framePo;

extern retCode gcCollect(heapPo H, long amount);

typedef struct _gc_support_ {
  heapPo H;
  long oCnt;
  termPo oldBase;
  termPo oldLimit;
} GCSupport, *gcSupportPo;

extern void setupGCSupport(heapPo H, gcSupportPo G);

extern void validPtr(heapPo H, termPo t);
extern void inStackPtr(processPo P, ptrPo o);
extern void verifyHeap(heapPo H);

extern termPo markPtr(gcSupportPo G, ptrPo p);
extern termPo scanTerm(gcSupportPo G, termPo x);

static inline logical inHeap(heapPo P, const termPo x) {
  return (logical) (x >= P->base && x < P->curr);
}

extern void lockHeap(heapPo H);
extern void releaseHeapLock(heapPo H);

extern retCode heapSummary(ioPo out, heapPo H);

extern void dumpGcStats();

#endif
