#include "config.h"
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

#include "heapP.h"
#include "Headers/codeP.h"

HeapRecord heap, oldHeap;
heapPo currHeap = NULL;

static ptrPo roots[1024];
static int rootTop = 0;

static void initHeapLock(heapPo h);

void initHeap(long heapSize) {
  if (currHeap == NULL) {
    heap.curr = heap.old = heap.base = heap.start =
      (ptrPo) malloc(sizeof(ptrPo) * heapSize); /* Allocate heap */
    heap.outerLimit = heap.base + heapSize;  /* The actual outer limit */
    heap.limit = heap.base + heapSize / 2;
    heap.allocMode = lowerHalf;
    initHeapLock(&heap);
    currHeap = &heap;

#ifdef MEMTRACE
    if(traceMemory){
      outMsg(logFile,"establish heap of %d words total\n",initHeapSize);
      outMsg(logFile,"lower half at 0x%x, %d words\n",heap.start,heap.limit-heap.base);
    }
#endif
  }
}

int gcAddRoot(ptrPo addr) {
  assert(rootTop < NumberOf(roots));

  roots[rootTop] = addr;
  return rootTop++;
}

void gcReleaseRoot(int mark) {
  assert(mark >= 0 && mark < NumberOf(roots));
  rootTop = mark;
}

retCode reserveSpace(size_t amnt) {
  if (currHeap->curr + amnt < currHeap->limit)
    return Ok;
  else
    return Error;
}

termPo allocateObject(heapPo H, clssPo clss, size_t amnt) {
  if (currHeap->curr + amnt < currHeap->limit) {
    termPo t = (termPo) currHeap->curr;
    H->curr = H->curr + amnt;
    t->clss = clss;
    return t;
  } else if (gc(amnt) == Ok) {
    termPo t = (termPo) currHeap->curr;
    currHeap->curr = currHeap->curr + amnt;
    t->clss = clss;
    return t;
  } else
    return Null;
}

normalPo allocateStruct(heapPo H, labelPo lbl) {
  return (normalPo) allocateObject(H, (clssPo) lbl, CellCount(sizeof(Normal) + sizeof(ptrPo) * lbl->arity));
}

void initHeapLock(heapPo heap) {
  initLock(&heap->heapLock);
}

extern void lockHeap(heapPo H) {
  acquireLock(&H->heapLock, 0.0);
}

extern void releaseHeapLock(heapPo H) {
  releaseLock(&H->heapLock);

}
