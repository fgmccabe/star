#include "config.h"
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

#include "heapP.h"
#include "Headers/codeP.h"

HeapRecord heap, oldHeap;
heapPo currHeap = NULL;

static void initHeapLck(heapPo heap);

void initHeap(long heapSize) {
  if (currHeap == NULL) {
    heap.curr = heap.old = heap.base = heap.start =
      (termPo) malloc(sizeof(ptrPo) * heapSize); /* Allocate heap */
    heap.outerLimit = heap.base + heapSize;  /* The actual outer limit */
    heap.limit = heap.base + heapSize / 2;
    heap.allocMode = lowerHalf;
    initHeapLck(&heap);
    currHeap = &heap;

#ifdef TRACEMEM
    if (traceMemory) {
      outMsg(logFile, "establish heap of %d words total\n", initHeapSize);
      outMsg(logFile, "lower half at 0x%x, %d words\n", heap.start, heap.limit - heap.base);
    }
#endif
  }
}

int gcAddRoot(heapPo H, ptrPo addr) {
  assert(H->topRoot < NumberOf(H->roots));

  H->roots[H->topRoot] = addr;
  return H->topRoot++;
}

void gcReleaseRoot(heapPo H, int mark) {
  assert(mark >= 0 && mark < NumberOf(H->roots));
  H->topRoot = mark;
}

retCode reserveSpace(size_t amnt) {
  if ((((ptrPo) currHeap->curr) + amnt) < ((ptrPo) (currHeap->limit)))
    return Ok;
  else
    return Error;
}

termPo allocateObject(heapPo H, clssPo clss, size_t amnt) {
  if ((((ptrPo) currHeap->curr) + amnt) < ((ptrPo) (currHeap->limit))) {
    termPo t = currHeap->curr;
    H->curr = H->curr + amnt;
    t->clss = clss;
    return t;
  } else if (gcCollect(H, amnt) == Ok) {
    termPo t = currHeap->curr;
    currHeap->curr = currHeap->curr + amnt;
    t->clss = clss;
    return t;
  } else
    return Null;
}

normalPo allocateStruct(heapPo H, labelPo lbl) {
  return (normalPo) allocateObject(H, (clssPo) lbl, CellCount(sizeof(Normal) + sizeof(ptrPo) * lbl->arity));
}

void initHeapLck(heapPo heap) {
  initLock(&heap->heapLock);
}

extern void lockHeap(heapPo H) {
  acquireLock(&H->heapLock, 0.0);
}

extern void releaseHeapLock(heapPo H) {
  releaseLock(&H->heapLock);
}

void validPtr(heapPo H, termPo t) {
  assert((t >= H->start && t < H->limit) || !(t >= H->base && t < H->outerLimit));
}

static retCode verifyScanHelper(ptrPo arg, void *c) {
  heapPo H = (heapPo) c;
  validPtr(H, *arg);
  return Ok;
}

void verifyHeap(heapPo H) {
  for (termPo t = H->start; t < H->limit;) {
    clssPo clss = classOf(t);
    if (isSpecialClass(clss)) {
      specialClassPo sClass = (specialClassPo) clss;
      t = sClass->scanFun(sClass, verifyScanHelper, H, t);
    } else {
      normalPo trm = C_TERM(t);
      labelPo lbl = C_LBL((termPo) clss);
      for (integer ix = 0; ix < lbl->arity; ix++) {
        validPtr(H, trm->args[ix]);
      }
      t = t + termSize(trm);
    }
  }
}
