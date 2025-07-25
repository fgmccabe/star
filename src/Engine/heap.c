#include "config.h"
#include <unistd.h>
#include <stdlib.h>
#include <threds.h>

long initHeapSize = 4 * 1024 * 1024;   /* How much memory to give the heap */
long maxHeapSize = 1024 * 1024 * 1024; // Maximum heap size 1G cells

tracingLevel traceMemory = noTracing;      /* memory tracing */
logical traceAllocs = False;      // trace allocations
logical validateMemory = False;   // Validate heap after every allocation

HeapRecord heap;
heapPo globalHeap = Null;
heapPo currentHeap = Null;

integer numAllocated = 0;
integer totalAllocated = 0;

void initHeap(long heapSize) {
  if (globalHeap == NULL) {
    heap.curr = heap.old = heap.base = heap.start =
      (termPo) malloc(sizeof(ptrPo) * heapSize); /* Allocate heap */
    heap.outerLimit = heap.base + heapSize;  /* The actual outer limit */
    heap.limit = heap.split = heap.base + heapSize / 2;
    heap.allocMode = lowerHalf;
    globalHeap = currentHeap = &heap;

#ifdef TRACEMEM
    if (traceMemory>noTracing) {
      outMsg(logFile, "establish heap of %ld words total\n", initHeapSize);
      outMsg(logFile, "lower half at 0x%x, %ld words\n", heap.start, heap.limit - heap.base);
    }
#endif
  }
}

retCode heapSummary(ioPo out, heapPo H) {
  return outMsg(out, ", H:0x%x(%s)%5.2g%%", H->curr, H->allocMode == lowerHalf ? "lower" : "upper",
                (double) (H->curr - H->start) * 100.0 / (double) (H->limit - H->start));
}

int gcAddRoot(heapPo H, ptrPo addr) {
  assert(H->topRoot < NumberOf(H->roots));

  H->roots[H->topRoot] = addr;
  return H->topRoot++;
}

void gcReleaseRoot(heapPo H, int mark) {
  assert(mark >= 0 && mark < NumberOf(H->roots) && mark <= H->topRoot);
  H->topRoot = mark;
}

retCode reserveSpace(heapPo H, integer amnt) {
  if ((((ptrPo) H->curr) + amnt) < ((ptrPo) (H->limit)))
    return Ok;
  else
    return Error;
}

termPo allocateObject(heapPo h, clssPo clss, integer amnt) {
  if ((((ptrPo) h->curr) + amnt) >= ((ptrPo) (h->limit))) {
    if (gcCollect(h, amnt) != Ok) {
      logMsg(logFile, "Could not allocate %d cells on heap", amnt);
      star_exit(oomCode);
      return Null;
    }
  }

  termPo t = h->curr;
  h->curr = h->curr + amnt;
  t->clss = clss;
#ifdef TRACEMEM
  if (traceAllocs) {
    numAllocated++;
    totalAllocated += amnt;
  }
#endif
  return t;
}

normalPo allocateStruct(heapPo H, labelPo lbl) {
  return (normalPo) allocateObject(H, (clssPo) lbl, NormalCellCount(lblArity(lbl)));
}

retCode enoughRoom(heapPo H, labelPo lbl) {
  return reserveSpace(H, NormalCellCount(lblArity(lbl)));
}

void validPtr(heapPo H, termPo t) {
  if (isPointer(t))
    assert((t >= H->start && t < H->limit) || !(t >= H->base && t < H->outerLimit));
}

static retCode verifyScanHelper(ptrPo arg, void *c) {
  heapPo H = (heapPo) c;
  validPtr(H, *arg);
  return Ok;
}

void verifyHeap(heapPo H) {
  for (termPo t = H->start; t < H->curr;) {
    clssPo clss = classOf(t);
    if (isSpecialClass(clss)) {
      specialClassPo sClass = (specialClassPo) clss;
      t = sClass->scanFun(sClass, verifyScanHelper, H, t);
    } else {
      normalPo trm = C_NORMAL(t);
      labelPo lbl = C_LBL((termPo) clss);
      for (int32 ix = 0; ix < lblArity(lbl); ix++) {
        validPtr(H, trm->args[ix]);
      }
      t = t + termSize(trm);
    }
  }
}
