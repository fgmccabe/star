#include "config.h"
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "heapP.h"
#include "Headers/codeP.h"

HeapRecord heap, oldHeap;
heapPo currHeap = NULL;

static ptrPo roots[1024];
static int rootTop = 0;

void initHeap(long heapSize) {
  if (currHeap == NULL) {
    heap.curr = heap.old = heap.base = heap.start =
      (integer *) malloc(sizeof(integer) * heapSize); /* Allocate heap */
    heap.outerLimit = heap.base + heapSize;  /* The actual outer limit */
    heap.limit = heap.base + heapSize / 2;
    heap.allocMode = lowerHalf;

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
  if ((integer *) ((byte *) currHeap->curr + amnt) < currHeap->limit)
    return Ok;
  else
    return Error;
}

termPo alloc(clssPo cls) {
  size_t amnt = allocAmnt(cls);

  if ((integer *) ((byte *) currHeap->curr + amnt) < currHeap->limit) {
    termPo t = (termPo) currHeap->curr;
    currHeap->curr = (integer *) ((byte *) currHeap->curr + amnt);
    t->clss = cls;
    return t;
  } else if (gc(amnt) == Ok) {
    termPo t = (termPo) currHeap->curr;
    currHeap->curr = (integer *) ((byte *) currHeap->curr + amnt);
    t->clss = cls;
    return t;
  } else
    return Null;
}

