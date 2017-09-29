#include "config.h"
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "heapP.h"
#include "Headers/codeP.h"

HeapRecord heap,oldHeap;
heapPo currHeap = NULL;

void initHeap(long heapSize)
{
  if(currHeap==NULL){
    heap.curr = heap.old = heap.base = heap.start = 
      (integer*)malloc(sizeof(integer)*heapSize); /* Allocate heap */
    heap.outerLimit = heap.base+heapSize;	/* The actual outer limit */
    heap.limit = heap.base+heapSize/2;
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

closurePo allocate(heapPo heap,methodPo mtd)
{
  int64 freeCount = mtd->freeCount;
  closurePo cl = (closurePo)heap->curr;
  heap->curr += freeCount+sizeof(ClosureRec)/sizeof(integer);
  cl->sig = (termPo)mtd;
  bzero(&cl->free,freeCount*sizeof(integer));
  return cl;
}
