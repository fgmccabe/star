#include "config.h"
#include <ooio.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>

#include "heapP.h"
#include "codeP.h"

HeapRecord heap,oldHeap;
heapPo currHeap = NULL;

void initHeap(long heapSize)
{
  if(currHeap==NULL){
    heap.curr = heap.old = heap.base = heap.start = 
      (uint64*)malloc(sizeof(uint64)*heapSize); /* Allocate heap */
    heap.outerLimit = heap.base+heapSize;	/* The actual outer limit */
    heap.limit = heap.base+heapSize/2;
    heap.allocMode = lowerHalf;

    currHeap = &heap;

#ifdef MEMTRACE
    if(traceMemory){
      outMsg(logFile,"establish heap of %d words total\n",heapSize);
      outMsg(logFile,"lower half at 0x%x, %d words\n",heap.start,heap.limit-heap.base);
    }
#endif
  }
}

closurePo allocate(heapPo heap,methodPo mtd)
{
  int32 freeCount = mtd->freeCount;
  closurePo cl = (closurePo)heap->curr;
  heap->curr += freeCount+sizeof(ClosureRec)/sizeof(uint64);
  cl->code = mtd;
  bzero(&cl->free,freeCount*sizeof(uint64));
  return cl;
}
