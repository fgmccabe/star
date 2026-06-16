#include "config.h"
#include <unistd.h>
#include <stdlib.h>
#include <threds.h>

#include "abort.h"
#include "normalP.h"

long initHeapSize = 4 * 1024 * 1024;   /* How much memory to give the heap */
long maxHeapSize = 1024 * 1024 * 1024; // Maximum heap size 1G cells

tracingLevel traceMemory = noTracing; /* memory tracing */
logical traceAllocs = False;          // trace allocations
logical validateMemory = False;       // Validate heap after every allocation

HeapRecord heap = {.base = Null};

integer numAllocated = 0;
integer totalAllocated = 0;
integer allocationHeaps[64];

cardMap masks[CARDWIDTH];

void initHeap(long heapSize) {
  if (heap.base == NULL) {
    for (int32 ix = 0; ix < CARDWIDTH; ix++)
      masks[ix] = 1 << ix; /* compute 2**i for i=0 to i=63 */

    if (setupHeap(&heap, heapSize) != Ok) {
      outMsg(logFile, "Unable to create heap of %ld cells", heapSize);
      star_exit(oomCode);
    }

#ifdef TRACEMEM
    if (traceMemory > noTracing) {
      outMsg(logFile, "establish heap of %ld words total\n", initHeapSize);
      outMsg(logFile, "lower half at 0x%x, %ld words\n", heap.start, heap.limit - heap.base);
      for (int ix = 0; ix < NumberOf(allocationHeaps); ix++)
        allocationHeaps[ix] = 0;
    }
#endif
  }
}

retCode heapSummary(ioPo out, heapPo H) {
  return outMsg(out, ", H:0x%x(%s)%5.2g%%", heap.curr, heap.allocMode == lowerHalf ? "lower" : "upper",
                (double)(heap.curr - heap.start) * 100.0 / (double)(heap.limit - heap.start));
}

retCode setupHeap(heapPo heap, int64 cellCount) {
  heap->curr = heap->old = heap->base = heap->start =
    (termPo)malloc(sizeof(ptrPo) * cellCount); /* Allocate heap */

  if (heap->curr == Null)
    return Space;
  else {
    heap->outerLimit = heap->base + cellCount; /* The actual outer limit */
    heap->limit = heap->split = heap->base + cellCount / 2;
    heap->allocMode = lowerHalf;

    heap->oldLimit = heap->start; // Nothing in the old generation at start.
    heap->ncards = (cellCount + CARDWIDTH - 1) / CARDWIDTH;
    heap->cards = (cardMap*)malloc(heap->ncards * sizeof(cardMap));

    for (int32 ix = 0; ix < heap->ncards; ix++)
      heap->cards[ix] = 0; /* clear the card table */
    return Ok;
  }
}

int gcAddRoot(ptrPo addr) {
  assert(heap.topRoot < NumberOf(heap.roots));

  heap.roots[heap.topRoot] = addr;
  return heap.topRoot++;
}

void gcReleaseRoot(int mark) {
  assert(mark >= 0 && mark < NumberOf(heap.roots) && mark <= heap.topRoot);
  heap.topRoot = mark;
}

retCode reserveSpace(integer amnt) {
  if ((((ptrPo)heap.curr) + amnt) < ((ptrPo)(heap.limit)))
    return Ok;
  else
    return Error;
}

termPo allocateObject(int32 index, integer amnt) {
  if ((((ptrPo)heap.curr) + amnt) >= ((ptrPo)(heap.limit))) {
    if (gcCollect(amnt) != Ok) {
      char msg[MAX_SYMB_LEN];
      strMsg(msg,NumberOf(msg), "Could not allocate %d cells on heap", amnt);
      syserr(msg);
      return Null;
    }
  }

  termPo t = heap.curr;
  heap.curr = heap.curr + amnt;
  t->lblIndex = index;
  t->space = FIVEAS;
#ifdef TRACEMEM
  if (traceAllocs) {
    numAllocated++;
    totalAllocated += amnt;
    allocationHeaps[lg2(amnt)]++;
  }
#endif
  return t;
}

void recordTermUpdate(termPo t) {
  if (t >= heap.old && t < heap.oldLimit) {
    uint64 add = t - heap.old;

    heap.cards[add >> CARDSHIFT] |= masks[add & CARDMASK];
  }
}

normalPo allocateUnary(int32 index, termPo arg) {
  int root = gcAddRoot(&arg);
  normalPo trm = C_NORMAL(allocateObject(index,NormalCellCount(1)));
  trm->args[0] = arg;
  gcReleaseRoot(root);
  return trm;
}

normalPo allocateBinary(int32 index, termPo left, termPo right) {
  int root = gcAddRoot((&left));
  gcAddRoot((&right));

  normalPo trm = C_NORMAL(allocateObject(index,NormalCellCount(2)));
  gcReleaseRoot(root);

  trm->args[0] = left;
  trm->args[1] = right;
  return trm;
}

normalPo allocateStruct(labelPo lbl) {
  return (normalPo)allocateObject(indexOfLabel(lbl), NormalCellCount(lblArity(lbl)));
}

retCode enoughRoom(labelPo lbl) {
  return reserveSpace(NormalCellCount(lblArity(lbl)));
}

void validPtr(termPo t) {
  if (isPointer(t))
    assert((t >= heap.start && t < heap.limit) || !(t >= heap.base && t < heap.outerLimit));
}

static retCode verifyScanHelper(ptrPo arg, void* c) {
  validPtr(*arg);
  return Ok;
}

void verifyHeap(void) {
  for (termPo t = heap.start; t < heap.curr;) {
    if (hasBuiltinType(t)) {
      builtinClassPo sClass = builtinClassOf(t);
      t = sClass->scanFun(sClass, verifyScanHelper, Null, t);
    }
    else {
      normalPo trm = C_NORMAL(t);
      labelPo lbl = termLbl(trm);
      for (int32 ix = 0; ix < lblArity(lbl); ix++) {
        validPtr(trm->args[ix]);
      }
      t = t + termSize(trm);
    }
  }
}

#ifdef TRACEMEM
void showMemoryStats(ioPo out) {
  outMsg(out, "%ld total allocations, %ld total words\n", numAllocated, totalAllocated);
  integer blockSize = 1;
  for (int ix = 0; ix < NumberOf(allocationHeaps); ix++) {
    if (allocationHeaps[ix] != 0) {
      outMsg(out, "%d allocations of %ld words\n", allocationHeaps[ix], blockSize);
    }
    blockSize <<= 1;
  }
}
#endif
