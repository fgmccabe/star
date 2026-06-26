//
// Created by Francis McCabe on 6/17/26.
// Generational garbage collector.
//
#include <stdlib.h>

#include "abort.h"
#include "config.h"

#include "heapP.h"
#include "engineP.h"
#include "globalsP.h"
#include "normalP.h"

static long gcCount = 0;        /* Number of times GC is invoked */
static long gcCompactCount = 0; // Number of heap compactions
static long gcGrow = 0;

static timerPo gcTimer = Null;

static void markOldGen(gcSupportPo G);
static termPo markHeapTerm(gcSupportPo G, termPo x);
static termPo finalizeTerm(gcSupportPo G, termPo x);
static void extendHeap(integer hmin);

static void swapHeap(void) {
  // assert(heap.outerLimit - heap.split >= heap.curr - heap.start);
  switch (heap.allocMode) {
  case lowerPhase1: {
    heap.start = heap.curr = heap.split;
    heap.limit = heap.outerLimit; /* shift to the upper half */
    heap.allocMode = upperPhase1; /* It is guaranteed to have enough room */
    break;
  }
  case upperPhase1: {
    /* Shift to the lower half */
    heap.limit = heap.split;
    heap.start = heap.curr = heap.oldLimit;
    heap.allocMode = lowerPhase2;
    break;
  }
  case lowerPhase2: {
    heap.start = heap.curr = heap.split;
    heap.limit = heap.outerLimit; /* shift to the upper half */
    heap.allocMode = upperPhase2; /* It is guaranteed to have enough room */
    break;
  }
  case upperPhase2: {
    /* Shift to the lower half */
    heap.limit = heap.split;
    heap.start = heap.curr = heap.oldLimit;
    heap.allocMode = lowerPhase1;
    break;
  }
  default: ;
  }
}

// The core of the GC algorithm.

static void gC(gcSupportPo G) {
  for (int i = 0; i < heap.topRoot; i++) /* mark the external roots */
    *heap.roots[i] = markPtr(G, heap.roots[i]);

  markLabels(G);
  markGlobals(G);
  markProcesses(G);

  markOldGen(G);

  termPo t = heap.start;
  while (t < heap.curr) {
    assert(t >= heap.start && t < heap.curr);
    t = markHeapTerm(G, t);
    assert(heap.curr <= heap.limit);
  }

  termPo f = G->oldBase;
  while (f < G->oldLimit) {
    f = finalizeTerm(G, f);
  }
}

retCode gcCollect(long amount) {
#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "GC #%d\n%_", gcCount);
  }
#endif

  GCSupport GCSRec = {
    .oldBase = heap.start, .oldLimit = heap.curr, .oCnt = 0
  };
  gcSupportPo G = &GCSRec;

  logical rT = isTimerRunning(runTimer);

  if (rT)
    pauseTimer(runTimer);

  if (gcTimer == Null)
    gcTimer = newTimer("gc");

  resumeTimer(gcTimer);

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap();
  }
#endif

  gcCount++;
  swapHeap();

  gC(G);

  if (heap.allocMode == lowerPhase1) { // Back to allocating in lower space
    assert(heap.start == heap.oldLimit);

    heap.oldLimit = heap.start = heap.curr; // Bring new generation into old

    integer cellCount = heap.outerLimit - heap.start; // How much heap is left?

    heap.limit = heap.split = heap.start + cellCount / 2;

    for (int32 ix = 0; ix < heap.ncards; ix++)
      heap.cards[ix] = 0; /* clear the card table */
  }

  if (heap.limit - heap.curr < (heap.outerLimit - heap.start) / 10) {
    gcCompactCount++;
#ifdef TRACEMEM
    if (traceMemory > noTracing) {
      outMsg(logFile, "GC compaction #%d\n%_", gcCompactCount);
    }
#endif

    compactHeap();
    if (heap.limit - heap.curr + amount < (heap.outerLimit - heap.start) / 10) {
      extendHeap(amount);
    }
  }

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap();
    verifyProcesses();
  }
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found\n", G->oCnt);
    outMsg(logFile, "%d cells used\n", heap.curr - heap.start);
    outMsg(logFile, "%d cells in old space\n", heap.oldLimit - heap.old);
    outMsg(logFile, "%d cells available\n%_", heap.limit - heap.curr);
    heapSummary(logFile);
  }
#endif

  pauseTimer(gcTimer);
  if (rT)
    resumeTimer(runTimer);
  return Ok;
}

void markOldGen(gcSupportPo G) {
  integer max = ((heap.oldLimit - heap.old) + CARDMASK) >> CARDSHIFT;

  for (integer ix = 0; ix < max; ix++) {
    if (heap.cards[ix] != 0) {
      for (integer jx = 0; jx < CARDWIDTH; jx++)
        if ((heap.cards[ix] & (1ull << jx)) != 0)
          markHeapTerm(G, heap.old + (ix << CARDSHIFT) + jx);
    }
  }
}

static retCode markScanHelper(ptrPo arg, void* c) {
  *arg = markPtr((gcSupportPo)c, arg);
  return Ok;
}

termPo markHeapTerm(gcSupportPo G, termPo x) {
  if (hasBuiltinType(x)) {
    builtinClassPo sClass = builtinClassOf(x);
    sClass->scanFun(markScanHelper, G, x);
    return x + sClass->sizeFun(sClass, x);
  }
  else {
    normalPo nml = C_NORMAL(x);
    integer arity = termArity(nml);
    for (integer ix = 0; ix < arity; ix++)
      nml->args[ix] = markPtr(G, &nml->args[ix]);
    return x + NormalCellCount(arity);
  }
}

termPo finalizeTerm(gcSupportPo G, termPo x) {
  if (hasMoved(x)) {
    termPo n = movedTo(x);
    if (hasBuiltinType(n)) {
      builtinClassPo sClass = builtinClassOf(n);

      return x + sClass->sizeFun(sClass, n);
    }
    else {
      return x + NormalCellCount(termArity(C_NORMAL(n)));
    }
  }
  else if (hasBuiltinType(x)) {
    builtinClassPo sClass = builtinClassOf(x);
    sClass->finalizer(sClass, x);
    return x + sClass->sizeFun(sClass, x);
  }
  else {
    return x + NormalCellCount(termArity(C_NORMAL(x)));
  }
}

void extendHeap(integer hmin) {
  integer newSize = (heap.outerLimit - heap.base) * 2 + hmin;
  if (newSize > maxHeapSize) {
    outMsg(logFile, "Maximum heap size (%ld) exceeded", maxHeapSize);
    star_exit(oomCode);
  }
  assert(heap.allocMode==lowerPhase1);
  termPo oldHeap = heap.base;
  GCSupport GCSRec = {.oldBase = oldHeap, .oldLimit = heap.curr};
  gcSupportPo G = &GCSRec;
  free(heap.cards);
  createHeap(newSize);

  gcGrow++;

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    outMsg(logFile, "extending heap to %ld\n%_", newSize);
#endif

  gC(G);

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses();
#endif

  free(oldHeap);

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d bytes used\n", heap.curr - heap.start);
    outMsg(logFile, "%d bytes available\n%_", heap.limit - heap.curr);
  }
#endif
}
