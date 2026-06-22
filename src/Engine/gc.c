/*
  Garbage collection
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
*/
#include "config.h"

#include <stdlib.h>
#include <assert.h>
#include "engineP.h"      /* access engine definitions */
#include "debug.h"
#include <memory.h>
#include <globalsP.h>

#include "abort.h"
#include "normalP.h"

static long gcCnt = 0; /* Number of times GC is invoked */
static long gcGrow = 0;

static void markMoved(termPo t, termPo where);
static void extendHeap(integer hmin);
static termPo finalizeTerm(gcSupportPo G, termPo x);
static termPo scanTerm(gcSupportPo G, termPo x);

timerPo gcTimer = Null;

/* The standard garbage collector invoked when a process runs out of
   heap is a compacting garbage collector, O(n) in time and space
   although the space overhead can often be shared */

static void swapHeap(void) {
  switch (heap.allocMode) {
  case lowerPhase1:
    assert(heap.outerLimit - heap.split >= heap.curr - heap.start);
    heap.start = heap.curr = heap.split;
    heap.limit = heap.outerLimit; /* shift to the upper half */
    heap.allocMode = upperPhase1; /* It is guaranteed to have enough room */
    break;
  case upperPhase1: /* Shift to the lower half */
    assert(heap.split - heap.base >= heap.curr - heap.start);
    heap.limit = heap.split;
    heap.start = heap.curr = heap.base;
    heap.allocMode = lowerPhase1;
  default: ;
  }
}

static logical inSwappedHeap(gcSupportPo G, termPo x) {
  return (logical)(x >= G->oldBase && x < G->oldLimit);
}

void setupGCSupport(gcSupportPo G) {
  G->oldBase = heap.start;
  G->oldLimit = heap.curr;

  G->oCnt = 0;
}

retCode gcCollectOld(long amount) {
#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "GC #%d\n%_", gcCnt);
  }
#endif

  GCSupport GCSRec;
  gcSupportPo G = &GCSRec;

  logical rT = isTimerRunning(runTimer);

  if (rT)
    pauseTimer(runTimer);

  if (gcTimer == Null)
    gcTimer = newTimer("gc");

  resumeTimer(gcTimer);

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses();
#endif

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap();
  }
#endif

  setupGCSupport(G);

  gcCnt++;
  swapHeap();

  for (int i = 0; i < heap.topRoot; i++) /* mark the external roots */
    *heap.roots[i] = markPtr(G, heap.roots[i]);

  markLabels(G);
  markGlobals(G);
  markProcesses(G);

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found in mark root phase\n%_", G->oCnt);
  }
#endif

  termPo t = heap.start;
  while (t < heap.curr) {
    assert(t >= heap.start && t < heap.curr);
    t = scanTerm(G, t);
    assert(heap.curr <= heap.limit);
  }

  termPo f = G->oldBase;
  while (f < G->oldLimit) {
    f = finalizeTerm(G, f);
  }

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses();
#endif

  if (heap.limit - heap.curr <= amount + 100) {
    extendHeap(amount);
  }

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap();
  }
#endif

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found\n", G->oCnt);
    outMsg(logFile, "%d bytes used\n", heap.curr - heap.start);
    outMsg(logFile, "%d bytes available\n%_", heap.limit - heap.curr);
  }
#endif

  pauseTimer(gcTimer);
  if (rT)
    resumeTimer(runTimer);
  return Ok;
}

termPo markPtr(gcSupportPo G, ptrPo p) {
  termPo t = *p;

  if (t != Null && isPointer(t)) {
    if (hasMoved(t))
      return movedTo(t);
    else if (inSwappedHeap(G, t)) {
      if (hasBuiltinType(t)) {
        builtinClassPo special = builtinClassOf(t);
        termPo nn = heap.curr;
        heap.curr = special->copyFun(special, nn, t);
        G->oCnt++;
        markMoved(t, nn);
        assert(hasMoved(t));
        return nn;
      }
      else {
        G->oCnt++;

        labelPo lbl = termLbl(C_NORMAL(t));
        int32 size = NormalCellCount(lblArity(lbl));
        termPo nn = heap.curr;
        memcpy(nn, t, termSize(C_NORMAL(t)) * sizeof(termPo));
        heap.curr += size;
        markMoved(t, nn);
        return nn;
      }
    }
    else {
      assert(isOldRef(t) || !inHeap(t));
      return t;
    }
  }
  else {
    return t;
  }
}

logical hasMoved(termPo t) {
  uint32 token = t->space;
  return (logical)((token & 1) == 1);
}

termPo movedTo(termPo t) {
  assert(hasMoved(t));

  uint64 tgt = (((uint64)(t->lblIndex)) << 32) | (((uint32)t->space) & ~1U);
  return (termPo)tgt;
}

static void markMoved(termPo t, termPo where) {
  uint32 lower = ((uint32)((uint64)where)) | 1u;
  uint32 upper = (uint32)(((uint64)where) >> 32u);
  t->lblIndex = (int32)upper;
  t->space = (int32)lower;
}

static retCode markScanHelper(ptrPo arg, void* c) {
  *arg = markPtr((gcSupportPo)c, arg);
  return Ok;
}

termPo scanTerm(gcSupportPo G, termPo x) {
  if (hasBuiltinType(x)) {
    builtinClassPo sClass = builtinClassOf(x);
    return sClass->scanFun(sClass, markScanHelper, G, x);
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

void dumpGcStats(ioPo out) {
  logMsg(out, "%d gc collections, %d heap grows", gcCnt, gcGrow);
#ifdef TRACEMEM
  showMemoryStats(out);
#endif
}

void extendHeap(integer hmin) {
  integer newSize = (heap.outerLimit - heap.base) * 2 + hmin;

  if (newSize > maxHeapSize) {
    outMsg(logFile, "Maximum heap size (%ld) exceeded", maxHeapSize);
    star_exit(oomCode);
  }

  termPo newHeap = (termPo)malloc(sizeof(ptrPo) * newSize);
  if (newHeap == Null) {
    outMsg(logFile, "Unable to allocate space (%ld cells) for heap", newSize);
    star_exit(oomCode);
  }

  gcGrow++;

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    outMsg(logFile, "extending heap to %ld\n%_", newSize);
#endif

  termPo oldHeap = heap.base;

  GCSupport GCSRec;
  gcSupportPo G = &GCSRec;

  setupGCSupport(G);

  heap.curr = heap.old = heap.base = heap.start = newHeap;
  heap.outerLimit = newHeap + newSize;
  heap.limit = heap.split = heap.base + newSize / 2;
  heap.allocMode = lowerPhase1;

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses();
#endif

  for (int i = 0; i < heap.topRoot; i++) /* mark the external roots */
    *heap.roots[i] = markPtr(G, heap.roots[i]);

  markLabels(G);
  markGlobals(G);
  markProcesses(G);

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found in mark phase\n%_", G->oCnt);
  }
#endif

  termPo t = heap.start;
  while (t < heap.curr)
    t = scanTerm(G, t);

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
