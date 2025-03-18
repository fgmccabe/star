/*
  Garbage collection program
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
*/
#include "config.h"

#include <stdlib.h>
#include <assert.h>
#include "engineP.h"      /* access engine definitions */
#include "debug.h"

#include <memory.h>
#include <globalsP.h>

long gcCount = 0;                       /* Number of times GC is invoked */
long gcGrow = 0;

#ifndef MAX_TABLE
#define MAX_TABLE 2048
#endif

static logical hasMoved(termPo t);
static termPo movedTo(termPo t);
static void markMoved(termPo t, termPo where);
static retCode extendHeap(heapPo H, integer factor, integer hmin);
static termPo finalizeTerm(gcSupportPo G, termPo x);

timerPo gcTimer = Null;

/* The standard garbage collector invoked when a process runs out of
   heap is a compacting garbage collector, O(n) in time and space
   although the space overhead can often be shared */

static void swapHeap(gcSupportPo G, heapPo H) {
  assert(H == &heap);

  switch (H->allocMode) {
    case lowerHalf:
      assert(H->outerLimit - H->split >= H->curr - H->start);
      H->start = H->curr = H->split;
      H->limit = H->outerLimit;  /* shift to the upper half */
      H->allocMode = upperHalf;    /* It is guaranteed to have enough room */
      break;
    case upperHalf:      /* Shift to the lower half */
      assert(H->split - H->base >= H->curr - H->start);
      H->limit = H->split;
      H->start = H->curr = H->base;
      H->allocMode = lowerHalf;
    default:;
  }
}

static logical inSwappedHeap(gcSupportPo G, termPo x) {
  return (logical) (x >= G->oldBase && x < G->oldLimit);
}

void setupGCSupport(heapPo H, gcSupportPo G) {
  G->oldBase = H->start;
  G->oldLimit = H->curr;

  G->H = H;
  G->oCnt = 0;
}

retCode gcCollect(heapPo H, long amount) {
#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "GC #%d @ %ld\n%_", gcCount, pcCount);
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
    verifyProcesses(H);
#endif

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap(H);
  }
#endif

  setupGCSupport(H, G);

  gcCount++;
  swapHeap(G, H);

  for (int i = 0; i < H->topRoot; i++)    /* mark the external roots */
    *H->roots[i] = markPtr(G, H->roots[i]);

  markLabels(G);
  markGlobals(G);
  markProcesses(H->owner, G);

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found in mark root phase\n%_", G->oCnt);
  }
#endif

  termPo t = H->start;
  while (t < H->curr) {
    assert(t >= H->start && t < H->curr);
    t = scanTerm(G, t);
    assert(H->curr <= H->limit);
  }

  termPo f = G->oldBase;
  while (f < G->oldLimit) {
    f = finalizeTerm(G, f);
  }

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses(H);
#endif

  if (H->limit - H->curr <= amount + 100) {
    if (extendHeap(H, 2, amount) != Ok) {
      syserr("Unable to grow process heap");
      return Space;
    }
  }

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap(H);
  }
#endif

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found\n", G->oCnt);
    outMsg(logFile, "%d bytes used\n", H->curr - H->start);
    outMsg(logFile, "%d bytes available\n%_", H->limit - H->curr);
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
      clssPo clss = classOf(t);
      if (isSpecialClass(clss)) {
        specialClassPo special = (specialClassPo) clss;
        termPo nn = G->H->curr;
        G->H->curr = special->copyFun(special, nn, t);
        G->oCnt++;
        markMoved(t, nn);
        return nn;
      } else {
        G->oCnt++;

        labelPo lbl = C_LBL((termPo) clss);
        int32 size = NormalCellCount(lblArity(lbl));
        termPo nn = G->H->curr;
        memcpy(nn, t, termSize(C_NORMAL(t)) * sizeof(termPo));
        G->H->curr += size;
        markMoved(t, nn);
        return nn;
      }
    } else {
      assert(!inHeap(G->H, t));
      return t;
    }
  } else
    return t;
}

static logical hasMoved(termPo t) {
  uint64 ix = (uint64) t->clss;
  return (logical) ((ix & (uint64) 1) == (uint64) 1);
}

static termPo movedTo(termPo t) {
  assert(hasMoved(t));

  uint64 ix = ((uint64) t->clss) & ~(uint64) 1;
  return (termPo) ix;
}

static void markMoved(termPo t, termPo where) {
  uint64 ix = ((uint64) where) | (uint64) 1;
  t->clss = (clssPo) ix;
}

static retCode markScanHelper(ptrPo arg, void *c) {
  *arg = markPtr((gcSupportPo) c, arg);
  return Ok;
}

termPo scanTerm(gcSupportPo G, termPo x) {
  clssPo clss = classOf(x);
  if (isSpecialClass(clss)) {
    specialClassPo sClass = (specialClassPo) clss;
    return sClass->scanFun(sClass, markScanHelper, G, x);
  } else {
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
    if (isSpecialClass(classOf(n))) {
      specialClassPo sClass = (specialClassPo) classOf(n);

      return x + sClass->sizeFun(sClass, n);
    } else {
      return x + NormalCellCount(termArity(C_NORMAL(n)));
    }
  } else if (isSpecialClass(classOf(x))) {
    specialClassPo sClass = (specialClassPo) classOf(x);
    sClass->finalizer(sClass, x);
    return x + sClass->sizeFun(sClass, x);

  } else {
    return x + NormalCellCount(termArity(C_NORMAL(x)));
  }
}

void dumpGcStats(ioPo out) {
  logMsg(out, "%ld total allocations, %ld total words", numAllocated, totalAllocated);
  logMsg(out, "%d gc collections, %d heap grows", gcCount, gcGrow);
}

retCode extendHeap(heapPo H, integer factor, integer hmin) {
  integer newSize = (integer) ((H->outerLimit - H->base) * factor + hmin);

  gcGrow++;

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    outMsg(logFile, "extending heap by: %ld+%ld to %ld\n%_", factor, hmin, newSize);
#endif

  if (newSize > maxHeapSize)
    return Error;

  termPo newHeap = (termPo) malloc(sizeof(ptrPo) * newSize);
  termPo oldHeap = H->base;

  GCSupport GCSRec;
  gcSupportPo G = &GCSRec;

  setupGCSupport(H, G);

  H->curr = H->old = H->base = H->start = newHeap;
  H->outerLimit = newHeap + newSize;
  H->limit = H->split = H->base + newSize / 2;
  H->allocMode = lowerHalf;

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses(H);
#endif

  for (int i = 0; i < H->topRoot; i++)    /* mark the external roots */
    *H->roots[i] = markPtr(G, H->roots[i]);

  markLabels(G);
  markGlobals(G);
  markProcesses(H->owner, G);

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d objects found in mark phase\n%_", G->oCnt);
  }
#endif

  termPo t = H->start;
  while (t < H->curr)
    t = scanTerm(G, t);

#ifdef TRACEMEM
  if (traceMemory > noTracing)
    verifyProcesses(H);
#endif

  free(oldHeap);

  if (H->limit - H->curr <= hmin) {
    syserr("Unable to grow process heap");
    return Space;
  }

#ifdef TRACEMEM
  if (traceMemory > noTracing) {
    outMsg(logFile, "%d bytes used\n", H->curr - H->start);
    outMsg(logFile, "%d bytes available\n%_", H->limit - H->curr);
  }
#endif
  return Ok;
}
