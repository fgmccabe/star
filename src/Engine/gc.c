/*
  Garbage collection program
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
*/
#include "config.h"

#include <stdlib.h>
#include <assert.h>
#include "engineP.h"      /* access engine definitions */

#include "config.h"    /* pick up standard configuration header */
#include <memory.h>
#include <labelsP.h>
#include <globalsP.h>
#include <debug.h>
#include "heap.h"

#ifdef TRACEMEM
long gcCount = 0;                       /* Number of times GC is invoked */
long gcGrow = 0;
#endif

#ifndef MAX_TABLE
#define MAX_TABLE 2048
#endif

static retCode markProcess(processPo P, gcSupportPo G);
static logical hasMoved(termPo t);
static termPo movedTo(termPo t);
static void markMoved(termPo t, termPo where);
static retCode extendHeap(heapPo H, integer factor, integer hmin);
static termPo finalizeTerm(gcSupportPo G, termPo x);

/* The standard garbage collector invoked when a process runs out of
   heap is a compacting garbage collector, O(n) in time and space
   although the space overhead can often be shared */

static void swapHeap(gcSupportPo G, heapPo H) {
  assert(H == &heap);

  switch (H->allocMode) {
    case lowerHalf:
#ifdef TRACEMEM
      if (traceMemory)
        outMsg(logFile, "switching to upper half\n");
#endif
      assert(H->outerLimit - H->split >= H->curr - H->start);
      H->start = H->curr = H->split;
      H->limit = H->outerLimit;  /* shift to the upper half */
      H->allocMode = upperHalf;    /* It is guaranteed to have enough room */
      break;
    case upperHalf:      /* Shift to the lower half */
#ifdef TRACEMEM
      if (traceMemory)
        outMsg(logFile, "switching to lower half\n");
#endif
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
  GCSupport GCSRec;
  gcSupportPo G = &GCSRec;

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap(H);
  }
#endif

  setupGCSupport(H, G);

#ifdef TRACEMEM
  gcCount++;
  if (traceMemory)
    outMsg(logFile, "starting gc: %d (%ld instructions)\n%_", gcCount, pcCount);
#endif

#ifdef TRACEMEM
  if (traceMemory) {
    if (H->owner != Null)
      verifyProc(H->owner, H);
    else
      processProcesses((procProc) verifyProc, H);
  }
#endif

  swapHeap(G, H);

  for (int i = 0; i < H->topRoot; i++)    /* mark the external roots */
    *H->roots[i] = markPtr(G, H->roots[i]);

  markLabels(G);
  markGlobals(G);

  if (H->owner != Null)
    markProcess(H->owner, G);
  else
    processProcesses((procProc) markProcess, G);

#ifdef TRACEMEM
  if (traceMemory) {
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
  if (traceMemory) {
    if (H->owner != Null)
      verifyProc(H->owner, H);
    else
      processProcesses((procProc) verifyProc, H);
  }
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
  if (traceMemory) {
    outMsg(logFile, "%d objects found\n", G->oCnt);
    outMsg(logFile, "%d bytes used\n", H->curr - H->start);
    outMsg(logFile, "%d bytes available\n%_", H->limit - H->curr);
  }
#endif
  return Ok;
}

termPo markPtr(gcSupportPo G, ptrPo p) {
  termPo t = *p;

  if (t != Null) {
    if (hasMoved(t))
      return movedTo(t);
    else if (inSwappedHeap(G, t)) {
      if (isSpecialClass(t->clss)) {
        specialClassPo special = (specialClassPo) t->clss;
        termPo nn = G->H->curr;
        G->H->curr = special->copyFun(special, nn, t);
        G->oCnt++;
        markMoved(t, nn);
        return nn;
      } else {
        G->oCnt++;

        labelPo lbl = C_LBL((termPo) t->clss);
        integer size = NormalCellCount(lbl->arity);
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
  if (isSpecialClass(x->clss)) {
    specialClassPo sClass = (specialClassPo) classOf(x);
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
    if (isSpecialClass(n->clss)) {
      specialClassPo sClass = (specialClassPo) classOf(n);

      return x + sClass->sizeFun(sClass, n);
    } else {
      return x + NormalCellCount(termArity(C_NORMAL(n)));
    }
  } else if (isSpecialClass(x->clss)) {
    specialClassPo sClass = (specialClassPo) classOf(x);
    sClass->finalizer(sClass, x);
    return x + sClass->sizeFun(sClass, x);

  } else {
    return x + NormalCellCount(termArity(C_NORMAL(x)));
  }
}

static retCode markProcess(processPo P, gcSupportPo G) {
#ifdef TRACEMEM
  if (traceMemory)
    outMsg(logFile, "Mark process %d\n%_", P->processNo);
#endif
  P->stk = C_TASK(markPtr(G, (ptrPo) &P->stk));

  return Ok;
}

void verifyProc(processPo P, heapPo H) {
  verifyTask(P->stk, H);
}

void dumpGcStats() {
#ifdef TRACEMEM
  if(traceAllocs)
    logMsg(logFile, "%ld char allocations, %ld integer allocations, %ld float allocations, %ld total allocations, %ld total words",
         allocatedChars,allocatedInts,allocatedFloats,numAllocated, totalAllocated);
  if(traceMemory)
    logMsg(logFile, "%d gc collections, %d heap grows, %d stack extensions", gcCount, gcGrow, stkGrow);
#endif
}

retCode extendHeap(heapPo H, integer factor, integer hmin) {
  gcGrow++;
  integer newSize = (integer) ((H->outerLimit - H->base) * factor + hmin);

#ifdef TRACEMEM
  if (traceMemory)
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
  if (traceMemory) {
    if (H->owner != Null)
      verifyProc(H->owner, H);
    else
      processProcesses((procProc) verifyProc, H);
  }
#endif

  for (int i = 0; i < H->topRoot; i++)    /* mark the external roots */
    *H->roots[i] = markPtr(G, H->roots[i]);

  markLabels(G);
  markGlobals(G);

  if (H->owner != Null)
    markProcess(H->owner, G);
  else
    processProcesses((procProc) markProcess, G);

#ifdef TRACEMEM
  if (traceMemory) {
    outMsg(logFile, "%d objects found in mark phase\n%_", G->oCnt);
  }
#endif

  termPo t = H->start;
  while (t < H->curr)
    t = scanTerm(G, t);

#ifdef TRACEMEM
  if (traceMemory) {
    if (H->owner != Null)
      verifyProc(H->owner, H);
    else
      processProcesses((procProc) verifyProc, H);
  }
#endif

  free(oldHeap);

  if (H->limit - H->curr <= hmin) {
    syserr("Unable to grow process heap");
    return Space;
  }

#ifdef TRACEMEM
  if (traceMemory) {
    outMsg(logFile, "%d bytes used\n", H->curr - H->start);
    outMsg(logFile, "%d bytes available\n%_", H->limit - H->curr);
  }
#endif
  return Ok;
}
