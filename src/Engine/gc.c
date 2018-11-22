/*
  Garbage collection program
  Copyright (c) 2016, 2017 and beyond. Francis G. McCabe
*/
#include "config.h"

#include <stdlib.h>
#include <assert.h>
#include "engineP.h"      /* access engine definitions */

#include "config.h"    /* pick up standard configuration header */
#include <stdlib.h>
#include <memory.h>
#include <labelsP.h>
#include <globalsP.h>

#ifdef TRACEMEM
long gcCount = 0;                       /* Number of times GC is invoked */
#endif

#ifndef MAX_TABLE
#define MAX_TABLE 2048
#endif

typedef struct _gc_support_ {
  heapPo H;
  long oCnt;
} GCSupport;

static retCode markProcess(processPo P, gcSupportPo G);
static logical hasMoved(termPo t);
static termPo movedTo(termPo t);
static void markMoved(termPo t, termPo where);

/* The standard garbage collector invoked when a process runs out of
   heap is a compacting garbage collector, O(n) in time and space
   although the space overhead can often be shared */

static void swapHeap(heapPo H) {
  oldHeap = heap;      /* copy current settings */
  switch (heap.allocMode) {
    case lowerHalf:
#ifdef TRACEMEM
      if (traceMemory)
        outMsg(logFile, "switching to upper half\n");
#endif
      heap.start = heap.curr;
      heap.limit = heap.outerLimit;  /* shift to the upper half */
      heap.allocMode = upperHalf;    /* It is guaranteed to have enough room */
      break;
    case upperHalf:      /* Shift to the lower half */
#ifdef TRACEMEM
      if (traceMemory)
        outMsg(logFile, "switching to lower half\n");
#endif
      heap.limit = heap.start;
      heap.start = heap.base;
      heap.curr = heap.base;
      heap.allocMode = lowerHalf;
    default:;
  }
}

static logical inSwappedHeap(heapPo H, termPo x) {
  switch (H->allocMode) {
    case lowerHalf:
      return (logical) (x >= H->limit && x < H->outerLimit);
    case upperHalf:
      return (logical) (x >= H->base && x < H->start);
  }
}

retCode gcCollect(heapPo H, long amount) {
  GCSupport GCSRec = {.H=H, .oCnt=0};
  gcSupportPo G = &GCSRec;

#ifdef TRACEMEM
  gcCount++;
  if (traceMemory)
    outMsg(logFile, "starting gc: %d\n%_", gcCount);
#endif

#ifdef TRACEMEM
  if (traceMemory) {
    if (H->owner != Null)
      verifyProc(H->owner, G);
    else
      processProcesses((procProc) verifyProc, G);
  }
#endif

  swapHeap(H);

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
      verifyProc(H->owner, G);
    else
      processProcesses((procProc) verifyProc, G);
  }
#endif

  if (H->limit - H->curr <= amount) {
    if (extendHeap(H, 2, amount) != Ok)
      syserr("Unable to grow process heap");
    return Space;
  }

#ifdef TRACEMEM
  if (traceMemory) {
    outMsg(logFile, "%d bytes used\n", heap.curr - heap.start);
    outMsg(logFile, "%d bytes available\n%_", heap.limit - heap.curr);
  }
#endif
  return Ok;
}

termPo markPtr(gcSupportPo G, ptrPo p) {
  termPo t = *p;

  if (t != Null) {
    if (hasMoved(t))
      return movedTo(t);
    else if (inSwappedHeap(G->H, t)) {
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
        memcpy(nn, t, termSize(C_TERM(t)) * sizeof(termPo));
        G->H->curr += size;
        markMoved(t, nn);
        return nn;
      }
    } else {
      // scanTerm(G,t);
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
    normalPo nml = C_TERM(x);
    integer arity = termArity(nml);
    for (integer ix = 0; ix < arity; ix++)
      nml->args[ix] = markPtr(G, &nml->args[ix]);
    return x + NormalCellCount(arity);
  }
}

static retCode markProcess(processPo P, gcSupportPo G) {
#ifdef TRACEMEM
  if (traceMemory)
    outMsg(logFile, "Mark process %d\n%_", P->processNo);
#endif

  ptrPo t = P->sp;
  framePo f = P->fp;

  while (f < (framePo) P->stackLimit) {
    while (t < (ptrPo) f) {
      *t = markPtr(G, t);
      t++;
    }
    integer off = insOffset(f->prog, f->rtn);
    f->prog = (methodPo) markPtr(G, (ptrPo) &f->prog);
    f->rtn = pcAddr(f->prog, off);
    t = (ptrPo) (f + 1);
    f = f->fp;
  }

  integer off = insOffset(P->prog, P->pc);
  P->prog = (methodPo) markPtr(G, (ptrPo) &P->prog);
  P->pc = pcAddr(P->prog, off);
  return Ok;
}

void verifyProc(processPo P, GCSupport *G) {
#ifdef TRACEMEM
  if (traceMemory)
    outMsg(logFile, "Verify process %d\n%_", P->processNo);
#endif
  heapPo H = G->H;
  integer lvl = 0;

  ptrPo t = P->sp;
  framePo f = P->fp;

  assert(t <= (ptrPo) f);
  assert((ptrPo) f < (ptrPo) P->stackLimit);

  while (f < (framePo) P->stackLimit) {
    inStackPtr(P, t);
    inStackPtr(P, (ptrPo) f);

    while (t < (ptrPo) f) {
      validPtr(H, *t);
      t++;
    }

    validPtr(H, (termPo) f->prog);

    t = (ptrPo) (f + 1);
    f = f->fp;
    lvl++;
  }

#ifdef TRACEMEM
  if (traceMemory)
    outMsg(logFile, "process %d ok\n%_", P->processNo);
#endif
}

void dumpGcStats() {
  logMsg(logFile, "%d allocations, %d words, %d gc collections", numAllocated, totalAllocated, gcCount);
}
