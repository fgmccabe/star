//
// Created by Francis McCabe on 6/22/26.
//

#include "compact.h"

#include <stdlib.h>

#include "constantsP.h"
#include "engineP.h"
#include "globalsP.h"
#include "labelsP.h"
#include "normalP.h"

// Compacting phase of garbage collector.
// We re-used the card table for a special mark phase
// And use a 'break' table to handle relocation.

static void cardMarkTerm(termPo t);

static logical isMarked(termPo t) { // Use the card table for mark bits
  uint64 add = t - heap.base;
  return (heap.cards[add >> CARDSHIFT] & (1ull << (add & CARDMASK)) ? True : False);
}

static void markCard(termPo t) {
  uint64 add = t - heap.base;
  heap.cards[add >> CARDSHIFT] |= (1ull << (add & CARDMASK));
}

static retCode cardMarkHelper(ptrPo p, void* c);

static uint64 termCount = 0;
static uint64 heapSize = 0;

void cardMarkTerm(termPo t) {
  if (t != Null && isPointer(t) && isHeapRef(t) && !isMarked(t)) {
    markCard(t);
    termCount++;
    if (hasBuiltinType(t)) {
      builtinClassPo special = builtinClassOf(t);
      special->scanFun(cardMarkHelper, Null, t);
      heapSize += special->sizeFun(special, t);
    }
    else {
      labelPo lbl = termLbl(C_NORMAL(t));
      int32 arity = lblArity(lbl);
      normalPo term = C_NORMAL(t);
      for (int32 ax = 0; ax < arity; ax++) {
        cardMarkTerm(term->args[ax]);
      }
      heapSize += arity + 1;
    }
  }
}

retCode cardMarkHelper(ptrPo p, void* c) {
  cardMarkTerm(*p);
  return Ok;
}

typedef struct {
  termPo start;
  termPo final;
} breakEntry, *breakPo;

typedef struct {
  breakPo from;
  breakPo to;
} BreakTable, *breakTablePo;

static void adjustHeap(breakPo from, breakPo to);

static termPo searchBreak(breakTablePo breaks, termPo p) {
  breakPo from = breaks->from;
  breakPo to = breaks->to;
  while (to > from) {
    breakPo middle = &from[(to - from) / 2];

    if (middle->start > p) {
      if (middle != from)
        to = middle;
      else
        return Null; /* couldnt find it in the table */
    }
    else if (middle->start < p) {
      if (middle != from)
        from = middle;
      else
        return Null;
    }
    else if (middle->start == p)
      return middle->final;
    else
      return Null;
  }
  return Null;
}

static int32 shuffleTerms(integer count) {
  int32 tx = 0;
  breakPo breaks = (breakPo)malloc(sizeof(breakEntry) * count);
  // Reset the heap pointers
  heap.old = heap.oldLimit = heap.start = heap.curr = heap.base;

  for (int32 ix = 0; ix < heap.ncards; ix++) {
    if (heap.cards[ix] != 0) {
      for (int32 jx = 0; jx < CARDWIDTH; jx++) {
        if (heap.cards[ix] & (1ul << jx)) {
          termPo t = heap.base + (ix << CARDSHIFT) + jx;
          breaks[tx].start = t;
          breaks[tx++].final = heap.curr;

          assert(heap.curr<=t);

          if (hasBuiltinType(t)) {
            builtinClassPo special = builtinClassOf(t);
            heap.curr = special->copyFun(special, heap.curr, t);
          }
          else {
            labelPo lbl = termLbl(C_NORMAL(t));
            int32 size = NormalCellCount(lblArity(lbl));
            for (int32 ex = 0; ex < size; ex++) {
              *heap.curr++ = *t++;
            }
          }
        }
      }
    }
  }

  adjustHeap(breaks, &breaks[tx]);

  free(breaks);

  heap.limit = heap.split = heap.base + (heap.outerLimit-heap.base) / 2;
  heap.allocMode = lowerPhase1;
  assert(heap.curr<=heap.limit);
  return tx;
}

static retCode adjustTerm(ptrPo p, void* cl) {
  termPo t = *p;
  if (t != Null && isPointer(t) && isHeapRef(t)) {
    termPo dest = searchBreak((breakTablePo)cl, t);
    assert(isHeapRef(dest));
    *p = dest;
  }
  return Ok;
}

void adjustHeap(breakPo from, breakPo to) {
  BreakTable breakTable = {.from = from, .to = to};

  // Adjust all pointers
  for (int i = 0; i < heap.topRoot; i++) /* mark the external roots */
    adjustTerm(heap.roots[i],&breakTable);

  scanLabels(adjustTerm, &breakTable);
  scanGlobals(adjustTerm, &breakTable);
  scanProcesses(adjustTerm, &breakTable);
  scanConstants(adjustTerm, &breakTable);

  termPo t = heap.start;
  while (t < heap.curr) {
    assert(t >= heap.start && t < heap.curr);

    if (hasBuiltinType(t)) {
      builtinClassPo sClass = builtinClassOf(t);
      sClass->scanFun(adjustTerm, &breakTable, t);
      t += sClass->sizeFun(sClass, t);
    }
    else {
      normalPo nml = C_NORMAL(t);
      integer arity = termArity(nml);
      for (integer ix = 0; ix < arity; ix++)
        adjustTerm(&nml->args[ix],&breakTable);
      t += NormalCellCount(arity);
    }
  }
}

void compactHeap() {
#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap();
    verifyProcesses();
  }
#endif
  // Clear card table.
  for (int32 ix = 0; ix < heap.ncards; ix++) {
    heap.cards[ix] = 0;
  }

  termCount = 0;
  heapSize = 0;

  // Mark everything
  for (int i = 0; i < heap.topRoot; i++) /* mark the external roots */
    cardMarkTerm(*heap.roots[i]);

  scanLabels(cardMarkHelper, Null);
  scanGlobals(cardMarkHelper, Null);
  scanProcesses(cardMarkHelper, Null);
  scanConstants(cardMarkHelper, Null);

  assert(heapSize>>CARDSHIFT <= heap.ncards);
  assert(heapSize>=termCount);

  int32 tx = shuffleTerms(termCount);
  assert(tx==termCount);

  for (int32 ix = 0; ix < heap.ncards; ix++)
    heap.cards[ix] = 0; /* clear the card table */

#ifdef TRACEMEM
  if (validateMemory) {
    verifyHeap();
    verifyProcesses();
  }
#endif
}
