#include "config.h"
#include <unistd.h>
#include <stdlib.h>

#include "abort.h"
#include "heapP.h"
#include "labelsP.h"
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

void initHeap(long heapSize) {
  if (heap.base == NULL) {
    int64 cellCount = heapSize;
    heap.curr = heap.old = heap.base = heap.start =
      (termPo)malloc(sizeof(ptrPo) * cellCount); /* Allocate heap */
    if (heap.curr == Null) {
      outMsg(logFile, "Unable to create heap of %ld cells", heapSize);
      star_exit(oomCode);
    }
    else {
      heap.outerLimit = heap.base + cellCount; /* The actual outer limit */
      heap.limit = heap.split = heap.base + cellCount / 2;
      heap.allocMode = lowerPhase1;

      heap.oldLimit = heap.start; // Nothing in the old generation at start.
      heap.ncards = (cellCount + CARDWIDTH - 1) / CARDWIDTH;
      heap.cards = (cardMap*)malloc(heap.ncards * sizeof(cardMap));

      for (int32 ix = 0; ix < heap.ncards; ix++)
        heap.cards[ix] = 0; /* clear the card table */
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

void heapSummary(ioPo out) {
  outMsg(out, BLUE_ESC_ON"Heap: |0x%lx..old..0x%lx|"BLUE_ESC_OFF, heap.old, heap.oldLimit);
  switch (heap.allocMode) {
  case lowerPhase1:
  case lowerPhase2:
    outMsg(out,GREEN_ESC_ON"%s 0x%lx..0x%lx..0x%lx|\n"GREEN_ESC_OFF,
           allocModeNames[heap.allocMode], heap.start, heap.curr, heap.limit);
    break;
  default:

    outMsg(out,YELLOW_ESC_ON"%s0x%lx..0x%lx..0x%lx|\n"YELLOW_ESC_OFF,
           allocModeNames[heap.allocMode], heap.start, heap.curr, heap.limit);
    break;
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

void showCards() {
  outMsg(logFile, "%d cards in table\n",heap.ncards);
  for (int ix = 0; ix < heap.ncards; ix++) {
    if (heap.cards[ix]!=0) {
      outMsg(logFile, "card 0x%lx: %lb\n", heap.old+ix, heap.cards[ix]);
    }
  }
}

void recordTermUpdate(termPo t) {
  if (t >= heap.old && t < heap.oldLimit) {
    uint64 add = t - heap.old;

    assert((add>>CARDSHIFT) < heap.ncards);

    heap.cards[add >> CARDSHIFT] |= (1ull << (add & CARDMASK));
  }
}

logical hasCard(termPo t) {
  if (t >= heap.old && t < heap.oldLimit) {
    uint64 add = t - heap.old;

    return heap.cards[add >> CARDSHIFT] & (1ull << (add & CARDMASK));
  }
  else
    return False;
}

logical isOldRef(termPo t) {
  return t >= heap.old && t < heap.oldLimit;
}

logical isNewRef(termPo t) {
  return t >= heap.start && t < heap.curr;
}

logical isHeapRef(termPo t) {
  return t >= heap.base && t < heap.outerLimit;
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
  if (isPointer(t)) {
    assert(isNewRef(t) || isOldRef(t) || !isHeapRef(t));
  }
}

static retCode verifyScanHelper(ptrPo arg, void* c) {
  termPo t = *arg;
  validPtr(t);
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

    integer max = ((heap.oldLimit - heap.old) + CARDMASK) >> CARDSHIFT;

    for (integer ix = 0; ix < max; ix++) {
      uint64 card = heap.cards[ix];
      if (card != 0) {
        for (integer jx = 0; jx < CARDWIDTH; jx++)
          if ((card & (1ull << jx)) != 0) {
            termPo t = heap.old+(ix << CARDSHIFT)+jx;

            if (hasBuiltinType(t)) {
              builtinClassPo sClass = builtinClassOf(t);
              sClass->scanFun(sClass, verifyScanHelper, Null, t);
            }
            else {
              normalPo trm = C_NORMAL(t);
              labelPo lbl = termLbl(trm);
              for (int32 ax = 0; ax < lblArity(lbl); ax++) {
                termPo arg = trm->args[ax];
                validPtr(arg);
              }
            }
          }

      }
    }


  for (termPo t = heap.old; t < heap.oldLimit;) {
    logical hasNewRef = False;
    termPo term = t;

    if (hasBuiltinType(t)) {
      builtinClassPo sClass = builtinClassOf(t);
      t = sClass->scanFun(sClass, verifyScanHelper, &hasNewRef, t);
    }
    else {
      normalPo trm = C_NORMAL(t);
      labelPo lbl = termLbl(trm);
      for (int32 ix = 0; ix < lblArity(lbl); ix++) {
        termPo arg = trm->args[ix];
        validPtr(arg);
        hasNewRef |= isNewRef(arg);
      }
      t = t + termSize(trm);
    }

    if (hasNewRef && !hasCard(term)) {
      check(hasCard(term), "old term not in card table");
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
