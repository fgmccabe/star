#ifndef _HEAP_P_H_
#define _HEAP_P_H_

#include <term.h>
#include "heap.h"
#include "engine.h"
#include "termP.h"

// Are we allocating from the lower or upper half?
typedef enum {
  lowerPhase1, upperPhase1, lowerPhase2, upperPhase2
} allocMode;

static char *allocModeNames[] = {"lP1", "uP1", "lP2","uP2"};

#ifndef MAX_ROOT
#define MAX_ROOT 128
#endif

/* Used for recording old->new pointers */
#ifndef CARDSHIFT
#define CARDSHIFT 6u		/* 2>>6 bits in an integer */
#define CARDWIDTH  (1u<<CARDSHIFT)
#define CARDMASK  (CARDWIDTH-1u)
#endif

typedef uint64 cardMap;

/*
 * | base                                                              outerLimit |
 * | old .... oldLimit                 split                                      |
 * |          start    ... curr(a) ... limit(a)
 * |                                              curr(b) ... limit(b)
 */

typedef struct heap_ {
  termPo base;
  termPo outerLimit; /* The real */
  termPo split;

  termPo start;
  termPo curr;
  termPo limit;

  allocMode allocMode;

  termPo old;      // base of the old term space (== start)
  termPo oldLimit; // Current end of old term space.

  cardMap* cards; /* this is a table of cards */
  long ncards;

  ptrPo roots[MAX_ROOT];
  int topRoot;
  enginePo owner;
} HeapRecord, *heapPo;

extern HeapRecord heap;

typedef struct stack_frame_* framePo;

extern long initHeapSize; /* How much memory to give the heap */
extern long maxHeapSize;  // Maximum permitted size of heap

logical isHeapRef(termPo t);
logical isNewRef(termPo t);
logical isOldRef(termPo t);
void recordTermUpdate(termPo t);
logical hasCard(termPo t);

#ifdef TRACEMEM
extern tracingLevel traceMemory; /* memory tracing */
extern logical validateMemory;   // Validate heap after every allocation
extern logical traceAllocs;      // trace allocations
void showMemoryStats(ioPo out);  // Show memory statistics
#endif

retCode gcCollect(long amount);

typedef struct gc_support_ {
  long oCnt;
  termPo oldBase;
  termPo oldLimit;
  termPo oldOld;
  termPo oldOldLimit;
} GCSupport, *gcSupportPo;

extern void setupGCSupport(gcSupportPo G);

extern void validPtr(termPo t);
extern void verifyHeap(void);

extern termPo markPtr(gcSupportPo G, ptrPo p);

static inline logical inHeap(const termPo x) {
  return (logical)((x >= heap.base && x < heap.curr) || (x >= heap.old && x < heap.oldLimit));
}

void heapSummary(ioPo out);

extern void dumpGcStats(ioPo out);

#endif
