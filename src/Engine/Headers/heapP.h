#ifndef _HEAP_P_H_
#define _HEAP_P_H_

#include <term.h>
#include "heap.h"
#include "engine.h"
#include "termP.h"

// Are we allocating from the lower or upper half?
typedef enum {
  lowerHalf, upperHalf
} allocMode;

#ifndef MAX_ROOT
#define MAX_ROOT 128
#endif

/* Used for recording old->new pointers */
#ifndef CARDSHIFT
#define CARDSHIFT 6		/* 64 bits in an integer */
#define CARDWIDTH  (1<<CARDSHIFT)
#define CARDMASK  (CARDWIDTH-1)
#endif

typedef uint64 cardMap;

extern cardMap masks[CARDWIDTH];

/*
 * | base                                                              outerLimit |
 * | old .... oldLimit                 split                                      |
 * |          start    ... curr(a) ... limit(a)
 * |                                              curr(b) ... limit(b)
 */

typedef struct heap_ {
  termPo base;
  termPo outerLimit; /* The real */

  termPo start;
  termPo split;

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

retCode setupHeap(heapPo heap, int64 cellCount);

void recordTermUpdate(termPo t);

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
} GCSupport, *gcSupportPo;

extern void setupGCSupport(gcSupportPo G);

extern void validPtr(termPo t);
extern void verifyHeap(void);

extern termPo markPtr(gcSupportPo G, ptrPo p);
extern termPo scanTerm(gcSupportPo G, termPo x);

static inline logical inHeap(const termPo x) {
  return (logical)((x >= heap.base && x < heap.curr) || (x >= heap.old && x < heap.oldLimit));
}

extern void lockHeap(heapPo H);
extern void releaseHeapLock(heapPo H);

extern retCode heapSummary(ioPo out, heapPo H);

extern void dumpGcStats(ioPo out);

#endif
