#ifndef _HEAP_P_H_
#define _HEAP_P_H_

#include "heap.h"

// Are we allocating from the lower or upper half?
typedef enum {
  lowerHalf, upperHalf
} AllocMode;

typedef struct _heap_ {
  void *start;
  void *curr;
  void *limit;
  void *outerLimit;			/* The real */
  void *base;
  void *old;
  AllocMode allocMode;
} HeapRecord;

extern HeapRecord heap, oldHeap;

typedef struct _closure_ *envPo;
typedef struct _stack_frame_ *framePo;
typedef void (*lScanFun)(framePo fp);	 /* locals scanner function */
typedef envPo (*scavengePo)(envPo clos); /* A scavenger function */

// This is pretty machine specific to the x86-64

typedef struct _scan_table_ *scanTablePo;
typedef struct _scan_table_ {
  void *rtn;				/* code address */
  lScanFun scanner;
} ScanTableEntry;

typedef struct _code_structure_ {
  scanTablePo scanners;
  scavengePo scavenger;
  scavengePo evacuator;
} CodeStructure, *codeStructPo;

typedef struct _closure_ {
  codeStructPo code;
} Closure;

extern void collect(framePo stack,void *site,int amnt);

// Change these if HeapRecord is changed
#define H_CURR_OFFSET (sizeof(void*))
#define H_LIMIT_OFFSET (sizeof(void*))

#define AddressOf(tp,field) ((long)(void*)(&((tp*)0)->field))

#endif
