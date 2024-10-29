#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"
#include "heapP.h"
#include "jit.h"
#include "array.h"

#include <assert.h>

typedef struct instruction_ {
  OpCode op;
  int32 fst;            // First integer operand
  int32 alt;            // but some may also have a second operand
} Instruction;

typedef struct methodLoc_ {
  int32 pc;
  int32 litNo;
} MethodLoc, *methodLocPo;

typedef struct method_ {
  clssPo clss;          // == specialClass
  jitCode jit;          /* Pointer to jit'ed code */
  integer entryCount;
  integer sigIx;        // Index of the function signature literal
  integer arity;        /* How many arguments in method */
  integer lclcnt;       // How many locals in the environment
  integer stackDelta;   // How much space to allocate for the stack
  normalPo pool;        /* A pool tuple of constants */
  arrayPo locs;         // Sorted array of location information
  integer insCount;     // How many instructions are there in the code?
  insPo instructions;   // The actual instructions
} MethodRec;

extern clssPo methodClass;

#define MtdCellCount CellCount(sizeof(MethodRec))

// These are needed during GC
extern methodPo haltMethod;
methodPo underflowMethod;
methodPo newFiberMethod;
methodPo newTaskMethod;
methodPo spawnMethod;

static inline logical isMethod(termPo m) {
  return hasClass(m, methodClass);
}

static inline insPo entryPoint(methodPo mtd) {
  assert(mtd != Null);
  return mtd->instructions;
}

static inline int64 argCount(methodPo mtd) {
  assert(mtd != Null);
  return mtd->arity;
}

static inline void incEntryCount(methodPo mtd) {
  mtd->entryCount++;
}

static inline logical hasJit(methodPo mtd) {
  assert(mtd != Null);
  return mtd->jit != Null;
}

static inline jitCode codeJit(methodPo mtd) {
  assert(mtd != Null && mtd->jit != Null);
  return mtd->jit;
}

retCode setJitCode(methodPo mtd, jitCode code);

labelPo mtdLabel(methodPo mtd);

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

methodPo
defineMtd(heapPo H, integer insCount, insPo instructions, integer funSigIx, integer lclCount, integer stackHeight,
          labelPo lbl, normalPo pool, arrayPo locs);

methodPo
specialMethod(const char *name, integer arity, integer insCount, insPo instructions, termPo sigTerm, integer lclCount);

void showMtdCounts(ioPo out);

#endif
