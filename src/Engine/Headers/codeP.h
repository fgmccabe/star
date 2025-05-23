#ifndef CODE_P_H_
#define CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"
#include "heapP.h"
#include "jit.h"
#include "array.h"
#include "labelsP.h"

typedef struct instruction_ {
  OpCode op;
  int32 fst;            // First integer operand
  int32 alt;            // but some may also have a second operand
} Instruction;

typedef struct method_ {
  ClassRecord clss;     // == specialClass
  jitCode jit;          // Jit'ed code
  integer entryCount;
  labelPo lbl;          // The label of this code
  int32 lclcnt;         // How many locals in the environment
  int32 stackDelta;     // How much space to allocate for the stack
  termPo locs;          // Sorted array of location information
  int32 insCount;       // How many instructions are there in the code?
  insPo instructions;   // The actual instructions
} MethodRec;

extern clssPo methodClass;

#define MtdCellCount CellCount(sizeof(MethodRec))

static inline logical isMethod(termPo m) {
  return hasClass(m, methodClass);
}

static inline insPo entryPoint(methodPo mtd) {
  assert(mtd != Null);
  return mtd->instructions;
}

static inline int32 argCount(methodPo mtd) {
  assert(mtd != Null);
  return lblArity(mtd->lbl);
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

static inline int32 lclCount(methodPo mtd) {
  return mtd->lclcnt;
}

retCode setJitCode(methodPo mtd, jitCode code);

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

methodPo
defineMtd(heapPo H, int32 insCount, insPo instructions, int32 lclCount, int32 stackHeight, labelPo lbl, termPo locs);

labelPo specialMethod(const char *name, int32 arity, int32 insCx, insPo instructions, termPo sigTerm, int32 lcls);

void showMtdCounts(ioPo out);
#endif
