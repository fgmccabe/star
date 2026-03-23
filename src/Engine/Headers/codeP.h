#ifndef CODE_P_H_
#define CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "array.h"
#include "code.h"
#include "heapP.h"
#include "jit.h"
#include "labelsP.h"
#include "termP.h"
#include "ssaOps.h"

typedef struct instruction_ {
  union {
    ssaOp op;
    int32 ltrl;
  } op;
} Instruction;

typedef struct method_ {
  TermHead clss;    // == specialClass
#ifndef NOJIT
  CodeBlock jit;      // Jit'ed code
#endif
  labelPo lbl;        // The label of this code
  int32 lclcnt;       // How many locals in the method
  int32 stackDelta;   // Maximum depth of active locals
  int32 insCount;     // How many instructions are there in the code?
  ssaInsPo instructions; // The actual instructions
} MethodRec;

extern int32 methodIndex;

#define MtdCellCount CellCount(sizeof(MethodRec))

static inline logical isMethod(termPo m) { return hasIndex(m, methodIndex); }

static inline ssaInsPo entryPoint(methodPo mtd) {
  assert(isMethod((termPo) mtd));
  return mtd->instructions;
}

static inline int32 argCount(methodPo mtd) {
  assert(mtd != Null);
  return lblArity(mtd->lbl);
}

#ifndef NOJIT
static inline logical hasJit(methodPo mtd) {
  assert(mtd != Null);
  return mtd->jit.code != Null;
}

static inline jittedCode jitCode(methodPo mtd) {
  assert(mtd != Null && mtd->jit.code != Null);
  return mtd->jit.code;
}

retCode setJitCode(methodPo mtd, jittedCode code, uint32 codeSize);
#endif

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

methodPo defineMtd(heapPo H, int32 insCount, ssaInsPo instructions, int32 lclCount, int32 stackDelta, labelPo lbl);

labelPo specialMethod(const char *name, int32 arity, int32 insCx,
                      ssaInsPo instructions, int32 lcls, int32 stkLimit);

void markMethod(methodPo mtd, gcSupportPo G);

void showMethodCode(ioPo out, char *msg, methodPo mtd);
#endif
