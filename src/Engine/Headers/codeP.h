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

typedef union ssa_instruction_ {
  ssaOp op;
  int32 operand;
} SSAInstruction;

typedef struct method_ {
  ClassRecord clss; // == specialClass
#ifndef NOJIT
  CodeBlock jit; // Jit'ed code
#endif
  labelPo lbl; // The label of this code
  int32 lclcnt; // How many locals in the environment
  int32 stackDelta; // How much space to allocate for the stack
  int32 insCount; // How many instructions are there in the code?
  ssaInsPo instructions; // The actual instructions
} MethodRec;

extern clssPo methodClass;

#define MtdCellCount CellCount(sizeof(MethodRec))

static inline logical isMethod(termPo m) { return hasClass(m, methodClass); }

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

logical validPC(methodPo mtd, ssaInsPo pc);
int32 codeOffset(methodPo mtd, ssaInsPo pc);

int32 opCount(ssaInsPo pc);
int32 operand(ssaInsPo pc, int32 ix);

retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

methodPo defineMtd(heapPo H, int32 insCount, ssaInsPo instructions, int32 lclCount, int32 stackLimit, labelPo lbl);

labelPo specialMethod(const char *name, int32 arity, int32 insCx,
                      ssaInsPo instructions, int32 lcls, int32 delta, int32 stackEntry);

void markMethod(methodPo mtd, gcSupportPo G);
#endif
