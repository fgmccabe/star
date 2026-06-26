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
#include "pkg.h"
#include "termP.h"
#include "ssaOps.h"

typedef struct instruction_ {
  union {
    ssaOp op;
    int32 ltrl;
  } op;
} Instruction;

typedef struct codeLocation_ {
  int32 offset;
  packagePo pkg;
  int32 line;
  int32 col;
  int32 from;
  int32 size;
} CodeLocation, *codeLocationPo;

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
  arrayPo locations;
} MethodRec;

extern int32 methodIndex;

#define MtdCellCount CellCount(sizeof(MethodRec))

static inline logical isMethod(termPo m) { return hasIndex(m, methodIndex); }

static inline ssaInsPo entryPoint(methodPo mtd) {
  assert(isMethod((termPo) mtd));
  return mtd->instructions;
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
void recordMethodJitCode(methodPo mtd);

#endif

void rebalanceCodeTree();
retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

methodPo defineMtd(int32 insCount, ssaInsPo instructions, int32 lclCount, int32 stackDelta, labelPo lbl);

labelPo specialMethod(const char *name, int32 arity, int32 insCx,
                      ssaInsPo instructions, int32 lcls, int32 stkLimit);

void recordMethodCode(methodPo mtd);
methodPo locateMethod(uinteger pc);

void markMethod(methodPo mtd, gcSupportPo G);

void showMethodCode(ioPo out, char *msg, methodPo mtd);

void recordMethodLocation(methodPo mtd, termPo loc, uint32 offset);

codeLocationPo locateMethodLocation(methodPo mtd, uinteger offset);

retCode scanMethod(methodPo mtd, termHelper helper, void *cl);
#endif
