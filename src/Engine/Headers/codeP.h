#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"
#include "pkgP.h"
#include "heapP.h"
#include "jit.h"

#include <assert.h>

typedef struct method_ {
  clssPo clss;         // == specialClass
  integer codeSize;     /* How big is the code block */
  jitCode jit;          /* Pointer to jit'ed code */
  integer entryCount;

  integer arity;        /* How many arguments in method */
  integer lclcnt;       // How many locals in the environment
  integer stackDelta;   // How much space to allocate for the stack
  normalPo pool;      /* A pool tuple of constants */
  normalPo locals;    /* A tuple of sorted locals */
  normalPo lines;      // A tuple of line information
  insPo instructions;   // a block of instructions
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

static inline integer insCount(methodPo mtd) {
  assert(mtd != Null);
  return mtd->codeSize;
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

static inline logical isPcOfMtd(methodPo mtd, insPo pc) {
  return pc >= entryPoint(mtd) && pc < entryPoint(mtd) + insCount(mtd);
}

labelPo mtdLabel(methodPo mtd);

extern retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);
extern logical validPC(methodPo mtd, insPo pc);

retCode loadPackage(packagePo p, char *errorMsg, long msgSize, void *cl);

typedef retCode (*pickupPkg)(packagePo pkg, char *errorMsg, long msgLen, void *cl);
extern retCode
installPackage(char *pkgText, long pkgTxtLen, heapPo H, char *errorMsg, long msgSize, pickupPkg pickup, void *cl);

methodPo
defineMtd(heapPo H, insPo ins, integer insCount, integer lclCount, integer stackDelta, labelPo lbl, normalPo pool,
          normalPo locals, normalPo lines);

methodPo declareMethod(const char *name, integer arity, insPo ins, integer insCount);

void showMtdCounts(ioPo out);
#endif
