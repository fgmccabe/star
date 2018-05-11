#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"
#include "pkgP.h"
#include "heapP.h"

#include <assert.h>

typedef struct _method_ {
  clssPo clss;         // == specialClass
  integer codeSize;     /* How big is the code block */
  jitCode jit;        /* Pointer to jit'ed code */
  integer jitSize;       /* How big is the Jit code? */

  integer arity;        /* How many arguments in method */
  integer lclcnt;       // How many locals in the environment
  normalPo pool;      /* A pool tuple of constants */
  normalPo locals;    /* A tuple of sorted locals */
  insWord code[ZEROARRAYSIZE];
} MethodRec;

extern clssPo methodClass;

#define MtdCellCount(count) (CellCount(sizeof(MethodRec))+CellCount((count)*sizeof(insWord)))

static inline logical isMethod(termPo m) {
  return hasClass(m, methodClass);
}

static inline insPo entryPoint(methodPo mtd) {
  assert(mtd != Null);
  return &mtd->code[0];
}

static inline integer insCount(methodPo mtd) {
  assert(mtd != Null);
  return mtd->codeSize;
}

static inline int64 argCount(methodPo mtd) {
  assert(mtd != Null);
  return mtd->arity;
}

extern retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);
extern logical validPC(methodPo mtd, insPo pc);

extern void markMtd(gcSupportPo G, methodPo mtd);

retCode loadPackage(packagePo p, char *errorMsg, long msgSize, void *cl);

typedef retCode (*pickupPkg)(packagePo pkg, char *errorMsg, long msgLen, void *cl);
extern retCode installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl);
#endif
