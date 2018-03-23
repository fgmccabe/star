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

typedef struct _pkg_record_ {
  PackageRec pkg;
  hashPo methods;
} PkgRec;

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

static inline methodPo clMethod(termPo cl) {
  assert(isMethod(cl));
  return (methodPo) cl;
}

static inline insPo entryPoint(methodPo mtd) {
  return &mtd->code[0];
}

static inline int64 argCount(methodPo cl) {
  return cl->arity;
}

extern retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

extern void markMtd(gcSupportPo G, methodPo mtd);

retCode loadPackage(char *pkg, char *vers, char *errorMsg, long msgSize, void *cl);

typedef retCode (*pickupPkg)(char *pkgNm, char *vers, char *errorMsg, long msgLen, void *cl);
extern retCode installPackage(char *pkgText, long pkgTxtLen, char *errorMsg, long msgSize, pickupPkg pickup, void *cl);
#endif
