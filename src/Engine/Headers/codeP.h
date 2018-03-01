#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"
#include "pkgP.h"

#include <assert.h>

typedef struct _pkg_record_ {
  PackageRec pkg;
  hashPo methods;
} PkgRec;

typedef struct _method_ {
  clssPo clss;         // == methodClass
  insPo code;         /* Pointer to the code */
  int64 codeSize;     /* How big is the code block */
  jitCode jit;        /* Pointer to jit'ed code */
  long jitSize;       /* How big is the Jit code? */

  int64 arity;        /* How many arguments in method */
  normalPo pool;      /* A pool tuple of constants */
  normalPo locals;    /* A tuple of sorted locals */
} MethodRec;

extern clssPo methodClass;

static inline logical isMethod(termPo m) {
  return hasClass(m, methodClass);
}

static inline methodPo clMethod(termPo cl) {
  assert(isMethod(cl));
  return (methodPo) cl;
}

static inline insPo entryPoint(methodPo mtd) {
  return mtd->code;
}

static inline int64 argCount(methodPo cl) {
  return cl->arity;
}

static inline normalPo codeLits(methodPo mtd) {
  return mtd->pool;
}

extern retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

extern void markMtd(heapPo h, methodPo mtd);
#endif
