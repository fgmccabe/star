#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include "code.h"
#include "termP.h"

#include <assert.h>

typedef struct _constant_ {
  char *sig;        /* The type signature of the constant */
  void *data;        /* The constant data */
} ConstantRec;

typedef struct _frame_ {
  int64 sig;        /* pool constant for signature */
  int64 pc;        /* program counter for frame */
} FrameRec, *framePtr;

typedef struct _local_ {
  int64 name;       /* pool constant for name */
  int64 sig;        /* pool constant for signature */
  int64 off;        /* which local is this? */
  int64 from;       /* start of valid range */
  int64 to;         /* end of valid range */
} LocalRec, *localPtr;

typedef struct _method_ {
  Term classPart;  // == methodClass
  insPo code;        /* Pointer to the code */
  int64 codeSize;      /* How big is the code block */
  jitCode jit;        /* Pointer to jit'ed code */
  long jitSize;        /* How big is the Jit code? */

  int64 typeSig;      /* Which constant has the type signature? */
  int64 freeSig;      /* which has the free signature? */
  int64 arity;        /* How many argument bytes in method */
  int64 freeCount;      /* Number of 64 bit free words */
  int64 poolCount;      /* Size of constants in pool */
  int64 frameCount;      /* Number of frame records */
  framePtr frames;      /* frame pointers */
  localPtr locals;      /* sorted locals */
  int64 localCount;      /* Number of local var records */
  ConstantRec pool[ZEROARRAYSIZE];  /* Pool of constants */
} MethodRec;

extern clssPo methodClass;

static inline logical isMethodClass(termPo m) {
  return hasClass(m, methodClass);
}

typedef struct _closureRec_ {
  termPo sig;                  // The class of the sig is methodClass
  integer free[ZEROARRAYSIZE];    /* Free variables for this closure */
} ClosureRec;

static inline logical isValidClosure(termPo o) {
  return isMethodClass(o->sig);
}

static inline methodPo clMethod(closurePo cl) {
  assert(isMethodClass(cl->sig));
  return (methodPo) cl->sig;
}

static inline constantPo codeLiterals(closurePo cl) {
  assert(isMethodClass(cl->sig));
  return &clMethod(cl)->pool[0];
}

static inline insPo entryPoint(closurePo cl) {
  assert(isMethodClass(cl->sig));
  return clMethod(cl)->code;
}

static inline int64 argCount(closurePo cl) {
  assert(isMethodClass(cl->sig));
  return clMethod(cl)->arity;
}

static inline int64 freeCount(closurePo cl) {
  assert(isMethodClass(cl->sig));
  return clMethod(cl)->freeCount;
}

static inline char *mtdSignature(methodPo mtd) {
  return (char *) mtd->pool[mtd->typeSig].data;
}

static inline char *mtdFreeSignature(methodPo mtd) {
  return (char *) mtd->pool[mtd->freeSig].data;
}

#endif
