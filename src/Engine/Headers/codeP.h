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
  termPo data;      /* The constant itself */
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
  Class clss;         // == methodClass
  insPo code;         /* Pointer to the code */
  int64 codeSize;     /* How big is the code block */
  jitCode jit;        /* Pointer to jit'ed code */
  long jitSize;       /* How big is the Jit code? */

  int64 typeSig;      /* Which constant has the type signature? */
  int64 arity;        /* How many arguments in method */
  int64 poolCount;    /* Size of constants in pool */
  int64 frameCount;   /* Number of frame records */
  framePtr frames;    /* frame pointers */
  localPtr locals;    /* sorted locals */
  int64 localCount;   /* Number of local var records */
  ConstantRec pool[ZEROARRAYSIZE];  /* Pool of constants */
} MethodRec;

extern clssPo methodClass;

static inline logical isMethod(termPo m) {
  return hasClass(m, methodClass);
}

static inline methodPo clMethod(termPo cl) {
  assert(isMethod(cl));
  return (methodPo) cl;
}

static inline constantPo codeLiterals(methodPo cl) {
  assert(isMethod((termPo)cl));
  return cl->pool;
}

static inline insPo entryPoint(methodPo mtd) {
  return mtd->code;
}

static inline int64 argCount(termPo cl) {
  assert(isMethod(cl));
  return clMethod(cl)->arity;
}

static inline char *mtdSignature(methodPo mtd) {
  return (char *) mtd->pool[mtd->typeSig].data;
}

extern retCode showMtdLbl(ioPo f, void *data, long depth, long precision, logical alt);

#endif
