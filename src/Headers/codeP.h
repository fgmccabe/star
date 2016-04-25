#ifndef _CODE_P_H_
#define _CODE_P_H_

/*
 * Private header for the code format structure
 */

#include <ooio.h>
#include "code.h"
#include "opcodes.h"

typedef struct _constant_ {
  uniChar *sig;				/* The type signature of the constant */
  void *data;				/* The constant data */
} ConstantRec;

typedef struct _frame_ {
  int32 sig;				/* pool constant for signature */
  int32 pc;				/* program counter for frame */
} FrameRec, *framePtr;

typedef struct _local_ {
  int32 name;				/* pool constant for name */
  int32 sig;				/* pool constant for signature */
  int32 off;				/* which local is this? */
  int32 from;				/* start of valid range */
  int32 to;				/* end of valid range */
} LocalRec, *localPtr;

typedef struct _method_ {
  insPo code;				/* Pointer to the code */
  long codeSize;			/* How big is the code block */
  jitCode jit;				/* Pointer to jit'ed code */
  long jitSize;				/* How big is the Jit code? */

  int32 typeSig;      /* Which constant has the type signature? */
  int32 freeSig;      /* which has the free signature? */
  int32 arity;				/* How many argument bytes in method */
  int32 freeCount;			/* Number of 64 bit free words */
  int32 poolCount;			/* Size of constants in pool */
  int32 frameCount;			/* Number of frame records */
  framePtr frames;			/* frame pointers */
  localPtr locals;			/* sorted locals */
  int32 localCount;			/* Number of local var records */
  ConstantRec pool[ZEROARRAYSIZE];	/* Pool of constants */
} MethodRec;

typedef struct _closureRec_ {
  methodPo code;			/* The code for this closure */
  uint64 free[ZEROARRAYSIZE];		/* Free variables for this closure */
} ClosureRec;

static inline methodPo clMethod(closurePo cl)
{
  return cl->code;
}

static inline constantPo codeLiterals(closurePo cl)
{
  return &cl->code->pool[0];
}

static inline insPo entryPoint(closurePo cl)
{
  return cl->code->code;
}

static inline int32 argCount(closurePo cl)
{
  return cl->code->arity;
}

static inline int32 freeCount(closurePo cl)
{
  return cl->code->freeCount;
}

static inline uniChar* mtdSignature(methodPo mtd)
{
  return (uniChar*)mtd->pool[mtd->typeSig].data;
}

static inline uniChar* mtdFreeSignature(methodPo mtd)
{
  return (uniChar*)mtd->pool[mtd->freeSig].data;
}

#endif
