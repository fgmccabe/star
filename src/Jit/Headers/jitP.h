//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JITP_H
#define STAR_JITP_H

#include <config.h>
#include "jit.h"
#include "ooio.h"
#include "array.h"
#include "macros.h"
#include "escape.h"
#include "codeP.h"
#include "infra.h"

#define MAX_VSTACK 256

typedef enum {
  int64Tp,
  fltTp,
  ptrTp
} lType;

typedef enum {
  argument,
  local,
  constant,
  mcReg,
  global,
  engineSymbol,
  stackRelative,
  noWhere
} srcLoc;

typedef struct {
  lType type;
  srcLoc loc;
  int64 ix;
  registerSpec mcLoc;
  void *address;
  escapePo escape;
} vOperand, *operandPo;

typedef enum {
  localVar,
  spilledVar,
  emptyVar
} localVarState;

typedef struct localSpec {
  integer offset;
  integer id;
  localVarState state;
} LocalRecord, *localPo;

typedef struct labelMarker {
  insPo pc;
  codeLblPo lbl;
} LabelMarkerRecord, *labelMarkerPo;

typedef struct jit_compiler_ {
  methodPo mtd;
  registerMap freeRegs;
  assemCtxPo assemCtx;
  int32 minOffset;
  int32 maxOffset;
  int32 stackDepth;
  char errMsg[MAXLINE];
} JitCompilerContext;

logical jitStackHasRoom(jitCompPo jit, int32 amnt);
int32 jitStackDepth(jitCompPo jit);
int32 jitTrueStackDepth(jitCompPo jit);

assemCtxPo assemCtx(jitCompPo jitCtx);

jitCompPo jitContext(methodPo mtd);
void clearJitContext(jitCompPo ctx);
void clearCodeCtxMaps(assemCtxPo ctx);;

jittedCode createCode(assemCtxPo ctx);

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space);

retCode reserveReg(jitCompPo jit, armReg rg);
armReg findFreeReg(jitCompPo jit);
void releaseReg(jitCompPo jit, armReg rg);

typedef struct lbl_ref {
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

logical isByte(int64 x);
logical isI32(int64 x);

retCode jitInstructions(jitCompPo jitCtx, methodPo mtd, char *errMsg, integer msgLen);

#endif //STAR_JITP_H
