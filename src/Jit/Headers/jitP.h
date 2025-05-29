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
#include "array.h"
#include "escape.h"
#include "codeP.h"
#include "asminfra.h"

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
  int32 currSPOffset;
  registerMap freeRegs;
  assemCtxPo assemCtx;
  codeLblPo entry;
  arrayPo locals;
  hashPo labels;
  char *errMsg;
  integer msgLen;
} JitCompilerContext;

assemCtxPo assemCtx(jitCompPo jitCtx);

jitCompPo jitContext(methodPo mtd, char *errMsg, integer msgLen);
void clearJitContext(jitCompPo ctx);
void clearCodeCtxMaps(assemCtxPo ctx);;

jittedCode createCode(assemCtxPo ctx);

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space);

void markEntry(jitCompPo jit, codeLblPo entry);
codeLblPo jitEntry(jitCompPo jit);

retCode reserveReg(jitCompPo jit, armReg rg);
armReg findFreeReg(jitCompPo jit);
void releaseReg(jitCompPo jit, armReg rg);

integer allocateLocal(jitCompPo jit, integer id, integer offset, localVarState state);
integer findLocalOffset(jitCompPo jit, integer id);
integer cancelLocal(jitCompPo jit, integer id);

codeLblPo defineJitLbl(jitCompPo jit, insPo pc);
codeLblPo newJitLbl(jitCompPo jit, insPo pc);
codeLblPo getJitLbl(jitCompPo jit, insPo pc);

typedef struct lbl_ref {
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

logical isByte(int64 x);
logical isI32(int64 x);

retCode jitInstructions(jitCompPo jitCtx, methodPo mtd, char *errMsg, integer msgLen);


#endif //STAR_JITP_H
