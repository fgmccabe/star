//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JITP_H
#define STAR_JITP_H

#include <config.h>
#include "jit.h"
#include "ooio.h"
#include "array.h"
#include "lower.h"
#include "macros.h"
#include "array.h"
#include "escape.h"

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

typedef struct assem_ctx {
  unsigned char *bytes;
  uint32 size;
  uint32 pc;
  arrayPo lbls;
} AssemCtxRecord;

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
  integer vTop;
  vOperand vStack[MAX_VSTACK];
  registerMap usedRegs;
  registerMap freeRegs;
  assemCtxPo assemCtx;
  codeLblPo entry;
  arrayPo locals;
  arrayPo labels;
} JitCompilerContext;

assemCtxPo assemCtx(jitCompPo jitCtx);

typedef struct assem_lbl {
  arrayPo refs;
  integer pc;
} AssemLblRecord;

void emitU8(assemCtxPo ctx, uint8 byte);
void emitU16(assemCtxPo ctx, uint16 word);
void emitU32(assemCtxPo ctx, uint32 word);
void emitU64(assemCtxPo ctx, uint64 word);
void updateU32(assemCtxPo ctx, integer pc, uint32 word);
uint32 readCtxAtPc(assemCtxPo ctx, integer pc);

jitCompPo jitContext(methodPo mtd);
void clearJitContext(jitCompPo ctx);
void clearCodeCtxMaps(assemCtxPo ctx);;

void initAssem();
assemCtxPo createCtx();
void discardCtx(assemCtxPo ctx);
jitCode createCode(assemCtxPo ctx);

void markEntry(jitCompPo jit, codeLblPo entry);
codeLblPo jitEntry(jitCompPo jit);

extern integer undefinedPc;

int32 collectOperand(insPo base, integer *pc);
insPo collectTgt(insPo base, integer *pc);

retCode sortLabels(jitCompPo jit);

armReg findFreeReg(jitCompPo jit);
void releaseReg(jitCompPo jit, armReg rg);

integer allocateLocal(jitCompPo jit, integer id, integer offset, localVarState state);
integer findLocalOffset(jitCompPo jit, integer id);
integer cancelLocal(jitCompPo jit, integer id);

void collectLblTgt(insPo pc, jitCompPo jit);
retCode sortLabels(jitCompPo jit);
retCode resolvePcLbl(insPo code, integer pc, jitCompPo jit, char *errMsg, integer msgLen);
codeLblPo getLblByPc(insPo pc, jitCompPo jit);

uint32 currentPc(assemCtxPo ctx);

typedef void (*lblRefUpdater)(assemCtxPo ctx, codeLblPo lbl, integer pc);
retCode addLabelReference(assemCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater);
static retCode updateLblEntry(void *entry, integer ix, void *cl);
integer lblDeltaRef(assemCtxPo ctx, codeLblPo tgt);
void emitLblRef(assemCtxPo ctx, codeLblPo tgt);
void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc);

typedef struct lbl_ref {
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

logical isByte(int64 x);
logical isI32(int64 x);

retCode jit_preamble(methodPo mtd, jitCompPo jit);

retCode jit_postamble(methodPo mtd, jitCompPo ctx);

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space);

retCode nextOperand(insPo code, integer *pc, opAndSpec spec, jitCompPo jit, char *errMsg, integer msgSize);

#endif //STAR_JITP_H
