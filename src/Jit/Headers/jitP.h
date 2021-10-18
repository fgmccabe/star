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

#define MAX_VSTACK 256

typedef uint64 registerMap;

typedef enum {
  int64Tp,
  fltTp,
  ptrTp
} lType;

typedef enum {
  argument,
  local,
  literal,
  immediate,
  mcReg
} srcLoc;

typedef struct {
  lType type;
  srcLoc loc;
  int64 ix;
  registerSpec mcLoc;
} vOperand;

typedef struct assem_ctx {
  unsigned char *bytes;
  uint32 size;
  uint32 pc;
  arrayPo lbls;
  registerMap usedRegs;
  registerMap freeRegs;
  jitCompPo jitCxt;
} AssemCtxRecord;

typedef struct jit_compiler_ {
  methodPo mtd;
  integer vTop;
  vOperand vStack[MAX_VSTACK];
  assemCtxPo assemCtx;
} JitCompilerContext;

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
void clearCodeCtxMaps(assemCtxPo ctx);;

void initAssemX64();
assemCtxPo createCtx();
void discardCtx(assemCtxPo ctx);
void *createCode(assemCtxPo ctx);

codeLblPo defineLabel(assemCtxPo ctx, char *lName, integer pc);
void setLabel(assemCtxPo ctx, codeLblPo lbl);
logical isLabelDefined(codeLblPo lbl);
retCode cleanupLabels(assemCtxPo ctx);

typedef void (*lblRefUpdater)(assemCtxPo ctx, codeLblPo lbl, integer pc);
retCode addLabelReference(assemCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater);
static retCode updateLblEntry(void *entry, integer ix, void *cl);
void emitLblRef(assemCtxPo ctx, codeLblPo tgt);
void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc);

typedef struct lbl_ref {
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

logical isByte(int64 x);
logical isI32(int64 x);

retCode jit_preamble(methodPo mtd, jitCompPo ctx);

retCode jit_postamble(methodPo mtd, jitCompPo ctx);

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space);

#endif //STAR_JITP_H
