//
// Created by Francis McCabe on 4/1/20.
//

#include <lower.h>
#include "jitP.h"
#include "pool.h"
#include <stdlib.h>
#include <assert.h>

static poolPo contextPool = Null;
static poolPo lblPool = Null;
static poolPo asmPool = Null;

void initJit() {
  if (contextPool == Null) {
    contextPool = newPool(sizeof(JitCompilerContext), 8);
    lblPool = newPool(sizeof(AssemLblRecord), 128);
    asmPool = newPool(sizeof(AssemCtxRecord), 128);
    initAssemX64();
  }
}

jitCompPo jitContext(methodPo mtd) {
  jitCompPo ctx = (jitCompPo) allocPool(contextPool);

  ctx->mtd = mtd;
  ctx->vTop = 0;

  return ctx;
}

static retCode clearLbl(char *lblNme, codeLblPo lbl);

assemCtxPo createCtx() {
  initJit();

  assemCtxPo ctx = (assemCtxPo) allocPool(asmPool);
  ctx->bytes = malloc(1024);
  ctx->size = 1024;
  ctx->pc = 0;
  ctx->lbls = newHash(256, (hashFun) uniHash, (compFun) uniCmp, (destFun) clearLbl);
  clearCodeCtxMaps(ctx);
  return ctx;
}

void discardCtx(assemCtxPo ctx) {
  cleanupLabels(ctx);
  if (ctx->bytes != Null) {
    free(ctx->bytes);
    ctx->bytes = Null;
  }
  freePool(asmPool, ctx);
}

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space) {
  check(jitCtx->vTop >= amnt && jitCtx->vTop<NumberOf(jitCtx->vStack)-space, "stack out of bounds");
}

codeLblPo findLabel(assemCtxPo ctx, char *lName) {
  codeLblPo lbl = (codeLblPo) hashGet(ctx->lbls, lName);
  if (lbl == Null) {
    lbl = (codeLblPo) allocPool(lblPool);
    uniCpy(lbl->nm, NumberOf(lbl->nm), lName);
    lbl->pc = -1;
    lbl->refs = Null;
  }
  return lbl;
}

retCode clearLbl(char *lblNme, codeLblPo lbl) {
  if (lbl->refs != Null) {
    eraseArray(lbl->refs);
  }
  freePool(lblPool, lbl);
  return Ok;
}

static retCode checkLbl(void *n, void *r, void *c) {
  codeLblPo lbl = (codeLblPo) r;
  if (lbl->pc >= 0) {
    if (lbl->refs != Null)
      return Error;
    else
      return Ok;
  }
  return Fail;  // undefined label
}

retCode cleanupLabels(assemCtxPo ctx) {
  if (ctx->lbls != Null) {
    eraseHash(ctx->lbls);
    ctx->lbls = Null;
  }
  return Ok;
}

typedef struct {
  assemCtxPo ctx;
  codeLblPo lbl;
} ClInfo;

static retCode updateLblEntry(void *entry, integer ix, void *cl) {
  AssemLblRefRecord *refEntry = (AssemLblRefRecord *) entry;
  ClInfo *info = (ClInfo *) cl;
  refEntry->updater(info->ctx, info->lbl, refEntry->pc);
  return Ok;
}

codeLblPo defineLabel(assemCtxPo ctx, char *lName, integer pc) {
  codeLblPo lbl = findLabel(ctx, lName);
  if (lbl == Null) {
    lbl = (codeLblPo) allocPool(lblPool);
    uniCpy(lbl->nm, NumberOf(lbl->nm), lName);
    lbl->pc = pc;
    lbl->refs = Null;
  } else {
    lbl->pc = pc;
    if (pc >= 0 && lbl->refs != Null) {
      ClInfo info = {.ctx=ctx, .lbl=lbl};
      processArrayElements(lbl->refs, updateLblEntry, &info);
      lbl->refs = eraseArray(lbl->refs);
    }
  }
  return lbl;
}

void setLabel(assemCtxPo ctx, codeLblPo lbl) {
  lbl->pc = ctx->pc;
  ClInfo info = {.ctx=ctx, .lbl=lbl};
  processArrayElements(lbl->refs, updateLblEntry, &info);
  lbl->refs = eraseArray(lbl->refs);
}

retCode addLabelReference(assemCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater) {
  check(!isLabelDefined(lbl), "label should not be defined");
  if (lbl->refs == Null) {
    lbl->refs = allocArray(sizeof(AssemLblRefRecord), 4, True);
  }
  AssemLblRefRecord ref = {.updater=updater, .pc=pc};
  return appendEntry(lbl->refs, &ref);
}

logical isLabelDefined(codeLblPo lbl) {
  return lbl->pc >= 0;
}

#define UNDEF_LBL_LANDING_PAD 0x11223344

void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  check(readCtxAtPc(ctx, pc) == UNDEF_LBL_LANDING_PAD, "bad label reference");

  integer delta = lbl->pc - (pc + 4);
  if (isI32(delta)) {
    updateU32(ctx, pc, delta);
  } else {
    check(False, "adjusted displacement too large");
  }
}

void emitLblRef(assemCtxPo ctx, codeLblPo tgt) {
  if (isLabelDefined(tgt)) {
    integer delta = tgt->pc - (ctx->pc + 4);
    if (!isI32(delta)) {
      check(False, "label displacement too large");
      return;
    }
    emitU32(ctx, delta);
  } else {
    addLabelReference(ctx, tgt, ctx->pc, labelDisp32);
    emitU32(ctx, UNDEF_LBL_LANDING_PAD);
  }
}

void emitU8(assemCtxPo ctx, uint8 byte) {
  assert(ctx->pc < ctx->size);
  ctx->bytes[ctx->pc++] = byte;
}

void emitU16(assemCtxPo ctx, uint16 word) {
  emitU8(ctx, word & 0xffu);
  emitU8(ctx, (uint8) (word >> 8u));
}

void emitU32(assemCtxPo ctx, uint32 word) {
  emitU16(ctx, word & 0xffffu);
  emitU16(ctx, (uint16) (word >> 16u));
}

void emitU64(assemCtxPo ctx, uint64 word) {
  emitU32(ctx, word & 0xffffffffu);
  emitU32(ctx, (uint32) (word >> 32u));
}

void updateU32(assemCtxPo ctx, integer pc, uint32 word) {
  ctx->bytes[pc++] = (word & 0xffu);
  ctx->bytes[pc++] = (word >> 8u) & 0xffu;
  ctx->bytes[pc++] = (word >> 16u) & 0xffu;
  ctx->bytes[pc] = (word >> 23u) & 0xffu;
}

uint32 readCtxAtPc(assemCtxPo ctx, integer pc) {
  check(pc >= 0 && pc <= ctx->pc - 4, "pc out of bounds");
  return ((unsigned) ctx->bytes[pc]) | (unsigned) (ctx->bytes[pc + 1] << 8u) | (unsigned) (ctx->bytes[pc + 2] << 16u) |
         (unsigned) (ctx->bytes[pc + 3] << 24u);
}

logical isByte(int64 x) {
  return x >= -128 && x < 128;
}

#define MAX_I32 0x7fffffffl
#define MIN_I32 (-0x80000000l)

logical isI32(int64 x) {
  return x >= MIN_I32 && x <= MAX_I32;
}
