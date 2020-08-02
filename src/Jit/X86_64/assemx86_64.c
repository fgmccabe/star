/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include <utils.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>

#include "x86_64P.h"

poolPo ctxPool = Null;
poolPo lblPool = Null;

void initAssemX64() {
  if (ctxPool == Null) {
    ctxPool = newPool(sizeof(AssemCtxRecord), 128);
    lblPool = newPool(sizeof(AssemLblRecord), 128);
  }
}

static retCode clearLbl(char *lblNme, x64LblPo lbl);
static retCode cleanupLabels(x64CtxPo ctx);

x64CtxPo createCtx() {
  initAssemX64();
  x64CtxPo ctx = (x64CtxPo) allocPool(ctxPool);
  ctx->bytes = malloc(1024);
  ctx->size = 1024;
  ctx->pc = 0;
  ctx->lbls = newHash(256, (hashFun) uniHash, (compFun) uniCmp, (destFun) clearLbl);
  return ctx;
}

void discardCtx(x64CtxPo ctx) {
  cleanupLabels(ctx);
  free(ctx->bytes);
  ctx->bytes = Null;
  freePool(ctxPool, ctx);
}

void *createCode(x64CtxPo ctx){
  cleanupLabels(ctx);
  void *code = mmap(Null,ctx->pc,PROT_EXEC|PROT_READ|PROT_WRITE,MAP_ANON|MAP_PRIVATE,-1,0);
  memcpy(code,ctx->bytes,ctx->pc);
  free(ctx->bytes);
  freePool(ctxPool,ctx);
  return code;
}

x64LblPo preamble(x64CtxPo ctx, i32 lclCount) {
  x64LblPo entry = defineLabel(ctx,"entry",ctx->pc);
  push(RG(RBP),ctx);
  mov(RG(RBP),RG(RSP),ctx);
  sub(RG(RSP),IM(lclCount),ctx);
  return entry;
}

retCode postamble(x64CtxPo ctx){
  mov(RG(RSP),RG(RBP),ctx);
  pop(RG(RBP),ctx);
  rtn(ctx);
  return Ok;
}

x64LblPo findLabel(x64CtxPo ctx, char *lName) {
  x64LblPo lbl = (x64LblPo) hashGet(ctx->lbls, lName);
  if (lbl == Null) {
    lbl = (x64LblPo) allocPool(lblPool);
    uniCpy(lbl->nm, NumberOf(lbl->nm), lName);
    lbl->pc = -1;
    lbl->refs = Null;
  }
  return lbl;
}

retCode clearLbl(char *lblNme, x64LblPo lbl) {
  if (lbl->refs != Null) {
    eraseArray(lbl->refs);
  }
  freePool(lblPool, lbl);
  return Ok;
}

static retCode checkLbl(void *n, void *r, void *c) {
  x64LblPo lbl = (x64LblPo) r;
  if (lbl->pc >= 0) {
    if (lbl->refs != Null)
      return Error;
    else
      return Ok;
  }
  return Fail;  // undefined label
}

retCode cleanupLabels(x64CtxPo ctx) {
  if (ctx->lbls != Null) {
    eraseHash(ctx->lbls);
    ctx->lbls = Null;
  }
  return Ok;
}

#define UNDEF_LBL_LANDING_PAD 0x11223344

typedef struct {
  x64CtxPo ctx;
  x64LblPo lbl;
} ClInfo;

static retCode updateLblEntry(void *entry, integer ix, void *cl) {
  AssemLblRefRecord *refEntry = (AssemLblRefRecord *) entry;
  ClInfo *info = (ClInfo *) cl;
  refEntry->updater(info->ctx, info->lbl, refEntry->pc);
  return Ok;
}

x64LblPo defineLabel(x64CtxPo ctx, char *lName, integer pc) {
  x64LblPo lbl = findLabel(ctx, lName);
  if (lbl == Null) {
    lbl = (x64LblPo) allocPool(lblPool);
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

void setLabel(x64CtxPo ctx, x64LblPo lbl) {
  lbl->pc = ctx->pc;
  ClInfo info = {.ctx=ctx, .lbl=lbl};
  processArrayElements(lbl->refs, updateLblEntry, &info);
  lbl->refs = eraseArray(lbl->refs);
}

retCode addLabelReference(x64CtxPo ctx, x64LblPo lbl, integer pc, lblRefUpdater updater) {
  check(!isLabelDefined(lbl), "label should not be defined");
  if (lbl->refs == Null) {
    lbl->refs = allocArray(sizeof(AssemLblRefRecord), 4, True);
  }
  AssemLblRefRecord ref = {.updater=updater, .pc=pc};
  return appendEntry(lbl->refs, &ref);
}

logical isLabelDefined(x64LblPo lbl) {
  return lbl->pc >= 0;
}

void labelDisp32(x64CtxPo ctx, x64LblPo lbl, integer pc) {
  check(readCtxAtPc(ctx, pc) == UNDEF_LBL_LANDING_PAD, "bad label reference");

  integer delta = lbl->pc - (pc + 4);
  if (isI32(delta)) {
    updateU32(ctx, pc, delta);
  } else {
    check(False, "adjusted displacement too large");
  }
}

void emitLblRef(x64CtxPo ctx, x64LblPo tgt) {
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

void emitU8(x64CtxPo ctx, u8 byte) {
  assert(ctx->pc < ctx->size);
  ctx->bytes[ctx->pc++] = byte;
}

void emitU16(x64CtxPo ctx, u16 word) {
  emitU8(ctx, word & 0xffu);
  emitU8(ctx, (u8) (word >> 8u));
}

void emitU32(x64CtxPo ctx, u32 word) {
  emitU16(ctx, word & 0xffffu);
  emitU16(ctx, (u16) (word >> 16u));
}

void emitU64(x64CtxPo ctx, u64 word) {
  emitU32(ctx, word & 0xffffffffu);
  emitU32(ctx, (u32) (word >> 32u));
}

void updateU32(x64CtxPo ctx, integer pc, u32 word) {
  ctx->bytes[pc++] = (word & 0xffu);
  ctx->bytes[pc++] = (word >> 8u) & 0xffu;
  ctx->bytes[pc++] = (word >> 16u) & 0xffu;
  ctx->bytes[pc] = (word >> 23u) & 0xffu;
}

u32 readCtxAtPc(x64CtxPo ctx, integer pc) {
  check(pc >= 0 && pc <= ctx->pc - 4, "pc out of bounds");
  return ((unsigned)ctx->bytes[pc]) | (unsigned)(ctx->bytes[pc + 1] << 8u) | (unsigned)(ctx->bytes[pc + 2] << 16u) | (unsigned)(ctx->bytes[pc + 3] << 24u);
}

logical isByte(i64 x) {
  return x >= -128 && x < 128;
}

logical isI32(i64 x) {
  return x >= MIN_I32 && x <= MAX_I32;
}
