/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include <utils.h>
#include <stdlib.h>
#include "x86_64P.h"

poolPo ctxPool = Null;
poolPo lblPool = Null;

void initAssemX4() {
  if (ctxPool == Null) {
    ctxPool = newPool(sizeof(AssemCtxRecord), 128);
    lblPool = newPool(sizeof(AssemLblRecord), 128);
  }
}

static retCode clearLbl(char *lblNme, x64LblPo lbl);
static retCode cleanupLabels(x64CtxPo ctx);

x64CtxPo createCtx() {
  initAssemX4();
  x64CtxPo ctx = (x64CtxPo) allocPool(ctxPool);
  ctx->bytes = malloc(1024);
  ctx->size = 1024;
  ctx->pc = 0;
  ctx->lbls = newHash(256, (hashFun) uniHash, (compFun) uniCmp, (destFun) clearLbl);
  return ctx;
}

void *cleanupCtx(x64CtxPo ctx) {
  cleanupLabels(ctx);
  void *code = realloc(ctx->bytes, ctx->pc);
  ctx->bytes = Null;
  freePool(ctxPool, ctx);
  return code;
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

static u8 encodeModRR(x64Reg dst, x64Reg src);
static u8 encodeModRM(x64Reg fst, x64Reg snd, i64 disp);
static void emitDisp(x64CtxPo ctx, i64 disp);
static u8 encodeSIB(x64Reg base, x64Reg index, u8 scale);
static u8 encodeModRI(x64Reg dst);
static u8 encodeRex(u8 rex, x64Reg dst, x64Reg index, x64Reg src);
static u8 encodeBinOp(u8 opCode, x64Op dst, x64Op src, x64CtxPo ctx);
static void emitLblRef(x64CtxPo ctx, x64LblPo tgt);
static logical isByte(i64 x);
static logical isI32(i64 x);

static void labelDisp32(x64CtxPo ctx, x64LblPo lbl, integer pc) {
  check(readCtxAtPc(ctx, pc) == UNDEF_LBL_LANDING_PAD, "bad label reference");

  integer delta = lbl->pc - (pc + 4);
  if (isI32(delta)) {
    updateU32(ctx, pc, delta);
  } else {
    check(False, "adjusted displacement too large");
  }
}

void lea_(x64Reg dst, x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Based:
      emitU8(ctx, encodeRex(REX_W, dst, 0, src.op.based.base));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dst, src.op.based.base, src.op.based.disp));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      emitU8(ctx, encodeRex(REX_W, dst, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dst, 0x4, src.op.indexed.disp));
      emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled: {
      x64LblPo tgt = src.op.lbl;
      emitU8(ctx, encodeRex(REX_W, dst, 0, 0x0));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dst, 0x5, 0));
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void mov_(x64Op dst, x64Op src, x64CtxPo ctx) {
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.based.base));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, dst.op.based.base, dst.op.based.disp));
          emitDisp(ctx, dst.op.based.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Imm:
          if (isI32(src.op.imm)) {
            emitU8(ctx, encodeRex(REX_W, 0, 4, dst.op.based.base));
            emitU8(ctx, MOV_rm_imm);
            if (dst.op.based.base != RAX) {
              emitU8(ctx, encodeModRM(0, 0x4, dst.op.based.disp)); // 4 = no index
              emitU8(ctx, encodeSIB(dst.op.based.base, 4, 1));
            } else {
              emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp));
            }
            emitDisp(ctx, dst.op.based.disp);
            emitU32(ctx, src.op.imm);
          } else {
            check(False, "immediate too large");
          }
          return;
        case Labeled:
          check(False, "Not permitted");
          return;
      }
    case Reg:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.reg));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Imm:
          emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
          if (isI32(src.op.imm)) {
            emitU8(ctx, MOV_rm_imm);
            emitU8(ctx, encodeModRI(dst.op.reg));
            emitU32(ctx, src.op.imm);
          } else {
            emitU8(ctx, MOV_r_i32 | (((unsigned) (dst.op.reg)) & 0x7u));
            emitU64(ctx, src.op.imm);
          }
          return;
        case Based:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp));
          emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          x64LblPo tgt = src.op.lbl;
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, 0x0));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x5, 0));
          emitLblRef(ctx, tgt);
          return;
        }
      }
    case Imm:
      check(False, "cannot target immediate operand");
      return;
    case Indexed:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp));
          emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
          emitDisp(ctx, dst.op.indexed.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Imm:
          emitU8(ctx, encodeRex(REX_W, 0, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, MOV_rm_imm);
          emitU8(ctx, encodeModRM(0, 0x4, dst.op.indexed.disp));
          emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
          emitDisp(ctx, dst.op.indexed.disp);
          emitU32(ctx, src.op.imm);
          return;
        case Labeled:
          check(False, "Not permitted");
          return;
      }
    case Labeled:
      check(False, "Not permitted");
      return;
  }
}

void push_(x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, PUSH_r + (src.op.reg & 0x7u));
      return;
    case Imm: {
      if (isByte(src.op.imm)) {
        emitU8(ctx, PUSH_i8);
        emitU8(ctx, (u8) src.op.imm);
      } else if (isI32(src.op.imm)) {
        emitU8(ctx, PUSH_i32);
        emitU32(ctx, (u32) src.op.imm);
      } else
        check(False, "immediate too large");
      return;
    }
    case Based:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.based.base));

      emitU8(ctx, PUSH_rm);
      emitU8(ctx, encodeModRM(6, src.op.based.base, src.op.based.disp));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      if (src.op.indexed.base >= R8 || src.op.indexed.index >= R8)
        emitU8(ctx, encodeRex(REX, 0, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, PUSH_rm);
      emitU8(ctx, encodeModRM(0x6, 0x4, src.op.indexed.disp));
      emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled:
      check(False, "Not permitted");
      return;
  }
}

void pop_(x64Op dst, x64CtxPo ctx) {
  switch (dst.mode) {
    case Reg:
      if (dst.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, dst.op.reg));
      emitU8(ctx, POP_r + (dst.op.reg & 0x7u));
      return;
    case Based:
      if (dst.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, dst.op.based.base));

      emitU8(ctx, POP_rm);
      emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      if (dst.op.indexed.base >= R8 || dst.op.indexed.index >= R8)
        emitU8(ctx, encodeRex(REX, 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, POP_rm);
      emitU8(ctx, encodeModRM(0x0, 0x4, dst.op.indexed.disp));
      emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void emitLblRef(x64CtxPo ctx, x64LblPo tgt) {
  if (isLabelDefined(tgt)) {
    integer delta = tgt->pc - (ctx->pc + 4);
    if (!isI32(delta)){
      check(False, "label displacement too large");
      return;
    }
    emitU32(ctx, delta);
  } else {
    addLabelReference(ctx, tgt, ctx->pc, labelDisp32);
    emitU32(ctx, UNDEF_LBL_LANDING_PAD);
  }
}

u8 encodeModRR(x64Reg dst, x64Reg src) {
  u8 dr = ((u8) dst) & 0x7u;
  u8 sr = ((u8) src) & 0x7u;

  return ((u8) 0xc0) | (unsigned) (sr << 3u) | dr;
}

u8 encodeModRM(x64Reg fst, x64Reg snd, i64 disp) {
  u8 fr = ((u8) fst) & 0x7u;
  u8 sr = ((u8) snd) & 0x7u;

  if (disp == 0)
    return (unsigned) (fr << 3u) | sr;
  else if (isByte(disp))
    return ((u8) 0x40u) | (unsigned) (fr << 3u) | sr;
  else
    return ((u8) 0x80u) | (unsigned) (fr << 3u) | sr;
}

void emitDisp(x64CtxPo ctx, i64 disp) {
  if (disp != 0) {
    if (isByte(disp))
      emitU8(ctx, (unsigned) disp & 0xffu);
    else
      emitU32(ctx, disp);
  }
}

u8 encodeModRI(x64Reg dst) {
  u8 dr = ((u8) dst) & 0x7u;

  return 0xc0u | dr;
}

u8 encodeSIB(x64Reg base, x64Reg index, u8 scale) {
  u8 sib = ((index & 0x7u) << 3u) | (base & 0x7u);

  switch (scale) {
    case 1:
      return sib;
    case 2:
      return (1u << 6u) | sib;
    case 4:
      return (2u << 6u) | sib;
    case 8:
      return (3u << 6u) | sib;
    default:
      check(False, "invalid scale factor");
      return 0;
  }
}

u8 encodeRex(u8 rex, x64Reg dst, x64Reg index, x64Reg src) {
  u8 dr = ((unsigned) (((u8) dst) >> 3u) & 1u) << 2u;
  u8 sr = ((unsigned) (((u8) src) >> 3u) & 1u);
  u8 sx = ((unsigned) (((u8) index) >> 3u) & 1u) << 1u;

  return (unsigned) rex | dr | sx | sr;
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
  check(pc >= 0 && pc < ctx->pc - 4, "pc out of bounds");
  return ctx->bytes[pc] | (ctx->bytes[pc + 1] << 8u) | (ctx->bytes[pc + 2] << 16u) | (ctx->bytes[pc + 3] << 24u);
}

logical isByte(i64 x) {
  return x >= -128 && x < 128;
}

logical isI32(i64 x) {
  return x >= MIN_I32 && x <= MAX_I32;
}
