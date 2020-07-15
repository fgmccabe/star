/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include <utils.h>
#include "x86_64P.h"

typedef struct assem_ctx {
  u8 *bytes;
  u32 size;
  u32 pc;
} AssemCtxRecord;

static u8 encodeModRR(x64Reg dst, x64Reg src);
static u8 encodeModRM(x64Reg dst, x64Reg src, i64 disp);
static void emitDisp(x64CtxPo ctx,i64 disp);
static u8 encodeSIB(x64Reg base, x64Reg index, u8 scale);
static u8 encodeModRI(x64Reg dst);
static u8 encodeRex(u8 rex, x64Reg dst, x64Reg index, x64Reg src);
static u8 encodeBinOp(u8 opCode, x64Op dst, x64Op src, x64CtxPo ctx);
static logical isByte(i64 x);

void mov(x64Op dst, x64Op src, x64CtxPo ctx) {
  switch (dst.mode) {
    case Based:
    case Reg:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.reg));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Imm:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, 0));
          emitU8(ctx, MOV_r_i32 | (((unsigned) (dst.op.reg)) & 0x7u));
          emitU64(ctx, src.op.imm);
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
          emitU8(ctx, encodeModRM(dst.op.reg, 0, src.op.indexed.disp));
          emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
          emitDisp(ctx, src.op.indexed.disp);
          return;
      }
    case Imm:
      check(False, "cannot target immediate operand");
      return;
    case Indexed:;
  }
}

u8 encodeModRR(x64Reg dst, x64Reg src) {
  u8 dr = ((u8) dst) & 0x7u;
  u8 sr = ((u8) src) & 0x7u;

  return ((u8) 0xc0) | (unsigned) (sr << 3u) | dr;
}

u8 encodeModRM(x64Reg dst, x64Reg src, i64 disp) {
  u8 dr = ((u8) dst) & 0x7u;
  u8 sr = ((u8) src) & 0x7u;

  if (disp == 0)
    return (unsigned) (sr << 3u) | dr;
  else if (isByte(disp))
    return ((u8) 0x40u) | (unsigned) (sr << 3u) | dr;
  else
    return ((u8) 0x80u) | (unsigned) (sr << 3u) | dr;
}

void emitDisp(x64CtxPo ctx,i64 disp){
  if(disp!=0){
    if(isByte(disp))
      emitU8(ctx,(unsigned)disp&0xffu);
    else
      emitU32(ctx,disp);
  }
}

u8 encodeModRI(x64Reg dst) {
  u8 dr = ((u8) dst) & 0x7u;

  return 0x5u | dr;
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
  emitU16(ctx, (u8) (word >> 16u));
}

void emitU64(x64CtxPo ctx, u64 word) {
  emitU32(ctx, word & 0xffffffffu);
  emitU32(ctx, (u8) (word >> 32u));
}

logical isByte(i64 x) {
  return x >= -128 && x < 128;
}
