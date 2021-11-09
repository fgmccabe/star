/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <utils.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>

#include "arm64P.h"
#include "jitP.h"

void initAssem() {
}

void *createCode(assemCtxPo ctx) {
  cleanupLabels(ctx);
  void *code = mmap(Null, ctx->pc, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  memcpy(code, ctx->bytes, ctx->pc);
  free(ctx->bytes);
  ctx->bytes = Null;
  discardCtx(ctx);
  return code;
}

static void encodePCRel(uint1 op, uint8 immio, int32 imm, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(op, 31) | two_bt(immio, 29) | one_bt(1, 28) | ntn_bt(imm, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeReg2Src(uint1 wide, uint1 o, uint1 S, uint8 op1, armReg R1, uint8 op2, armReg R2, armReg RD, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30)
               | one_bt(S, 29) | ayt_bt(op1, 21) | fiv_bt(R2, 16) |
               six_bt(op2, 10) | fiv_bt(R1, 5) | fiv_bt(RD, 0);
  emitU32(ctx, ins);
}

static void
encode4Reg(uint1 w, uint8 opc, uint8 op, armReg Rm, uint1 o0, armReg Ra, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) | one_bt(o0, 15) |
               fiv_bt(Ra, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeSz4Reg(uint8 sz, uint8 op, uint8 L, armReg Rm, uint1 o0, armReg Ra, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | six_bt(op, 24) | thr_bt(L, 21) |
               fiv_bt(Rm, 16) | one_bt(o0, 15) |
               fiv_bt(Ra, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encode2SrcExt(uint1 wide, uint1 o, uint1 S, uint8 op, armReg Rm, armExtent ex, uint8 imm, armReg Rn, armReg Rd,
              assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30) | one_bt(S, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) |
               thr_bt(ex, 13) | thr_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeDPRegImm(uint1 wide, uint8 opc, uint8 op, uint1 sh, uint16 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | two_bt(opc, 29) |
               six_bt(op, 23) | one_bt(sh, 22) | twl_bt(imm, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeSz2OpcImm2Reg(uint8 sz, uint8 op, uint8 opc, uint1 N, uint16 imm, uint8 o2, armReg R1, armReg R2,
                    assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | six_bt(op, 24) | two_bt(opc, 22) | one_bt(N, 21) |
               nin_bt(imm, 12) | two_bt(o2, 10) | fiv_bt(R1, 5) | fiv_bt(R2, 0);
  emitU32(ctx, ins);
}

static void encodeLdStRegUnscaled(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void encodeLdStRegPost(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | two_bt(1, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void encodeLdStRegPre(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | two_bt(3, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void encodeLdStRegOff(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | two_bt(2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeIxReg(uint8 w, uint1 V, int8 opc, int8 imm, armReg Rn, armReg Rt, ixMode ix, assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      encodeLdStRegPost(w, V, opc, imm, Rn, Rt, ctx);
      return;
    }
    case preIndex: {
      encodeLdStRegPre(w, V, opc, imm, Rn, Rt, ctx);
      return;
    }
    case signedOff: {
      encodeLdStRegOff(w, V, opc, imm, Rn, Rt, ctx);
      return;
    }
    default: {
      check(False, "invalid index mode");
    }
  }
}

void
encodeLdSt3Reg(uint8 sz, uint1 V, uint8 opc, armReg Rm, uint8 option, uint1 S, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(option, 13) | one_bt(S, 12) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void
encodeSz2OpcImm3Reg(uint8 sz, uint8 op, uint8 opc, uint1 N, armReg Rm, armExtent ex, uint1 S, uint8 S1, armReg Rn,
                    armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | six_bt(op, 24) | two_bt(opc, 22) | one_bt(N, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(S, 12) | two_bt(S1, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void
encode3Reg7Imm(uint8 opc, uint8 op, uint1 L, uint8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | svn_bt(op, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void
encodeImm3Reg(uint8 sz, uint8 opc, uint8 op, armReg Rm, uint8 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(sz, 31) | two_bt(opc, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) | six_bt(imm, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeDPRegLngImm(uint1 wide, uint8 opc, uint8 op, uint32 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | two_bt(opc, 29) | six_bt(op, 23) |
               one_bt((imm >> 12), 22) | six_bt(imm, 16) | six_bt((imm >> 6), 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeDPReg2Imm(uint1 wide, uint8 opc, uint8 op, uint1 N, uint8 imm1, uint8 imm2, armReg Rn, armReg Rd,
                assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | two_bt(opc, 29)
               | six_bt(op, 23) | one_bt(N, 22) |
               six_bt(imm1, 16) | six_bt(imm2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeShift3Reg(uint1 wide, uint1 o, uint1 S, uint8 op, armShift sh, uint1 N, armReg Rm, int8 imm, armReg Rn,
                armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30)
               | one_bt(S, 29) | fiv_bt(op, 24) | two_bt(sh, 22) | one_bt(N, 21) |
               fiv_bt(Rm, 16) | six_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encode2Imm1Src(uint1 w, uint8 op, uint8 im1, uint8 im2, armReg R1, armReg RD, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | ayt_bt(op, 23)
               | one_bt(w, 22) | six_bt(im1, 16) | six_bt(im2, 10) |
               fiv_bt(R1, 5) | fiv_bt(RD, 0);
  emitU32(ctx, ins);
}

static void encodeCondTgt(uint8 op, uint32 imm, armCond cond, assemCtxPo ctx) {
  uint32 ins = ayt_bt(op, 24) | ntn_bt(imm, 5) | for_bt(cond, 0);
  emitU32(ctx, ins);
}

static void
encodeCnd3Reg(uint1 w, uint1 o, uint1 S, uint8 op, armReg Rm, armCond cond, uint1 o2, armReg Rn, armReg Rd,
              assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(o, 30) | one_bt(S, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) | for_bt(cond, 12) | one_bt(o2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void encodeRelTgt(uint8 op, uint32 imm, assemCtxPo ctx) {
  uint32 ins = six_bt(op, 24) | tsx_bt(imm, 0);
  emitU32(ctx, ins);
}

static void encodeRelReg(uint1 wide, uint8 op, uint32 imm, armReg Rt, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | svn_bt(op, 24) | ntn_bt(imm, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void encodeCas(uint8 size, uint8 op, uint1 L, armReg Rs, uint o0, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(size, 30) | svn_bt(op, 23) | one_bt(L, 22) | one_bt(1, 21) |
               fiv_bt(Rs, 16) |
               one_bt(o0, 15) | fiv_bt(0x1f, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

static void
encodeCmpImm(uint1 w, uint8 o0, uint8 op, uint8 imm, armCond cond, armReg Rn, uint8 nzcv, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(o0, 29) | ayt_bt(op, 21) | fiv_bt(imm, 16) |
               for_bt(cond, 12) | one_bt(1, 11) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
  emitU32(ctx, ins);
}

void clearCodeCtxMaps(assemCtxPo ctx) {
  ctx->usedRegs = 0;
  ctx->freeRegs =
    X8_mask | X9_mask | X10_mask | X11_mask | X12_mask | X13_mask | X14_mask | X15_mask | X16_mask | X17_mask |
    X18_mask | X19_mask | X20_mask | X21_mask | X22_mask | X23_mask | X24_mask | X25_mask | X26_mask | X27_mask |
    X28_mask;
}

void adc_reg(uint1 wide, armReg RD, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeReg2Src(wide, 0, 0, 0xd0, S1, 0x0, S2, RD, ctx);
}

void adcs_reg(uint1 wide, armReg RD, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeReg2Src(wide, 0, 1, 0xd0, S1, 0x0, S2, RD, ctx);
}

void add_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx) {
  encode2SrcExt(w, 0, 0, 0x59, Rm, ex, shift, Rn, Rd, ctx);
}

void add_imm(uint1 wide, armReg RD, armReg S1, uint1 sh, uint16 imm, uint8 shift, assemCtxPo ctx) {
  encodeDPRegImm(wide, 0, 0x42, sh, imm, S1, RD, ctx);
}

void add_sh_(uint1 wide, armReg RD, armReg S1, armShift sh, uint8 imm, armReg S2, assemCtxPo ctx) {
  encodeShift3Reg(wide, 0, 0, 0x23, sh, 0, S1, imm, S2, RD, ctx);
}

void adds_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx) {
  encode2SrcExt(w, 0, 1, 0x59, Rm, ex, shift, Rn, Rd, ctx);
}

void add_imm_s(uint1 wide, armReg RD, armReg S1, uint1 sh, uint16 imm, uint8 shift, assemCtxPo ctx) {
  encodeDPRegImm(wide, 1, 0x42, sh, imm, S1, RD, ctx);
}

void adds_sh_(uint1 wide, armReg RD, armReg S1, armShift sh, uint8 imm, armReg S2, assemCtxPo ctx) {
  encodeShift3Reg(wide, 0, 1, 0x23, sh, 0, S1, imm, S2, RD, ctx);
}

void adr_(armReg Rd, int32 imm, assemCtxPo ctx) {
  encodePCRel(0, two_bt(imm, 0), (imm >> 2), Rd, ctx);
}

void adrp_(armReg Rd, int32 imm, assemCtxPo ctx) {
  encodePCRel(1, two_bt(imm >> 12, 0), (imm >> 2), Rd, ctx);
}

void and_imm_(uint1 wide, armReg RD, armReg S1, uint32 imm, assemCtxPo ctx) {
  encodeDPRegLngImm(wide, 0, 0x24, imm, S1, RD, ctx);
}

void and_sh_(uint1 w, armReg RD, armReg S1, uint8 sh, uint8 imm, armReg S2, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 0, 0xa, sh, 0, S1, imm, S2, RD, ctx);
}

void ands_imm_(uint1 wide, armReg RD, armReg S1, uint32 imm, assemCtxPo ctx) {
  encodeDPRegLngImm(wide, 0x3, 0x24, imm, S1, RD, ctx);
}

void ands_sh_(uint1 wide, armReg RD, armReg S1, uint8 sh, uint8 imm, armReg S2, assemCtxPo ctx) {
  encodeShift3Reg(wide, 1, 1, 0xa, sh, 0, S1, imm, S2, RD, ctx);
}

void asr_reg_(uint1 w, armReg RD, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, S1, 0x0a, S2, RD, ctx);
}

void asr_imm_(uint1 w, armReg RD, armReg S1, uint8 sh, assemCtxPo ctx) {
  uint32 imm = six_bt(sh, 6) | one_bt(w, 12) | one_bt(w, 5) | 0x1f;
  encodeDPRegLngImm(w, 0, 0x26, imm, S1, RD, ctx);
}

void asrv_(uint1 w, armReg RD, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, S2, 0xa, S1, RD, ctx);
}

void b_cond_(armCond cond, codeLblPo lbl, assemCtxPo ctx) {
  integer delta = lblDeltaRef(ctx, lbl);
  check(absolute(delta >> 2) < (1 << 19), "label out of range");
  encodeCondTgt(0x54, (uint32) delta, cond, ctx);
}

void b_(codeLblPo lbl, assemCtxPo ctx) {
  integer delta = lblDeltaRef(ctx, lbl);
  check(absolute(delta >> 2) < (1 << 26), "label out of range");
  encodeRelTgt(0x5, delta, ctx);
}

void bfc_(uint1 w, armReg RD, uint8 width, uint8 bit, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, ~bit, width - 1, 0x1f, RD, ctx);
}

void bfi_(uint1 w, armReg RD, armReg S, uint8 width, uint8 bit, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, ~bit, width - 1, S, RD, ctx);
}

void bfx_(uint1 w, armReg RD, armReg S, uint8 width, uint8 bit, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, bit, width + bit - 1, S, RD, ctx);
}

void bic_(uint1 w, armShift tp, armReg RD, armReg Rn, armReg Rm, uint8 shift, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 0, 0xa, tp, 1, Rm, shift, Rn, RD, ctx);
}

void bics_(uint1 w, armShift tp, armReg RD, armReg Rn, armReg Rm, uint8 shift, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 1, 0xa, tp, 1, Rm, shift, Rn, RD, ctx);
}

void bl_(codeLblPo lbl, assemCtxPo ctx) {
  integer delta = lblDeltaRef(ctx, lbl);
  check(absolute(delta >> 2) < (1 << 26), "label out of range");
  encodeRelTgt(0x25, delta, ctx);
}

void blr_(armReg Rn, assemCtxPo ctx) {
  uint32 ins = svn_bt(0x6b, 25) | two_bt(1, 21) |
               six_bt(0x1f, 16) | fiv_bt(Rn, 5);
  emitU32(ctx, ins);
}

void br_(armReg Rn, assemCtxPo ctx) {
  uint32 ins = svn_bt(0x6b, 25) | six_bt(0x1f, 16) | fiv_bt(Rn, 5);
  emitU32(ctx, ins);
}

void brk_(uint16 bkpt, assemCtxPo ctx) {
  uint32 ins = ayt_bt(0xd4, 24) | two_bt(1, 21) |
               sxt_bt(bkpt, 5);
  emitU32(ctx, ins);
}

void casab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0x11, 1, Rs, 0, Rn, Rt, ctx);
}

void casalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0x11, 1, Rs, 1, Rn, Rt, ctx);
}

void casb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0x11, 0, Rs, 0, Rn, Rt, ctx);
}

void caslb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0x11, 0, Rs, 1, Rn, Rt, ctx);
}

void casah_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0x11, 1, Rs, 0, Rn, Rt, ctx);
}

void casalh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0x11, 1, Rs, 1, Rn, Rt, ctx);
}

void cash_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0x11, 0, Rs, 0, Rn, Rt, ctx);
}

void caslh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0x11, 0, Rs, 1, Rn, Rt, ctx);
}

void casap_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(w, 0x10, 1, Rs, 0, Rn, Rt, ctx);
}

void casalp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(w, 0x10, 1, Rs, 1, Rn, Rt, ctx);
}

void casp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(w, 0x10, 0, Rs, 0, Rn, Rt, ctx);
}

void caslp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(w, 0x10, 0, Rs, 1, Rn, Rt, ctx);
}

void casa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(2 | w, 0x10, 1, Rs, 0, Rn, Rt, ctx);
}

void casal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(2 | w, 0x10, 1, Rs, 1, Rn, Rt, ctx);
}

void cas_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(2 | w, 0x10, 0, Rs, 0, Rn, Rt, ctx);
}

void casl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(2 | w, 0x10, 0, Rs, 1, Rn, Rt, ctx);
}

void cbnz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx) {
  integer delta = lblDeltaRef(ctx, lbl);
  check(absolute(delta >> 2) < (1 << 19), "label out of range");
  encodeRelReg(w, 0x35, delta, Rt, ctx);
}

void cbz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx) {
  integer delta = lblDeltaRef(ctx, lbl);
  check(absolute(delta >> 2) < (1 << 19), "label out of range");
  encodeRelReg(w, 0x34, delta, Rt, ctx);
}

void ccmn_imm_(uint1 w, armReg Rn, uint8 imm, uint8 nzcv, armCond cond, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(1, 29) | ayt_bt(0xd2, 21) | fiv_bt(imm, 16) |
               for_bt(cond, 12) | one_bt(1, 11) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
  emitU32(ctx, ins);
}

void ccmn_r_(uint1 w, armReg Rn, armReg Rm, uint8 nzcv, armCond cond, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(1, 29) | ayt_bt(0xd2, 21) | fiv_bt(Rm, 16) |
               for_bt(cond, 12) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
  emitU32(ctx, ins);
}

void ccmp_imm_(uint1 w, armReg Rn, uint8 imm, uint8 nzcv, armCond cond, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(3, 29) | ayt_bt(0xd2, 21) | fiv_bt(imm, 16) |
               for_bt(cond, 12) | one_bt(2, 10) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
  emitU32(ctx, ins);
}

void ccmp_r_(uint1 w, armReg Rn, armReg Rm, uint8 nzcv, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 1, 0xd2, Rm, cond, 0, Rn, nzcv, ctx);
}

void cinc_(uint1 w, armReg Rd, armReg Rm, armCond cond, armReg Rn, assemCtxPo ctx) {
  check(Rm != ZR && Rn != ZR, "invalid register");
  check((cond & 0xe0) != 0xe0, "invalid condition");

  encodeCnd3Reg(w, 0, 0, 0xd4, Rm, cond, 1, Rn, Rd, ctx);
}

void cinv_(uint1 w, armReg Rd, armReg Rm, armCond cond, armReg Rn, assemCtxPo ctx) {
  check(Rm != ZR && Rn != ZR, "invalid register");
  check((cond & 0xe0) != 0xe0, "invalid condition");

  encodeCnd3Reg(w, 1, 0, 0xd4, Rm, cond, 0, Rn, Rd, ctx);
}

void cls_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(2, 29) | ayt_bt(0xd6, 21) | thr_bt(5, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void clz_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(2, 29) | ayt_bt(0xd6, 21) | thr_bt(4, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void cmn_x(uint1 w, armReg Rn, armReg Rm, armExtent ex, uint8 imm, assemCtxPo ctx) {
  encode2SrcExt(w, 0, 1, 0x59, Rm, ex, imm, Rn, 0x1f, ctx);
}

void cmn_imm(uint1 w, armReg Rn, uint32 imm, uint1 sh, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | ayt_bt(0x62, 23) | one_bt(sh, 22) | twl_bt(imm, 10) |
               fiv_bt(Rn, 5) | fiv_bt(0x1f, 0);
  emitU32(ctx, ins);
}

void cmn_shft(uint1 w, armReg Rn, armReg Rm, armShift sh, uint8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xb, sh, 0, Rm, amnt, Rn, 0x1f, ctx);
}

void cmp_x(uint1 w, armReg Rn, armReg Rm, armExtent ex, uint8 imm, assemCtxPo ctx) {
  encode2SrcExt(w, 1, 1, 0x59, Rm, ex, imm, Rn, 0x1f, ctx);
}

void cmp_i(uint1 w, armReg Rn, uint1 sh, uint16 imm, assemCtxPo ctx) {
  encodeDPRegImm(w, 3, 0x22, sh, imm, Rn, 0x1f, ctx);
}

void cmp_shft(uint1 w, armReg Rn, armReg Rm, armShift sh, uint8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 1, 0xb, sh, 0, Rm, amnt, Rn, 0x1f, ctx);
}

void cmpp_(armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(1, 0, 1, 0xd6, Rn, 0, Rm, 0x1f, ctx);
}

void cneg_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, Rm, cond, 1, Rn, Rd, ctx);
}

void csel_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 0, 0, 0xd4, Rm, cond, 0, Rn, Rd, ctx);
}

void cset_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 0, 0, 0xd4, 0x1f, cond, 1, 0x1f, Rd, ctx);
}

void csetm_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, 0x1f, cond, 0, 0x1f, Rd, ctx);
}

void csinc_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 0, 0, 0xd4, Rm, cond, 1, Rn, Rd, ctx);
}

void csinv_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, Rm, cond, 0, Rn, Rd, ctx);
}

void csneg_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, Rm, cond, 1, Rn, Rd, ctx);
}

void eon_sh_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armShift sh, uint8 imm, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xa, sh, 1, Rm, imm, Rn, Rd, ctx);
}

void eor_imm_(uint1 w, armReg Rd, armReg Rn, uint32 imm, assemCtxPo ctx) {
  encodeDPRegLngImm(w, 2, 0x24, imm, Rn, Rd, ctx);
}

void eor_sh_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, uint8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xa, sh, 0, Rm, amnt, Rn, Rd, ctx);
}

void ld64b_(armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = fiv_bt(0x1f, 27) | ayt_bt(0xff, 14) | one_bt(1, 12) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStPrPostIx(uint8 opc, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | thr_bt(5, 27) | one_bt(V, 26) | thr_bt(1, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStPrPreIx(uint8 opc, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | thr_bt(5, 27) | one_bt(V, 26) | thr_bt(3, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStPrOffset(uint8 opc, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | thr_bt(5, 27) | one_bt(V, 26) | thr_bt(12, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeIxRegPr(uint8 w, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, ixMode ix, assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      encodeLdStPrPostIx(w, V, L, imm, Rt2, Rn, Rt, ctx);
      return;
    }
    case preIndex: {
      encodeLdStPrPreIx(w, V, L, imm, Rt2, Rn, Rt, ctx);
      return;
    }
    case signedOff: {
      encodeLdStPrOffset(w, V, L, imm, Rt2, Rn, Rt, ctx);
      return;
    }
    default: {
      check(False, "invalid index mode");
    }
  }
}

void ldp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, int8 imm, ixMode ix, assemCtxPo ctx) {
  encodeIxRegPr(w << 1, 0, 1, imm, Rt2, Rn, Rt, ix, ctx);
}

void ldpsw_(armReg Rt, armReg Rt2, armReg Rn, int8 imm, ixMode ix, assemCtxPo ctx) {
  encodeIxRegPr(1, 0, 1, imm, Rt2, Rn, Rt, ix, ctx);
}

void
encode2SrcIxImmPrePost(uint8 sz, uint8 op, uint8 opc, ixMode ix, uint16 imm, armReg Rn, armReg Rt,
                       assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      uint32 ins = two_bt(sz, 30) | for_bt(op, 26) | two_bt(opc, 22) |
                   nin_bt(imm, 12) | two_bt(1, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    case preIndex: {
      uint32 ins = two_bt(sz, 30) | for_bt(op, 26) | two_bt(opc, 22) |
                   nin_bt(imm, 12) | two_bt(3, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    case unsignedOff: {
      uint32 ins = two_bt(sz, 30) | for_bt(op, 26) | two_bt(1, 24) | two_bt(opc, 22) |
                   twl_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    default:
      check(False, "invalid prePostindex mode");
  }
}

void ldr_(uint1 w, armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx) {
  encode2SrcIxImmPrePost((2 | w), 0xe, 0x1, imm, ix, Rn, Rt, ctx);
}

void ldr_lit(uint1 w, armReg Rt, uint32 imm, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 30) | six_bt(0x18, 24) |
               ntn_bt(imm, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldr_r_(uint1 w, armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical shft, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 31) | one_bt(w, 30) | six_bt(0x38, 24) | two_bt(1, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(shft, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrb_imm(armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx) {
  encode2SrcIxImmPrePost(0, 0xe, 0x1, imm, ix, Rn, Rt, ctx);
}

void ldrb_r(armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical sh, assemCtxPo ctx) {
  uint32 ins = thr_bt(7, 27) | one_bt(1, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(sh, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrh_imm(armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx) {
  encode2SrcIxImmPrePost(1, 0xe, 0x1, imm, ix, Rn, Rt, ctx);
}

void ldrh_r_(armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical shft, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 30) | six_bt(0x38, 24) | two_bt(1, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(shft, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrsb_imm(uint1 w, armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx) {
  encode2SrcIxImmPrePost(0, 0xe, (2 | w), imm, ix, Rn, Rt, ctx);
}

void ldrsb_r(uint1 w, armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical sh, assemCtxPo ctx) {
  uint32 ins = thr_bt(7, 27) | one_bt(1, 23) | one_bt(~w, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(sh, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrsh_imm(uint1 w, armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx) {
  encode2SrcIxImmPrePost(1, 0xe, (2 | w), imm, ix, Rn, Rt, ctx);
}

void ldrsh_r(uint1 w, armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical sh, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 30) | thr_bt(7, 27) | one_bt(1, 23) | one_bt(~w, 22) |
               one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(sh, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrsw_imm(armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx) {
  encode2SrcIxImmPrePost(2, 0xe, 2, imm, ix, Rn, Rt, ctx);
}

void ldrsw_lit(armReg Rt, codeLblPo lbl, assemCtxPo ctx) {
  integer delta = lblDeltaRef(ctx, lbl);
  check(absolute(delta >> 2) < (1 << 19), "label out of range");
  encodeRelReg(1, 0x18, delta, Rt, ctx);
}

void ldrsw_r(armReg Rt, armReg Rn, armReg Rm, armExtent ex, uint1 sh, assemCtxPo ctx) {
  encodeSz2OpcImm3Reg(2, 0x38, 2, 1, Rm, ex, sh, 2, Rn, Rt, ctx);
}

void ldur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled((2 | w), 0, 1, imm, Rn, Rt, ctx);
}

void ldurb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(0, 0, 1, imm, Rn, Rt, ctx);
}

void ldurh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(1, 0, 1, imm, Rn, Rt, ctx);
}

void ldursb_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(0, 0, (2 | w), imm, Rn, Rt, ctx);
}

void ldursh_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(1, 0, (2 | w), imm, Rn, Rt, ctx);
}

void ldursw_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(2, 0, 2, imm, Rn, Rt, ctx);
}

void ldxp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 31) | one_bt(w, 30) | six_bt(8, 24) | one_bt(1, 22) | one_bt(1, 21) |
               fiv_bt(0x1f, 16) |
               fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt1, 0);
  emitU32(ctx, ins);
}

void ldxr_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 31) | one_bt(w, 30) | six_bt(8, 24) |
               one_bt(1, 22) | fiv_bt(0x1f, 16) |
               fiv_bt(0x1f, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldxrb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  uint32 ins = six_bt(8, 24) |
               one_bt(1, 22) | fiv_bt(0x1f, 16) |
               fiv_bt(0x1f, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldxrh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 30) | six_bt(8, 24) |
               one_bt(1, 22) | fiv_bt(0x1f, 16) |
               fiv_bt(0x1f, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void lsl_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, Rm, 0x8, Rn, Rd, ctx);
}

void lsl_imm(uint1 w, armReg Rd, armReg Rn, uint8 shift, assemCtxPo ctx) {
  encodeDPReg2Imm(w, 2, 0x26, w, (-shift % 32), ((-shift % 32) - 1), Rn, Rd, ctx);
}

void lsr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, Rm, 0x9, Rn, Rd, ctx);
}

void lsr_imm(uint1 w, armReg Rd, armReg Rn, uint8 shift, assemCtxPo ctx) {
  encodeDPReg2Imm(w, 2, 0x26, w, shift, (one_bt(w, 5) | 0x1f), Rn, Rd, ctx);
}

void madd_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armReg Ra, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 0, Ra, Rn, Rd, ctx);
}

void mneg_(uint1 w, armReg Rd, armReg Rm, armReg Rn, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 1, ZR, Rn, Rd, ctx);
}

void mov_sp_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  check(Rd == SP || Rn == SP, "Only use with SP");
  encodeDPRegImm(w, 0, 0x42, 0, 0, Rn, Rd, ctx);
}

void encodeImm1Reg(uint1 w, uint8 opc, uint8 op, uint8 hw, int16 imm, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | six_bt(op, 23) | two_bt(hw, 21) |
               sxt_bt(imm, 5) | fiv_bt(Rd, 0);
}

void mov_inv_(uint1 w, armReg Rd, int32 imm, int8 sh, assemCtxPo ctx) {
  encodeImm1Reg(w, 0, 0x25, sh, imm, Rd, ctx);
}

void mov_wide(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeImm1Reg(w, 2, 0x25, sh, imm, Rd, ctx);
}

void mov_msk(uint1 w, armReg Rd, uint16 imm, assemCtxPo ctx) {
  encodeDPRegImm(w, 1, 0x24, (imm >> 12) & 1, imm, ZR, Rd, ctx);
}

void mov_r(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, 0, 0, Rm, 0, ZR, Rd, ctx);
}

void movk_(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeImm1Reg(w, 3, 0x25, sh, imm, Rd, ctx);
}

void movn_(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeImm1Reg(w, 0, 0x25, sh, imm, Rd, ctx);
}

void movz_(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeImm1Reg(w, 2, 0x25, sh, imm, Rd, ctx);
}

void mul_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 0, ZR, Rn, Rd, ctx);
}

void mvn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 1, Rm, amnt, ZR, Rd, ctx);
}

void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xb, sh, 0, Rm, amnt, ZR, Rd, ctx);
}

void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 1, 0xb, sh, 0, Rm, amnt, ZR, Rd, ctx);
}

void orn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 1, Rm, amnt, ZR, Rd, ctx);
}

void orr_(uint1 w, armReg Rd, armReg Rn, int32 imm, assemCtxPo ctx) {
  encodeDPRegLngImm(w, 1, 0x24, imm, Rn, Rd, ctx);
}

void orr_sh_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 0, Rm, amnt, Rn, Rd, ctx);
}

void rbit_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encode4Reg(w, 2, 0xd6, 0, 0, 0, Rn, Rd, ctx);
}

void ret_(armReg Rn, assemCtxPo ctx) {
  uint32 ins = svn_bt(0x6b, 25) | two_bt(2, 21) | six_bt(0x1f, 16) | fiv_bt(Rn, 5);
  emitU32(ctx, ins);
}

void rev_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encode2SrcExt(w, 1, 0, 0xd6, 0, 0, (2 | w), Rn, Rd, ctx);
}

void rev16_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encode2SrcExt(w, 1, 0, 0xd6, 0, 0, 1, Rn, Rd, ctx);
}

void rev32_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encode2SrcExt(1, 1, 0, 0xd6, 0, 0, 2, Rn, Rd, ctx);
}

void ror_(uint1 w, armReg Rd, armReg Rn, int16 amnt, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, (0xc | (w << 1)), Rn, amnt, Rn, Rd, ctx);
}

void rorv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeImm3Reg(w, 0, 0xd6, Rm, 0xb, Rn, Rd, ctx);
}

void sbc_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeImm3Reg(w, 2, 0xd0, Rm, 0, Rn, Rd, ctx);
}

void sbcs_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeImm3Reg(w, 3, 0xd0, Rm, 0, Rn, Rd, ctx);
}

void sbfiz_(uint1 w, armReg Rd, armReg Rn, int8 lsb, int8 width, assemCtxPo ctx) {
  encodeDPReg2Imm(w, 0, 0x26, w, (-lsb % 64), width - 1, Rn, Rd, ctx);
}

void sbfx_(uint1 w, armReg Rd, armReg Rn, int8 lsb, int8 width, assemCtxPo ctx) {
  encodeDPReg2Imm(w, 0, 0x26, w, lsb, (lsb + width - 1), Rn, Rd, ctx);
}

void sdiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, Rm, 3, Rn, Rd, ctx);
}

void smaddl(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 0, Ra, Rn, Rd, ctx);
}

void smnegl_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 1, ZR, Rn, Rd, ctx);
}

void smsubl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 1, Ra, Rn, Rd, ctx);
}

void smmulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xda, Rm, 0, ZR, Rn, Rd, ctx);
}

void smmull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xda, Rm, 0, ZR, Rn, Rd, ctx);
}

void stllrb_(armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg(0, 0x8, 4, 0x1f, 0, 0x1f, Rn, Rd, ctx);
}

void stllrh_(armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg(1, 0x8, 4, 0x1f, 0, 0x1f, Rn, Rd, ctx);
}

void stllr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg((2 | w), 0x8, 4, 0x1f, 0, 0x1f, Rn, Rd, ctx);
}

void stlr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg((2 | w), 0x8, 4, 0x1f, 1, 0x1f, Rn, Rd, ctx);
}

void stlrb_(armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg(0, 0x8, 4, 0x1f, 1, 0x1f, Rn, Rd, ctx);
}

void stlrh_(armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg(1, 0x8, 4, 0x1f, 1, 0x1f, Rn, Rd, ctx);
}

void stlur_(uint1 w, armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeSz2OpcImm2Reg((2 | w), 0x19, 0, 0, imm, 0, Rn, Rd, ctx);
}

void stlurb_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeSz2OpcImm2Reg(0, 0x19, 0, 0, imm, 0, Rn, Rd, ctx);
}

void stlurh_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeSz2OpcImm2Reg(1, 0x19, 0, 0, imm, 0, Rn, Rd, ctx);
}

void stlxp_(uint1 w, armReg Rs, armReg Rt1, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg((2 | w), 0x8, 1, Rs, 1, Rt2, Rn, Rt1, ctx);
}

void stlxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg((2 | w), 0x8, 0, Rs, 1, 0x1f, Rn, Rt, ctx);
}

void stlxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg(0, 0x8, 0, Rs, 1, 0x1f, Rn, Rt, ctx);
}

void stlxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg(1, 0x8, 0, Rs, 1, 0x1f, Rn, Rt, ctx);
}

void stnp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx) {
  encode3Reg7Imm(one_bt(w, 1), 0x58, 0, imm, Rt2, Rn, Rt1, ctx);
}

void
encode2Src7ImmPrePost(uint8 sz, uint8 op, ixMode ix, uint8 imm, armReg Rt2, armReg Rn, armReg Rt,
                      assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      uint32 ins = two_bt(sz, 30) | for_bt(op, 26) | two_bt(1, 23) |
                   svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    case preIndex: {
      uint32 ins = two_bt(sz, 30) | for_bt(op, 26) | two_bt(3, 23) |
                   svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    case unsignedOff: {
      uint32 ins = two_bt(sz, 30) | for_bt(op, 26) | two_bt(2, 23) |
                   twl_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    default:
      check(False, "invalid prePostindex mode");
  }
}

void stp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, int8 imm, ixMode ix, assemCtxPo ctx) {
  encodeIxRegPr(w << 1, 0, 0, imm, Rt2, Rn, Rt, ix, ctx);
}

void str_(uint1 w, armReg Rt, armReg Rn, ixMode ix, int16 imm, assemCtxPo ctx) {
  encodeIxReg((2 | w), 0, 0, imm, Rn, Rt, ix, ctx);
}

void str_r_(uint1 w, armReg Rt, armReg Rn, armReg Rm, armExtent ex, uint1 scaled, uint8 amnt, assemCtxPo ctx) {
  encodeSz2OpcImm3Reg((2 | w), 0x1c, 0, 1, Rm, ex, scaled, 2, Rn, Rt, ctx);
}

void strb_(armReg Rt, armReg Rn, int16 imm, ixMode ix, assemCtxPo ctx) {
  encodeIxReg(0, 0, 0, imm, Rn, Rt, ix, ctx);
}

void strb_r_(armReg Rt, armReg Rn, armReg Rm, armExtent ex, uint1 shft, assemCtxPo ctx) {
  encodeLdSt3Reg(0, 0, 0, Rm, ex, shft, Rn, Rt, ctx);
}

void strh_(armReg Rt, armReg Rn, int16 imm, ixMode ix, assemCtxPo ctx) {
  encodeIxReg(1, 0, 0, imm, Rn, Rt, ix, ctx);
}

void strh_r_(armReg Rt, armReg Rn, armReg Rm, armExtent ex, uint1 shft, assemCtxPo ctx) {
  encodeLdSt3Reg(1, 0, 0, Rm, ex, shft, Rn, Rt, ctx);
}

void sttr_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegOff((2 | w), 0, 0, imm, Rn, Rt, ctx);
}

void sttrb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegOff(0, 0, 0, imm, Rn, Rt, ctx);
}

void sttrh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegOff(1, 0, 0, imm, Rn, Rt, ctx);
}

void stur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled((2 | w), 0, 0, imm, Rn, Rt, ctx);
}

void sturb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(0, 0, 0, imm, Rn, Rt, ctx);
}

void sturh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(1, 0, 0, imm, Rn, Rt, ctx);
}

void sub_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, int8 amnt, assemCtxPo ctx) {
  encode2SrcExt(w, 1, 0, 0x59, Rm, ex, amnt, Rn, Rd, ctx);
}

void sub_sh(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 imm, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xb, sh, 0, Rm, imm, Rn, Rd, ctx);
}
