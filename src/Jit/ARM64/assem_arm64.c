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

static void
encodeReg2Src(uint1 wide, uint1 o, uint1 S, uint8 op1, armReg R1, uint8 op2, armReg R2, armReg RD, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30)
               | one_bt(S, 29) | ayt_bt(op1, 21) | fiv_bt(R2, 16) |
               six_bt(op2, 10) | fiv_bt(R1, 5) | fiv_bt(RD, 0);
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
encodeDPRegImm(uint1 wide, uint1 o, uint1 S, uint8 op1, uint1 N, uint16 imm, armReg R1, armReg RD, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30) | one_bt(S, 29) |
               six_bt(op1, 23) | one_bt(N, 22) | twl_bt(imm, 10) |
               fiv_bt(R1, 5) | fiv_bt(RD, 0);
  emitU32(ctx, ins);
}

static void
encodeDPRegLngImm(uint1 wide, uint8 opc, uint8 op, uint32 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | two_bt(opc, 29)
               | six_bt(op, 23) |
               one_bt((imm >> 12), 22) | six_bt(imm, 16) | six_bt((imm >> 6), 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void
encodeShift3Reg(uint1 wide, uint1 o, uint1 S, uint8 op, armShift sh, uint1 N, armReg Rm, uint8 imm, armReg Rn,
                armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30)
               | one_bt(S, 29) | fiv_bt(op, 24) | two_bt(sh, 22) | one_bt(N, 21) |
               fiv_bt(Rm, 16) | six_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void encodeDPImm(uint1 w, uint8 op, uint32 imm, armReg Rg, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(imm, 29) | fiv_bt(op, 24) | snt_bt((imm >> 2u), 5) | fiv_bt(Rg, 0);
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

static void
encodeRelReg(uint1 wide, uint8 op, uint32 imm, armReg Rt, assemCtxPo ctx) {
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
  encodeDPRegImm(wide, 0, 0, 0x42, sh, imm, S1, RD, ctx);
}

void add_sh_(uint1 wide, armReg RD, armReg S1, armShift sh, uint8 imm, armReg S2, assemCtxPo ctx) {
  encodeShift3Reg(wide, 0, 0, 0x23, sh, 0, S1, imm, S2, RD, ctx);
}

void adds_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx) {
  encode2SrcExt(w, 0, 1, 0x59, Rm, ex, shift, Rn, Rd, ctx);
}

void add_imm_s(uint1 wide, armReg RD, armReg S1, uint1 sh, uint16 imm, uint8 shift, assemCtxPo ctx) {
  encodeDPRegImm(wide, 0, 1, 0x42, sh, imm, S1, RD, ctx);
}

void adds_sh_(uint1 wide, armReg RD, armReg S1, armShift sh, uint8 imm, armReg S2, assemCtxPo ctx) {
  encodeShift3Reg(wide, 0, 1, 0x23, sh, 0, S1, imm, S2, RD, ctx);
}

void adr_(armReg RD, uint32 imm, assemCtxPo ctx) {
  encodeDPImm(0, 0x10, imm, RD, ctx);
}

void adrp_(armReg RD, uint32 imm, assemCtxPo ctx) {
  encodeDPImm(1, 0x10, imm, RD, ctx);
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
  encodeDPRegImm(w, 1, 1, 0x22, sh, imm, Rn, 0x1f, ctx);
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

void ldp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, uint8 imm, prePostIndex ix, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | fiv_bt(0x1f, 25) | two_bt(ix, 23) | one_bt(1, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldpsw_(armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, prePostIndex ix, assemCtxPo ctx) {
  uint32 ins = two_bt(1, 30) | fiv_bt(0x14, 25) | two_bt(ix, 23) | one_bt(1, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt1, 0);
  emitU32(ctx, ins);
}

void
encode2SrcImmPrePost(uint8 w, uint8 op, uint8 opc, prePostIndex ix, uint16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      uint32 ins = two_bt(w, 30) | six_bt(op, 24) | two_bt(opc, 22) |
                   nin_bt(imm, 12) | two_bt(1, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    case preIndex: {
      uint32 ins = two_bt(w, 30) | six_bt(op, 24) | two_bt(opc, 22) |
                   nin_bt(imm, 12) | two_bt(3, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    case unsignedOff: {
      uint32 ins = two_bt(w, 30) | six_bt(op, 24) | two_bt(opc, 22) |
                   twl_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
      emitU32(ctx, ins);
      return;
    }
    default:
      check(False, "invalid prePostindex mode");
  }
}

void ldr_(uint1 w, armReg Rt, armReg Rn, uint16 imm, prePostIndex ix, assemCtxPo ctx) {
  encode2SrcImmPrePost(two_bt(one_bt(w, 0), 1), 0x38, 0x1, imm, ix, Rn, Rt, ctx);
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

void ldrb_imm(armReg Rt, armReg Rn, uint16 imm, prePostIndex ix, assemCtxPo ctx) {
  encode2SrcImmPrePost(0, 0x38, 0x1, imm, ix, Rn, Rt, ctx);
}

void ldrb_r(armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical sh, assemCtxPo ctx) {
  uint32 ins = thr_bt(7, 27) | one_bt(1, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(sh, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrh_imm(armReg Rt, armReg Rn, uint16 imm, prePostIndex ix, assemCtxPo ctx) {
  encode2SrcImmPrePost(1, 0x39, 0x1, imm, ix, Rn, Rt, ctx);
}

void ldrh_r_(armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical shft, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 30) | six_bt(0x38, 24) | two_bt(1, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(shft, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrsb_imm(uint1 w, armReg Rt, armReg Rn, uint16 imm, prePostIndex ix, assemCtxPo ctx) {
  encode2SrcImmPrePost(0, 0x38, two_bt(2 + w, 22), imm, ix, Rn, Rt, ctx);
}

void ldrsb_r(uint1 w, armReg Rt, armReg Rn, armReg Rm, armExtent ex, logical sh, assemCtxPo ctx) {
  uint32 ins = thr_bt(7, 27) | one_bt(1, 23) | one_bt(~w, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(sh, 12) | two_bt(2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldrsh_imm(uint1 w, armReg Rt, armReg Rn, uint16 imm, prePostIndex ix, assemCtxPo ctx) {
  encode2SrcImmPrePost(1, 0x38, two_bt(2 + w, 22), imm, ix, Rn, Rt, ctx);
}
