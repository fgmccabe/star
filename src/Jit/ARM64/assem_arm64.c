/*
 * Arm 64 assembler
 * Intended to be called from C code
 */

#include <utils.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>
#include <assert.h>

#include "arm64P.h"
#include "assem_encode.h"
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

void clearCodeCtxMaps(assemCtxPo ctx) {
  ctx->usedRegs = 0;
  ctx->freeRegs =
    X8_mask | X9_mask | X10_mask | X11_mask | X12_mask | X13_mask | X14_mask | X15_mask | X16_mask | X17_mask |
    X18_mask | X19_mask | X20_mask | X21_mask | X22_mask | X23_mask | X24_mask | X25_mask | X26_mask | X27_mask |
    X28_mask;
}

void adc_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(wide, 0, 0, 0xd0, Rm, 0x0, Rn, rd, ctx);
}

void adcs_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(wide, 0, 1, 0xd0, Rm, 0x0, Rn, rd, ctx);
}

void add_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:  // Immediate value
      encodeAddSubImm(w, 0, 0, 0x22, S2.hiLo, S2.immediate, Rn, Rd, ctx);
      return;
    case lsli: // Logical shift left immediate
      encode3Reg7Imm(w, 0, 0, 0xb, LSL, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case lsri :// logical shift right immediate
      encode3Reg7Imm(w, 0, 0, 0xb, LSR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri: // arithmetic shift right immediate
      encode3Reg7Imm(w, 0, 0, 0xb, ASR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encode3Reg7Imm(w, 0, 0, 0xb, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void add_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx) {
  encode2SrcExt(w, 0, 0, 0x59, Rm, ex, shift, Rn, Rd, ctx);
}

void adds_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx) {
  encode2SrcExt(w, 0, 1, 0x59, Rm, ex, shift, Rn, Rd, ctx);
}

void adds_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:  // Immediate value
      encodeAddSubImm(w, 0, 1, 0x22, S2.hiLo, S2.immediate, Rn, Rd, ctx);
      return;
    case lsli: // Logical shift left immediate
      encode3Reg7Imm(w, 0, 1, 0xb, LSL, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case lsri :// logical shift right immediate
      encode3Reg7Imm(w, 0, 1, 0xb, LSR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri: // arithmetic shift right immediate
      encode3Reg7Imm(w, 0, 1, 0xb, ASR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encode3Reg7Imm(w, 0, 1, 0xb, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void adr_(armReg Rd, codeLblPo lbl, assemCtxPo ctx) {
  encodePCRel(0, lbl, Rd, ctx);
}

void adrp_(armReg Rd, codeLblPo lbl, assemCtxPo ctx) {
  encodePCRel(1, lbl, Rd, ctx);
}

void and_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:  // Immediate value
      encodeLogImm(w, 0, S2.immediate, Rn, Rd, ctx);
      return;
    case lsli: // Logical shift left immediate
      encode3Reg7Imm(w, 0, 0, 0xa, LSL, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case lsri :// logical shift right immediate
      encode3Reg7Imm(w, 0, 0, 0xa, LSR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri: // arithmetic shift right immediate
      encode3Reg7Imm(w, 0, 0, 0xa, ASR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case rori: // rotate right immediate
      encode3Reg7Imm(w, 0, 0, 0xa, ROR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encodeShift3Reg(w, 0, 0, 0xa, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void ands_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:  // Immediate value
      encodeLogImm(w, 3, S2.immediate, Rn, Rd, ctx);
      return;
    case lsli: // Logical shift left immediate
      encodeShift3Reg(w, 1, 1, 0xa, LSL, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case lsri :// logical shift right immediate
      encodeShift3Reg(w, 1, 1, 0xa, LSR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri: // arithmetic shift right immediate
      encodeShift3Reg(w, 1, 1, 0xa, ASR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case rori: // rotate right immediate
      encodeShift3Reg(w, 1, 1, 0xa, ROR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encodeShift3Reg(w, 1, 1, 0xa, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void asr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:
      encodeImmRegReg(w, 0, w, S2.immediate, 0x1f | (w << 5), Rn, Rd, ctx);
      return;
    case reg:
      encodeReg2Src(w, 0, 0, 0xd6, S2.reg, 0x0a, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void b_cond_(armCond cond, codeLblPo lbl, assemCtxPo ctx) {
  encodeCondBrnch(0x2a, 0, lbl, cond, ctx);
}

void b_(codeLblPo lbl, assemCtxPo ctx) {
  encodeBranchImm(0, lbl, ctx);
}

void bfc_(uint1 w, armReg RD, uint8 bit, uint8 width, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, 64 - bit, width - 1, 0x1f, RD, ctx);
}

void bfi_(uint1 w, armReg RD, armReg Rn, uint8 bit, uint8 width, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, 64 - bit, width - 1, Rn, RD, ctx);
}

void bfxil_(uint1 w, armReg RD, armReg Rn, uint8 bit, uint8 width, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, bit, width + bit - 1, Rn, RD, ctx);
}

void bfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx) {
  encode2Imm1Src(w, 0x66, immr, imms, Rn, Rd, ctx);
}

void bic_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encodeShift3Reg(w, 0, 0, 0xa, LSL, 1, S2.reg, 0, Rn, Rd, ctx);
      return;
    case lsli:
      encodeShift3Reg(w, 0, 0, 0xa, LSL, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case lsri:
      encodeShift3Reg(w, 0, 0, 0xa, LSR, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri:
      encodeShift3Reg(w, 0, 0, 0xa, ASR, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case rori:
      encodeShift3Reg(w, 0, 0, 0xa, ROR, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void bics_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encodeShift3Reg(w, 1, 1, 0xa, LSL, 1, S2.reg, 0, Rn, Rd, ctx);
      return;
    case lsli:
      encodeShift3Reg(w, 1, 1, 0xa, LSL, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case lsri:
      encodeShift3Reg(w, 1, 1, 0xa, LSR, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri:
      encodeShift3Reg(w, 1, 1, 0xa, ASR, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case rori:
      encodeShift3Reg(w, 1, 1, 0xa, ROR, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void bl_(codeLblPo lbl, assemCtxPo ctx) {
  encodeBranchImm(1, lbl, ctx);
}

void blr_(armReg Rn, assemCtxPo ctx) {
  encodeBranch(1, 0x1f, 0, Rn, 0, ctx);
}

void br_(armReg Rn, assemCtxPo ctx) {
  encodeBranch(0, 0x1f, 0, Rn, 0, ctx);
}

void brk_(uint16 bkpt, assemCtxPo ctx) {
  uint32 ins = ayt_bt(0xd4, 24) | one_bt(1, 21) |
               sxt_bt(bkpt, 5);
  emitU32(ctx, ins);
}

void casab(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 1, Rs, 0, XZR, Rn, Rt, ctx);
}

void casalb(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 1, Rs, 1, XZR, Rn, Rt, ctx);
}

void casb(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void caslb(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void casah(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 1, Rs, 0, XZR, Rn, Rt, ctx);
}

void casalh(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 1, Rs, 1, XZR, Rn, Rt, ctx);
}

void cash(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void caslh(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void casp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCasPr(w, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void caspa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCasPr(w, 1, Rs, 0, XZR, Rn, Rt, ctx);
}

void caspal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCasPr(w, 1, Rs, 1, XZR, Rn, Rt, ctx);
}

void caspl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCasPr(w, 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void casa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas((2 | w), 1, Rs, 0, XZR, Rn, Rt, ctx);
}

void casal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas((2 | w), 1, Rs, 1, XZR, Rn, Rt, ctx);
}

void cas_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas((2 | w), 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void casl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas((2 | w), 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void cbnz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx) {
  encodeCmpBr(w, 1, lbl, Rt, ctx);
}

void cbz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx) {
  encodeCmpBr(w, 0, lbl, Rt, ctx);
}

void ccmn_(uint1 w, armReg Rn, armCond cnd, uint8 nzcv, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm: {
      uint32 ins = one_bt(w, 31) | two_bt(1, 29) | ayt_bt(0xd2, 21) | fiv_bt(imm, 16) |
                   for_bt(cnd, 12) | one_bt(1, 11) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
      emitU32(ctx, ins);
      return;
    }
    case reg: {
      uint32 ins = one_bt(w, 31) | two_bt(1, 29) | ayt_bt(0xd2, 21) | fiv_bt(S2.reg, 16) |
                   for_bt(cnd, 12) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
      emitU32(ctx, ins);
      return;
    }
    default:
      assert(False);
  }
}

void ccmp_(uint1 w, armReg Rn, armCond cnd, uint8 nzcv, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm: {
      uint32 ins = one_bt(w, 31) | two_bt(3, 29) | ayt_bt(0xd2, 21) | fiv_bt(imm, 16) |
                   for_bt(cnd, 12) | one_bt(1, 11) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
      emitU32(ctx, ins);
      return;
    }
    case reg: {
      uint32 ins = one_bt(w, 31) | two_bt(3, 29) | ayt_bt(0xd2, 21) | fiv_bt(S2.reg, 16) |
                   for_bt(cnd, 12) | fiv_bt(Rn, 5) | for_bt(nzcv, 0);
      emitU32(ctx, ins);
      return;
    }
    default:
      assert(False);
  }
}

static uint8 twistCond(armCond cond) {
  return (((uint8) cond) & 0xfe) | ((~(uint8) cond) & 0x1);
}

void cinc_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx) {
  check(Rn != XZR, "invalid register");
  check((cond & 0xe0) != 0xe0, "invalid condition");

  encodeCnd3Reg(w, 0, 0, 0xd4, Rn, twistCond(cond), 1, Rn, Rd, ctx);
}

void cinv_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx) {
  check(Rn != XZR, "invalid register");
  check((cond & 0xe0) != 0xe0, "invalid condition");

  encodeCnd3Reg(w, 1, 0, 0xd4, Rn, twistCond(cond), 0, Rn, Rd, ctx);
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

void cmn_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encode3Reg7Imm(w, 0, 1, 0xb, 0, 0, S2.reg, 0, Rn, 0x1f, ctx);
      return;
    case imm: {
      uint16 shifted = ((S2.immediate & 0xfff) == 0) ? S2.immediate >> 12 : (S2.immediate & 0xfff);
      encodeDPRegImm(w, 1, 0x22, (S2.immediate & 0xfff) == 0, shifted, Rn, 0x1f, ctx);
      return;
    }
    case lsli:
      encode3Reg7Imm(w, 0, 1, 0xb, LSL, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case lsri:
      encode3Reg7Imm(w, 0, 1, 0xb, LSR, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case asri:
      encode3Reg7Imm(w, 0, 1, 0xb, ASR, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 0, 1, 0x59, S2.reg, S2.ext, S2.immediate, Rn, 0x1f, ctx);
      return;
    default:
      assert(False);
  }
}

void cmp_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encode3Reg7Imm(w, 1, 1, 0xb, 0, 0, S2.reg, 0, Rn, 0x1f, ctx);
      return;
    case imm: {
      uint16 shifted = ((S2.immediate & 0xfff) == 0) ? S2.immediate >> 12 : (S2.immediate & 0xfff);
      encodeDPRegImm(w, 3, 0x22, (S2.immediate & 0xfff) == 0, shifted, Rn, 0x1f, ctx);
      return;
    }
    case lsli:
      encode3Reg7Imm(w, 1, 1, 0xb, LSL, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case lsri:
      encode3Reg7Imm(w, 1, 1, 0xb, LSR, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case asri:
      encode3Reg7Imm(w, 1, 1, 0xb, ASR, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 1, 1, 0x59, S2.reg, S2.ext, S2.immediate, Rn, 0x1f, ctx);
      return;
    default:
      assert(False);
  }
}

void cmpp_(armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(1, 0, 1, 0xd6, Rn, 0, Rm, 0x1f, ctx);
}

void cneg_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, Rn, twistCond(cond), 1, Rn, Rd, ctx);
}

void csel_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 0, 0, 0xd4, Rm, cond, 0, Rn, Rd, ctx);
}

void cset_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 0, 0, 0xd4, 0x1f, twistCond(cond), 1, 0x1f, Rd, ctx);
}

void csetm_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, 0x1f, twistCond(cond), 0, 0x1f, Rd, ctx);
}

void csinc_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 0, 0, 0xd4, Rm, cond, 1, Rn, Rd, ctx);
}

void csinv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, Rm, cond, 0, Rn, Rd, ctx);
}

void csneg_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx) {
  encodeCnd3Reg(w, 1, 0, 0xd4, Rm, cond, 1, Rn, Rd, ctx);
}

void eon_sh_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armShift sh, uint8 imm, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xa, sh, 1, Rm, imm, Rn, Rd, ctx);
}

void eor_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:
      encodeLogImm(w, 2, S2.immediate, Rn, Rd, ctx);
      return;
    case reg:
      encodeShift3Reg(w, 1, 0, 0xa, LSL, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    case lsli:
      encodeShift3Reg(w, 1, 0, 0xa, LSL, 0, S2.reg, (uint8)(S2.immediate), Rn, Rd, ctx);
      return;
    case lsri:
      encodeShift3Reg(w, 1, 0, 0xa, LSR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case asri:
      encodeShift3Reg(w, 1, 0, 0xa, ASR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case rori:
      encodeShift3Reg(w, 1, 0, 0xa, ROR, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      assert(False);
  }
}

void extr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, uint8 lsb, assemCtxPo ctx) {
  encodeExtrct(w, 0, w, 0, Rm, lsb, Rn, Rd, ctx);
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

void encodeIxRegPr(uint8 opc, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, ixMode ix, assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      encodeLdStPrPostIx(opc, V, L, imm, Rt2, Rn, Rt, ctx);
      return;
    }
    case preIndex: {
      encodeLdStPrPreIx(opc, V, L, imm, Rt2, Rn, Rt, ctx);
      return;
    }
    case signedOff: {
      encodeLdStPrOffset(opc, V, L, imm, Rt2, Rn, Rt, ctx);
      return;
    }
    default: {
      check(False, "invalid index mode");
    }
  }
}

void ldnp(uint1 w, armReg Rt, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx) {
  encodeLdStNoAl(one_bt(w, 1), 0, 1, imm, Rt2, Rn, Rt, ctx);
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

void ldr_lit(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx) {
  encodeLdPcLit(w, 0, lbl, Rt, ctx);
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
  encodeLdPcLit(2, 0, lbl, Rt, ctx);
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

void ldxap_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 1, XZR, 1, Rt2, Rn, Rd, ctx);
}

void ldxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 1, XZR, 0, Rt2, Rn, Rd, ctx);
}

void ldxr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr((2 | w), 1, XZR, 0, XZR, Rn, Rd, ctx);
}

void ldapur_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnscaled((2 | w), 1, imm, Rn, Rd, ctx);
}

void ldapurb_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdRegLit(0, 1, imm, Rn, Rd, ctx);
}

void ldapursb_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdRegLit(0, (2 | ~w), imm, Rn, Rd, ctx);
}

void ldapursw_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnscaled(2, 2, imm, Rn, Rd, ctx);
}

void ldapurh_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdRegLit(1, 1, imm, Rn, Rd, ctx);
}

void ldapursh_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdRegLit(0, (2 | ~w), imm, Rn, Rd, ctx);
}

void ldaxr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr((2 | w), 1, XZR, 1, XZR, Rn, Rd, ctx);
}

void ldxrb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStRg(0, 1, XZR, 0, XZR, Rn, Rt, ctx);
}

void ldaxrb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStRg(0, 1, XZR, 1, XZR, Rn, Rt, ctx);
}

void ldxrh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStRg(1, 1, XZR, 0, XZR, Rn, Rt, ctx);
}

void ldaxrh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStRg(1, 1, XZR, 1, XZR, Rn, Rt, ctx);
}

void ldarb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(0, 1, XZR, 1, XZR, Rn, Rt, ctx);
}

void ldlar_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd((2 | w), 1, XZR, 1, XZR, Rn, Rt, ctx);
}

void ldlarb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(0, 1, XZR, 0, XZR, Rn, Rt, ctx);
}

void ldarh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(1, 1, XZR, 1, XZR, Rn, Rt, ctx);
}

void ldlarh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(1, 1, XZR, 0, XZR, Rn, Rt, ctx);
}

void lsl_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, Rm, 0x8, Rn, Rd, ctx);
}

void lsl_imm(uint1 w, armReg Rd, armReg Rn, uint8 shift, assemCtxPo ctx) {
  uint8 mod = (w ? 64 : 32);
  ubfm_(w, Rd, Rn, (-shift % mod), (mod - 1 - shift), ctx);
}

void lsr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, Rm, 0x9, Rn, Rd, ctx);
}

void lsr_imm(uint1 w, armReg Rd, armReg Rn, uint8 shift, assemCtxPo ctx) {
  ubfm_(w, Rd, Rn, shift, (0x1f | one_bt(w, 5)), ctx);
}

void madd_(uint1 w, armReg Rd, armReg Rm, armReg Rn, armReg Ra, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 0, Ra, Rn, Rd, ctx);
}

void mneg_(uint1 w, armReg Rd, armReg Rm, armReg Rn, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 1, XZR, Rn, Rd, ctx);
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
  encodeDPRegImm(w, 1, 0x24, (imm >> 12) & 1, imm, XZR, Rd, ctx);
}

void mov_r(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, 0, 0, Rm, 0, XZR, Rd, ctx);
}

void movk_(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeMovWide(w, 3, sh, imm, Rd, ctx);
}

void movn_imm(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeMovWide(w, 0, sh, imm, Rd, ctx);
}

void movz_(uint1 w, armReg Rd, uint8 sh, int16 imm, assemCtxPo ctx) {
  encodeMovWide(w, 2, sh, imm, Rd, ctx);
}

void mul_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 0, XZR, Rn, Rd, ctx);
}

void mvn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 1, Rm, amnt, XZR, Rd, ctx);
}

void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xb, sh, 0, Rm, amnt, XZR, Rd, ctx);
}

void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 1, 0xb, sh, 0, Rm, amnt, XZR, Rd, ctx);
}

void orn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 1, Rm, amnt, XZR, Rd, ctx);
}

void orr_imm(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLogImm(w, 1, imm, Rn, Rd, ctx);
}

void orr_sh_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 0, Rm, amnt, Rn, Rd, ctx);
}

void rbit_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encode4Reg(w, 2, 0xd6, 0, 0, 0, Rn, Rd, ctx);
}

void ret_(armReg Rn, assemCtxPo ctx) {
  encodeBranch(2, 0x1f, 0, Rn, 0, ctx);
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

void ror_imm(uint1 w, armReg Rd, armReg Rn, int16 amnt, assemCtxPo ctx) {
  extr_(w, Rd, Rn, Rn, amnt, ctx);
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

void sbfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx) {
  encodeImmRegReg(w, 0, w, immr, imms, Rn, Rd, ctx);
}

void ubfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx) {
  encodeImmRegReg(w, 2, w, immr, imms, Rn, Rd, ctx);
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
  encode4Reg(1, 0, 0xd9, Rm, 1, XZR, Rn, Rd, ctx);
}

void smsubl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 1, Ra, Rn, Rd, ctx);
}

void smmulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xda, Rm, 0, XZR, Rn, Rd, ctx);
}

void smmull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xda, Rm, 0, XZR, Rn, Rd, ctx);
}

void stllrb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(0, 0, XZR, 0, XZR, Rn, Rt, ctx);
}

void stllrh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(1, 0, XZR, 0, XZR, Rn, Rt, ctx);
}

void stllr_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd((2 | w), 0, XZR, 0, XZR, Rn, Rt, ctx);
}

void stlr_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd((2 | w), 0, XZR, 1, XZR, Rn, Rt, ctx);
}

void stlrb_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(0, 0, XZR, 1, XZR, Rn, Rt, ctx);
}

void stlrh_(armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeLdStOrd(1, 0, XZR, 1, XZR, Rn, Rt, ctx);
}

void stlur_(uint1 w, armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeLdRegLit((2 | w), 0, imm, Rn, Rd, ctx);
}

void stlurb_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeLdRegLit(0, 0, imm, Rn, Rd, ctx);
}

void stlurh_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeLdRegLit(1, 0, imm, Rn, Rd, ctx);
}

void stlxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg((2 | w), 0x8, 0, Rs, 1, 0x1f, Rn, Rt, ctx);
}

void stlxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(0, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void stxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(1, 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void stlxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(1, 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void stnp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx) {
  encodeLdStNoAl(one_bt(w, 1), 0, 0, imm, Rt2, Rn, Rt, ctx);
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

void ldtr_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv((2 | w), 0, 1, imm, Rn, Rt, ctx);
}

void ldtrb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv(0, 0, 1, imm, Rn, Rt, ctx);
}

void ldtrh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv(1, 0, 1, imm, Rn, Rt, ctx);
}

void ldtrsh_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv(1, 0, (2 | ~w), imm, Rn, Rt, ctx);
}

void sttr_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv((2 | w), 0, 0, imm, Rn, Rt, ctx);
}

void sttrb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv(0, 0, 0, imm, Rn, Rt, ctx);
}

void sttrh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnPriv(1, 0, 0, imm, Rn, Rt, ctx);
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

void stlxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 0, XZR, 1, Rt2, Rn, Rd, ctx);
}

void stxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 0, XZR, 0, Rt2, Rn, Rd, ctx);
}

void stxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr((2 | w), 0, Rs, 0, Rn, Rn, Rt, ctx);
}

void stxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(0, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void sub_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, int8 amnt, assemCtxPo ctx) {
  encode2SrcExt(w, 1, 0, 0x59, Rm, ex, amnt, Rn, Rd, ctx);
}

void sub_sh(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 imm, assemCtxPo ctx) {
  encodeShift3Reg(w, 1, 0, 0xb, sh, 0, Rm, imm, Rn, Rd, ctx);
}

void sub_imm(uint1 w, armReg Rd, armReg Rn, uint1 sh, int16 imm, assemCtxPo ctx) {
  encodeAddSubImm(w, 1, 0, 0, sh, imm, Rn, Rd, ctx);
}

void subs_imm(uint1 w, armReg Rd, armReg Rn, uint1 sh, int16 imm, assemCtxPo ctx) {
  encodeAddSubImm(w, 1, 1, 0, sh, imm, Rn, Rd, ctx);
}

void tbnz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx) {
  encodeTstBr(w, 1, pos, lbl, Rt, ctx);
}

void tbz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx) {
  encodeTstBr(w, 0, pos, lbl, Rt, ctx);
}
