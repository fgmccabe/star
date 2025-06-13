/*
 * Arm 64 assembler
 * Intended to be called from C code
 */

#include "utils.h"

#include "arm64P.h"
#include "assem_encode.h"
#include "macros.h"

codeLblPo preamble(assemCtxPo ctx, int32 lclSize) {
  codeLblPo entry = defineLabel(ctx, ctx->pc);
  int32 stkAdjustment = ALIGNVALUE(lclSize, 16);
  stp(FP, X30, PRX(SP, -16));
  mov(FP, RG(SP));
  if (stkAdjustment != 0)
    sub(SP, SP, IM(stkAdjustment));
  return entry;
}

retCode postamble(assemCtxPo ctx) {
  mov(SP, RG(X29));
  ldp(X29, X30, PSX(SP, 16));
  ret(X30);
  return Ok;
}

void clearCodeCtxMaps(assemCtxPo ctx) {
}

void adc_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(wide, 0, 0, 0xd0, Rm, 0x0, Rn, rd, ctx);
}

void adcs_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(wide, 0, 1, 0xd0, Rm, 0x0, Rn, rd, ctx);
}

void add_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm: {  // Immediate value
      encodeAddSubImm(w, 0, 0, 0x22, (int32) S2.immediate, Rn, Rd, ctx);
      return;
    }
    case shft: // Shift mode
      encode3Reg7Imm(w, 0, 0, 0xb, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encode3Reg7Imm(w, 0, 0, 0xb, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 0, 0, 0x59, S2.reg, S2.ext, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (add)");
  }
}

void adds_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm: {  // Immediate value
      encodeAddSubImm(w, 0, 1, 0x22, (int32) S2.immediate, Rn, Rd, ctx);
      return;
    }
    case shft: // Shifting mode
      encode3Reg7Imm(w, 0, 1, 0xb, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encode3Reg7Imm(w, 0, 1, 0xb, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 0, 1, 0x59, S2.reg, S2.ext, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (adds)");
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
    case shft:
      encode3Reg7Imm(w, 0, 0, 0xa, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encodeShift3Reg(w, 0, 0, 0xa, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (and)");
  }
}

void ands_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:  // Immediate value
      encodeLogImm(w, 3, S2.immediate, Rn, Rd, ctx);
      return;
    case shft:
      encodeShift3Reg(w, 1, 1, 0xa, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encodeShift3Reg(w, 1, 1, 0xa, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (ands)");
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
      check(False, "unsupported address mode (asr)");
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
    case shft:
      encodeShift3Reg(w, 0, 0, 0xa, S2.shift, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (bic)");
  }
}

void bics_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encodeShift3Reg(w, 1, 1, 0xa, LSL, 1, S2.reg, 0, Rn, Rd, ctx);
      return;
    case shft:
      encodeShift3Reg(w, 1, 1, 0xa, S2.shift, 1, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (bics)");
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

void casab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 1, Rs, 0, XZR, Rn, Rt, ctx);
}

void casalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 1, Rs, 1, XZR, Rn, Rt, ctx);
}

void casb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void caslb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(0, 0, Rs, 1, XZR, Rn, Rt, ctx);
}

void casah_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 1, Rs, 0, XZR, Rn, Rt, ctx);
}

void casalh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 1, Rs, 1, XZR, Rn, Rt, ctx);
}

void cash_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeCas(1, 0, Rs, 0, XZR, Rn, Rt, ctx);
}

void caslh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
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
      check(False, "unsupported address mode (ccmn)");
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
      check(False, "unsupported address mode (ccmp)");
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
    case shft:
      encode3Reg7Imm(w, 0, 1, 0xb, S2.shift, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 0, 1, 0x59, S2.reg, S2.ext, S2.immediate, Rn, 0x1f, ctx);
      return;
    default:
      check(False, "unsupported address mode (cmn)");
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
    case shft:
      encode3Reg7Imm(w, 1, 1, 0xb, S2.shift, 0, S2.reg, S2.immediate, Rn, 0x1f, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 1, 1, 0x59, S2.reg, S2.ext, S2.immediate, Rn, 0x1f, ctx);
      return;
    default:
      check(False, "unsupported address mode (cmp)");
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
    case shft:
      encodeShift3Reg(w, 1, 0, 0xa, S2.shift, 0, S2.reg, (uint8) (S2.immediate), Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (eor)");
  }
}

void extr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, uint8 lsb, assemCtxPo ctx) {
  encodeExtrct(w, 0, w, 0, Rm, lsb, Rn, Rd, ctx);
}

void ldaddab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(0, 0, one, zero, one, Rs, 0, Rn, Rt, ctx);
}

void ldaddalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(0, 0, one, one, one, Rs, 0, Rn, Rt, ctx);
}

void ldaddb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(0, 0, zero, zero, one, Rs, 0, Rn, Rt, ctx);
}

void ldaddlb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(0, 0, zero, one, one, Rs, 0, Rn, Rt, ctx);
}

void ldadd_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(2 | w, 0, zero, zero, one, Rs, 0, Rn, Rt, ctx);
}

void ldadda_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(2 | w, 0, one, zero, one, Rs, 0, Rn, Rt, ctx);
}

void ldaddal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(2 | w, 0, one, one, one, Rs, 0, Rn, Rt, ctx);
}

void ldaddl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeALd3Reg(2 | w, 0, zero, one, one, Rs, 0, Rn, Rt, ctx);
}

void ld64b_(armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = fiv_bt(0x1f, 27) | ayt_bt(0xff, 14) | one_bt(1, 12) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void ldnp(uint1 w, armReg Rt, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx) {
  encodeLd3Reg(0, one_bt(w, 1), 0, 0, 0, 1, imm, Rt2, Rn, Rt, ctx);
}

void ldp_(uint1 w, armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case postX:
      encodeLd3Reg(w, 0, 0x5, 0, 1, 1, Sn.immediate >> (w + 2), Rt2, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLd3Reg(w, 0, 0x5, 0, 3, 1, Sn.immediate >> (w + 2), Rt2, Sn.reg, Rt, ctx);
      return;
    case sOff:
      encodeLd3Reg(w, 0, 0x5, 0, 2, 1, Sn.immediate >> (w + 2), Rt2, Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode (ldp)");
  }
}

void ldpsw_(armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case postX:
      encodeLd3Reg(0, 1, 0x5, 0, 1, 1, Sn.immediate >> 2, Rt2, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLd3Reg(0, 1, 0x5, 0, 3, 1, Sn.immediate >> 2, Rt2, Sn.reg, Rt, ctx);
      return;
    case sOff:
      encodeLd3Reg(0, 1, 0x5, 0, 2, 1, Sn.immediate >> 2, Rt2, Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode (ldpsw)");
  }
}

void ldr_(uint1 w, armReg Rt, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case postX:
      encodeLdStRegX((2 | w), 0, 1, (int16) (Sn.immediate), 1, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLdStRegX((2 | w), 0, 1, (int16) (Sn.immediate), 3, Sn.reg, Rt, ctx);
      return;
    case sOff: {
      if (Sn.immediate >= 0)
        encodeLdSt((2 | w), 0xe5, Sn.immediate >> (2 + w), Sn.reg, Rt, ctx);
      else
        encodeLdStUnPriv((2 | w), 0, 1, (int16) Sn.immediate, Sn.reg, Rt, ctx);
      return;
    }
    case pcRel:
      encodeLdPcLit(w, 0, Sn.lbl, Rt, ctx);
      return;
    case extnd:
      encodeSz2OpcImm3Reg((2 | w), 0x38, 1, 1, Sn.rgm, Sn.ext, (Sn.immediate != 0 ? one : zero), 2, Sn.reg,
                          Rt, ctx);
      return;
    case reg:
      encodeLdStUnPriv((2 | w), 0, 1, 0, Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode in ldr");
  }
}

void ldrb_(armReg Rt, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case postX:
      encode2SrcIxImmPrePost(0, 0xe, 0x1, postIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case preX:
      encode2SrcIxImmPrePost(0, 0xe, 0x1, preIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdSt(0, 0xe5, S2.immediate, S2.reg, Rt, ctx);
      return;
    case extnd:
      encodeSz2OpcImm3Reg(0, 0x38, 1, 1, S2.rgm, S2.ext, (S2.immediate != 0 ? one : zero), 2, S2.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode in ldrb");
  }
}

void ldrh_(armReg Rt, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case postX:
      encode2SrcIxImmPrePost(1, 0xe, 0x1, postIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case preX:
      encode2SrcIxImmPrePost(1, 0xe, 0x1, preIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdSt(1, 0xe5, S2.immediate >> 1, S2.reg, Rt, ctx);
      return;
    case extnd:
      encodeSz2OpcImm3Reg(1, 0x38, 1, 1, S2.rgm, S2.ext, (S2.immediate != 0 ? one : zero), 2, S2.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode in ldrh");
  }
}

void ldrsb_(uint1 w, armReg Rt, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case postX:
      encode2SrcIxImmPrePost(0, 0xe, (2 | w), postIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case preX:
      encode2SrcIxImmPrePost(0, 0xe, (2 | w), preIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case sOff:
      encode2SrcIxImmPrePost(0, 0xe, (2 | w), unsignedOff, S2.immediate, S2.reg, Rt, ctx);
      return;
    case extnd:
      encodeSz2OpcImm3Reg(0, 0x38, (2 | w), 1, S2.rgm, S2.ext, (S2.immediate != 0 ? one : zero), 2, S2.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode in ldrsb");
  }
}

void ldrsh_(uint1 w, armReg Rt, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case postX:
      encode2SrcIxImmPrePost(1, 0xe, (2 | w), postIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case preX:
      encode2SrcIxImmPrePost(1, 0xe, (2 | w), preIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case sOff:
      encode2SrcIxImmPrePost(1, 0xe, (2 | w), unsignedOff, S2.immediate >> 1, S2.reg, Rt, ctx);
      return;
    case extnd:
      encodeSz2OpcImm3Reg(1, 0x38, (2 | w), 1, S2.rgm, S2.ext, (S2.immediate != 0 ? one : zero), 2, S2.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode in ldrsh");
  }
}

void ldrsw_(armReg Rt, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case postX:
      encode2SrcIxImmPrePost(2, 0xe, 0x2, postIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case preX:
      encode2SrcIxImmPrePost(2, 0xe, 0x2, preIndex, S2.immediate, S2.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdSt(2, 0xe6, S2.immediate >> 2, S2.reg, Rt, ctx);
      return;
    case extnd:
      encodeSz2OpcImm3Reg(2, 0x38, 2, 1, S2.rgm, S2.ext, (S2.immediate != 0 ? one : zero), 2, S2.reg, Rt, ctx);
      return;
    case pcRel:
      encodeLdPcLit(2, 0, S2.lbl, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode in ldrsw");
  }
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
  encodeLdStRegUnscaled(0, 0, (2 | ~w), imm, Rn, Rt, ctx);
}

void ldursh_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(1, 0, (2 | ~w), imm, Rn, Rt, ctx);
}

void ldursw_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStRegUnscaled(2, 0, 2, imm, Rn, Rt, ctx);
}

void ldxap_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 1, one, XZR, 1, Rt2, Rn, Rd, ctx);
}

void ldxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 1, one, XZR, 0, Rt2, Rn, Rd, ctx);
}

void ldxr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 1, 0, XZR, 0, XZR, Rn, Rd, ctx);
}

void ldapur_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx) {
  encodeLdStUnscaled(w, 1, imm, Rn, Rd, ctx);
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
  encodeExLdStPr((2 | w), 1, one, XZR, 1, XZR, Rn, Rd, ctx);
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

void lsl_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encodeReg2Src(w, 0, 0, 0xd6, S2.reg, 0x8, Rn, Rd, ctx);
      return;
    case imm: {
      uint8 mod = (w ? 64 : 32);
      ubfm_(w, Rd, Rn, (-S2.immediate % mod), (mod - 1 - S2.immediate), ctx);
      return;
    }
    default:
      check(False, "unsupported address mode in lsl");
  }
}

void lsr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case reg:
      encodeReg2Src(w, 0, 0, 0xd6, S2.reg, 0x9, Rn, Rd, ctx);
      return;
    case imm: {
      ubfm_(w, Rd, Rn, S2.immediate, (0x1f | one_bt(w, 5)), ctx);
      return;
    }
    default:
      check(False, "unsupported address mode in lsr");
  }
}

void madd_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 0, Ra, Rn, Rd, ctx);
}

void mneg_(uint1 w, armReg Rd, armReg Rm, armReg Rn, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 1, XZR, Rn, Rd, ctx);
}

void mov_(armReg Rd, FlexOp S1, assemCtxPo ctx) {
  switch (S1.mode) {
    case reg:
      encodeDPRegImm(1, 0, 0x22, 0, 0, S1.reg, Rd, ctx);
      return;
    case imm: {
      uint8 sh;
      uint16 imm16;
      if ((S1.immediate & 0xffffffffffff) == 0) {
        imm16 = S1.immediate >> 48;
        sh = 3;
      } else if ((S1.immediate & 0xffffffff) == 0 && (S1.immediate & 0xffff00000000) == S1.immediate) {
        imm16 = S1.immediate >> 32;
        sh = 2;
      } else if ((S1.immediate & 0xffff) == 0 && (S1.immediate & 0xffff0000) == S1.immediate) {
        imm16 = S1.immediate >> 16;
        sh = 1;
      } else if ((S1.immediate & 0xffff) == S1.immediate) {
        imm16 = S1.immediate;
        sh = 0;
      } else {
        uint16 chunk1 = (S1.immediate >> 48);
        uint16 chunk2 = (S1.immediate >> 32) & 0xffff;
        uint16 chunk3 = (S1.immediate >> 16) & 0xffff;
        uint16 chunk4 = S1.immediate & 0xffff;

        encodeImm1Reg(1, 2, 0x25, 0, chunk4, Rd, ctx);
        if (chunk3 != 0)
          encodeImm1Reg(1, 3, 0x25, 1, chunk3, Rd, ctx);
        if (chunk2 != 0)
          encodeImm1Reg(1, 3, 0x25, 2, chunk2, Rd, ctx);
        if (chunk1 != 0)
          encodeImm1Reg(1, 3, 0x25, 3, chunk1, Rd, ctx);
        return;
      }

      encodeImm1Reg(1, 2, 0x25, sh, (int16) imm16, Rd, ctx);
      return;
    }
    default:
      check(False, "unsupported address mode in mov");
  }
}

void msub_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(w, 0, 0xd8, Rm, 1, Ra, Rn, Rd, ctx);
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

void ngc_(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx) {
  encodeImm3Reg(w, 2, 0xd0, Rm, 0, XZR, Rd, ctx);
}

void ngcs_(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx) {
  encodeImm3Reg(w, 3, 0xd0, Rm, 0, XZR, Rd, ctx);
}

void nop(assemCtxPo ctx) {
  encodeImm3Reg(1, 2, 0xa8, 0x3, 0x8, 0, 0x1f, ctx);
}

void orn_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx) {
  encodeShift3Reg(w, 0, 1, 0xa, sh, 1, Rm, amnt, Rn, Rd, ctx);
}

void orr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:
      encodeLogImm(w, 1, S2.immediate, Rn, Rd, ctx);
      return;
    case reg:
      encodeShift3Reg(w, 0, 1, 0xa, LSL, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    case shft:
      encodeShift3Reg(w, 0, 1, 0xa, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode in mov");
  }
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

void rev32_(armReg Rd, armReg Rn, assemCtxPo ctx) {
  encode2SrcExt(1, 1, 0, 0xd6, 0, 0, 2, Rn, Rd, ctx);
}

void ror_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm:
      encodeExtrct(w, 0, w, 0, Rn, S2.immediate, Rn, Rd, ctx);
      return;
    case reg:
      encodeReg2Src(w, 0, 0, 0xd6, S2.reg, 0x0b, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (asr)");
  }
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

void smaddl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 0, Ra, Rn, Rd, ctx);
}

void smnegl_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 1, XZR, Rn, Rd, ctx);
}

void smsubl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 1, Ra, Rn, Rd, ctx);
}

void smulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xda, Rm, 0, XZR, Rn, Rd, ctx);
}

void smull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xd9, Rm, 0, XZR, Rn, Rd, ctx);
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
  encodeLdRegLit((2 | w), 0, (int16) imm, Rn, Rd, ctx);
}

void stlurb_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeLdRegLit(0, 0, (int16) imm, Rn, Rd, ctx);
}

void stlurh_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx) {
  encodeLdRegLit(1, 0, (int16) imm, Rn, Rd, ctx);
}

void stlxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeSz4Reg((2 | w), 0x8, 0, Rs, 1, 0x1f, Rn, Rt, ctx);
}

void stlxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(0, 0, one, Rs, 0, XZR, Rn, Rt, ctx);
}

void stxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(1, 0, one, Rs, 1, XZR, Rn, Rt, ctx);
}

void stlxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(1, 0, one, Rs, 1, XZR, Rn, Rt, ctx);
}

void stnp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx) {
  encodeLd3Reg(0, one_bt(w, 1), 0, 0, 0, 0, imm, Rt2, Rn, Rt, ctx);
}

void stp_(uint1 w, armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case postX:
      encodeLdStPrPostIx(one, 0, 0, 0, (int8) (Sn.immediate >> (w + 2)), Rt2, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLdStPrPreIx(w << 1, 0, 0, (int8) (Sn.immediate >> (w + 2)), Rt2, Sn.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdStPrOffset(w << 1, 2, 0, (int8) (Sn.immediate >> (w + 2)), Rt2, Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode (ldp)");
  }
}

void str_(uint1 w, armReg Rt, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case extnd:
      encodeSz2OpcImm3Reg((2 | w), 0x38, 0, 1, Sn.rgm, Sn.ext, Sn.immediate != 0, 2, Sn.reg, Rt, ctx);
      return;
    case postX:
      encodeLdStRegX((2 | w), 0, 0, (int16) Sn.immediate, 1, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLdStRegX((2 | w), 0, 0, (int16) Sn.immediate, 3, Sn.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdSt((2 | w), 0xe4, Sn.immediate >> (w + 2), Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode (str)");
  }
}

void strb_(armReg Rt, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case extnd:
      encodeSz2OpcImm3Reg(0, 0x38, 0, 1, Sn.rgm, Sn.ext, Sn.immediate != 0, 2, Sn.reg, Rt, ctx);
      return;
    case postX:
      encodeLdStRegX(0, 0, 0, (int16) Sn.immediate, 1, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLdStRegX(0, 0, 0, (int16) Sn.immediate, 3, Sn.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdSt(0, 0xe4, Sn.immediate, Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode (strb)");
  }
}

void strh_(armReg Rt, FlexOp Sn, assemCtxPo ctx) {
  switch (Sn.mode) {
    case extnd:
      encodeSz2OpcImm3Reg(1, 0x38, 0, 1, Sn.rgm, Sn.ext, Sn.immediate != 0, 2, Sn.reg, Rt, ctx);
      return;
    case postX:
      encodeLdStRegX(1, 0, 0, (int16) Sn.immediate, 1, Sn.reg, Rt, ctx);
      return;
    case preX:
      encodeLdStRegX(1, 0, 0, (int16) Sn.immediate, 3, Sn.reg, Rt, ctx);
      return;
    case sOff:
      encodeLdSt(1, 0xe4, Sn.immediate >> 1, Sn.reg, Rt, ctx);
      return;
    default:
      check(False, "unsupported address mode (strb)");
  }
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

void sub_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm: {  // Immediate value
      encodeAddSubImm(w, 1, 0, 0x22, (int32) S2.immediate, Rn, Rd, ctx);
      return;
    }
    case shft: // Shift mode
      encode3Reg7Imm(w, 1, 0, 0xb, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encode3Reg7Imm(w, 1, 0, 0xb, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 1, 0, 0x59, S2.reg, S2.ext, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (add)");
  }
}

void subs_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  switch (S2.mode) {
    case imm: {  // Immediate value
      encodeAddSubImm(w, 1, 1, 0x22, (int32) S2.immediate, Rn, Rd, ctx);
      return;
    }
    case shft: // Shift mode
      encode3Reg7Imm(w, 1, 1, 0xb, S2.shift, 0, S2.reg, S2.immediate, Rn, Rd, ctx);
      return;
    case reg: // register
      encode3Reg7Imm(w, 1, 1, 0xb, 0, 0, S2.reg, 0, Rn, Rd, ctx);
      return;
    case extnd:
      encode2SrcExt(w, 1, 1, 0x59, S2.reg, S2.ext, S2.immediate, Rn, Rd, ctx);
      return;
    default:
      check(False, "unsupported address mode (add)");
  }
}

void sxtb(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  sbfm_(w, Rd, Rn, 0, 7, ctx);
}

void sxth(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx) {
  sbfm_(w, Rd, Rn, 0, 15, ctx);
}

void sxtw(armReg Rd, armReg Rn, assemCtxPo ctx) {
  sbfm_(1, Rd, Rn, 0, 31, ctx);
}

void stlxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 0, one, XZR, 1, Rt2, Rn, Rd, ctx);
}

void stxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(w, 0, one, XZR, 0, Rt2, Rn, Rd, ctx);
}

void stxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr((2 | w), 0, one, Rs, 0, Rn, Rn, Rt, ctx);
}

void stxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx) {
  encodeExLdStPr(0, 0, one, Rs, 0, XZR, Rn, Rt, ctx);
}

void tbnz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx) {
  encodeTstBr(w, 1, pos, lbl, Rt, ctx);
}

void tbz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx) {
  encodeTstBr(w, 0, pos, lbl, Rt, ctx);
}

void tst_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx) {
  ands_(w, XZR, Rn, S2, ctx);
}

void udiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encodeReg2Src(w, 0, 0, 0xd6, Rm, 2, Rn, Rd, ctx);
}

void umaddl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xdd, Rm, 0, Ra, Rn, Rd, ctx);
}

void umnegl_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xdd, Rm, 1, XZR, Rn, Rd, ctx);
}

void umsubl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xdd, Rm, 1, Ra, Rn, Rd, ctx);
}

void umulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xde, Rm, 0, XZR, Rn, Rd, ctx);
}

void umull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx) {
  encode4Reg(1, 0, 0xdd, Rm, 0, XZR, Rn, Rd, ctx);
}

void fadd_(Precision p, fpReg Rd, fpReg Rn, fpReg Rm, assemCtxPo ctx) {
  encodeScalarOp(p, 0b001010, Rm, Rn, Rd, ctx);
}

void fsub_(Precision p, fpReg Rd, fpReg Rn, fpReg Rm, assemCtxPo ctx) {
  encodeScalarOp(p, 0b001110, Rm, Rn, Rd, ctx);
}

void fmul_(Precision p, fpReg Rd, fpReg Rn, fpReg Rm, assemCtxPo ctx) {
  encodeScalarOp(p, 0b000010, Rm, Rn, Rd, ctx);
}

void fdiv_(Precision p, fpReg Rd, fpReg Rn, fpReg Rm, assemCtxPo ctx) {
  encodeScalarOp(p, 0b000110, Rm, Rn, Rd, ctx);
}

void fmov_(Precision p, FlexOp d, FlexOp s, assemCtxPo ctx) {
  switch (d.mode) {
    case reg: {
      switch (s.mode) {
        case fp: {
          encodeFpMovOp(1, p, 0, 0b11000, s.fp, d.reg, ctx);
          return;
        }
        case imm:
        default:
          check(False, "unsupported address mode (fmov)");
      }
    }
    case fp: {
      switch (s.mode) {
        case reg: {
          encodeFpMovOp(1, p, 0, 0b11100, s.reg, d.fp, ctx);
          return;
        }
        case fp: {
          encodeFpMovOp(0, p, 0, 1, s.fp, d.fp, ctx);
          return;
        }
        case imm: {
          uint32 ins = ayt_bt(0b0011110, 24) | two_bt(p, 22) | one_bt(0b1, 21) |
                       ayt_bt(s.immediate, 13) | one_bt(1, 12) | fiv_bt(d.fp, 0);
          emitU32(ctx, ins);
          return;
        }
        default:
          check(False, "unsupported address mode (fmov)");
      }
    }
    default:
      check(False, "unsupported address mode (fmov)");
  }
}

logical sameFlexOp(FlexOp a, FlexOp b) {
  return a.mode == b.mode && a.rgm == b.rgm && a.ext == b.ext && a.shift == b.shift && a.immediate == b.immediate && a.
    reg == b.reg;
}

logical isRegisterOp(FlexOp a) {
  return a.mode == reg;
}
