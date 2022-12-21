//
// Created by Francis McCabe on 12/15/22.
//

#include "assem_encode.h"
#include <assert.h>

static void updateRelPc(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  assert(isLabelDefined(lbl));
  integer delta = labelTgt(lbl) - (pc + PLATFORM_PC_DELTA);
  uint32 oldIns = readCtxAtPc(ctx, pc);
  uint32 newIns =
    one_bt((oldIns >> 31), 31) | two_bt(delta, 29) | one_bt((oldIns >> 28u), 28) | ntn_bt(delta >> 2, 5) |
    fiv_bt(oldIns, 0);
  updateU32(ctx, pc, newIns);
}

void encodePCRel(uint1 op, codeLblPo lbl, armReg Rd, assemCtxPo ctx) {
  if (!isLabelDefined(lbl)) {
    addLabelReference(ctx, lbl, ctx->pc, updateRelPc);
    emitU32(ctx, one_bt(op, 31) | one_bt(1, 28) | fiv_bt(Rd, 0)); // The rest of the instruction
  } else {
    integer delta = lblDeltaRef(ctx, lbl);
    check(absolute(delta >> 2) < (1 << 19), "label out of range");
    emitU32(ctx, one_bt(op, 31) | two_bt(delta, 29) | one_bt(1, 28) | ntn_bt(delta >> 2, 5) | fiv_bt(Rd, 0));
  }
}

void
encodeReg2Src(uint1 wide, uint1 o, uint1 S, uint8 op1, armReg Rm, uint8 op2, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30)
               | one_bt(S, 29) | ayt_bt(op1, 21) | fiv_bt(Rm, 16) |
               six_bt(op2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encode4Reg(uint1 w, uint8 opc, uint8 op, armReg Rm, uint1 o0, armReg Ra, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) | one_bt(o0, 15) |
               fiv_bt(Ra, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeSz4Reg(uint8 sz, uint8 op, uint8 L, armReg Rm, uint1 o0, armReg Ra, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | six_bt(op, 24) | thr_bt(L, 21) |
               fiv_bt(Rm, 16) | one_bt(o0, 15) |
               fiv_bt(Ra, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encode2SrcExt(uint1 wide, uint1 o, uint1 S, uint8 op, armReg Rm, armExtent ex, uint8 imm, armReg Rn, armReg Rd,
                   assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30) | one_bt(S, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) |
               thr_bt(ex, 13) | thr_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeDPRegImm(uint1 wide, uint8 opc, uint8 op, uint1 sh, uint16 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | two_bt(opc, 29) |
               six_bt(op, 23) | one_bt(sh, 22) | twl_bt(imm, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeImm1Reg(uint1 w, uint8 opc, uint8 op, uint8 hw, int16 imm, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | six_bt(op, 23) | two_bt(hw, 21) |
               sxt_bt(imm, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeAddSubImm(uint1 w, uint1 op, uint1 S, uint8 code, int32 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint1 sh;
  if ((imm & 0xfff) == 0) {
    sh = 1;
    imm = imm >> 12;
  } else {
    sh = 0;
    imm = imm & 0xfff;
  }
  uint32 ins = one_bt(w, 31) | one_bt(op, 30) | one_bt(S, 29) | six_bt(code, 23) |
               one_bt(sh, 22) | twl_bt(imm, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static inline uint64 rotRight(uint64 x, uint8 amnt) {
  return (x >> amnt) | (x << (64 - amnt));
}

static inline uint64 rotLeft(uint64 x, uint8 amnt) {
  return (x << amnt) | (x >> (64 - amnt));
}

typedef struct {
  uint1 N;
  uint8 imms;
  uint8 immr;
} BitMaskLiteral;

uint8 immsMasks[] = {0x3c, 0x38, 0x30, 0x20, 0, 0x40};

static BitMaskLiteral encodeBitMask(uint64 val) {
  assert(val > 0);
  int rot = 0;

  uint64 vl = (uint64) val;
  while ((vl & 1) == 0) {
    rot++;
    vl = rotRight(vl, 1);
  }
  uint8 immr = (64 - rot) % 64;
  uint64 lowerMask = -1;

  // Determine what kind of pattern we have
  uint64 ptnSize = 64;
  uint64 ptn = vl;
  while (ptnSize > 2) {
    lowerMask >>= (ptnSize >> 1);
    uint64 upper = (ptn >> (ptnSize >> 1)) & lowerMask;
    uint64 lower = ptn & lowerMask;
    if (lower != upper)
      break;
    else {
      ptnSize >>= 1;
      ptn = lower;
    }
  }
  uint8 immsMask = immsMasks[lg2(ptnSize) - 1];

  uint8 count = countBits(ptn) - 1;

  BitMaskLiteral literl = {.N=immsMask >> 6, .imms=(immsMask & 0x3c) | count, .immr = immr};
  return literl;
}

void encodeLogImm(uint1 w, uint8 opc, uint64 val, armReg Rn, armReg Rd, assemCtxPo ctx) {
  BitMaskLiteral literl = encodeBitMask(val);
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | six_bt(0x24, 23) |
               one_bt(literl.N, 22) | six_bt(literl.immr, 16) | six_bt(literl.imms, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeMovWide(uint1 w, uint8 opc, uint8 hw, int16 imm, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | six_bt(0x25, 23) |
               two_bt(hw, 21) | sxt_bt(imm, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeImmRegReg(uint1 w, uint8 opc, uint1 N, uint8 immr, uint8 imms, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | six_bt(0x26, 23) | one_bt(N, 22) |
               six_bt(immr, 16) | sxt_bt(imms, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeExtrct(uint1 w, uint8 opc, uint1 N, uint1 o0, armReg Rm, uint8 imms, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | two_bt(opc, 29) | six_bt(0x27, 23) | one_bt(N, 22) | one_bt(o0, 21) |
               fiv_bt(Rm, 16) | sxt_bt(imms, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

static void updateCondPc(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  assert(isLabelDefined(lbl));
  integer delta = (integer) labelTgt(lbl) - (pc + PLATFORM_PC_DELTA);
  uint32 oldIns = readCtxAtPc(ctx, pc);
  uint32 newIns =
    ayt_bt((oldIns >> 24), 24) | ntn_bt(delta >> 2, 5) | fiv_bt(oldIns, 0);
  updateU32(ctx, pc, newIns);
}

void encodeCondBrnch(uint8 op, uint1 o1, codeLblPo lbl, armCond cond, assemCtxPo ctx) {
  if (!isLabelDefined(lbl)) {
    addLabelReference(ctx, lbl, ctx->pc, updateCondPc);
    emitU32(ctx, ((uint32) (six_bt(op, 25) | for_bt(cond, 0)))); // The rest of the instruction
  } else {
    integer delta = lblDeltaRef(ctx, lbl);
    check(absolute(delta >> 2) < (1 << 19), "label out of range");
    emitU32(ctx, ((uint32) (six_bt(op, 25) | ntn_bt(delta >> 2, 5) | for_bt(cond, 0))));
  }
}

void encodeBranch(uint8 opc, uint8 op2, uint8 op3, armReg Rn, uint8 op4, assemCtxPo ctx) {
  uint32 ins = svn_bt(0x6b, 25) | for_bt(opc, 21) | fiv_bt(op2, 16) | six_bt(op3, 10) | fiv_bt(Rn, 5) | fiv_bt(op4, 0);
  emitU32(ctx, ins);
}

static void updateBImm(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  assert(isLabelDefined(lbl));
  integer delta = (integer) labelTgt(lbl) - (pc + PLATFORM_PC_DELTA);
  uint32 oldIns = readCtxAtPc(ctx, pc);
  uint32 newIns =
    six_bt((oldIns >> 26), 26) | tsx_bt(delta >> 2, 0);
  updateU32(ctx, pc, newIns);
}

void encodeBranchImm(uint1 op, codeLblPo lbl, assemCtxPo ctx) {
  if (!isLabelDefined(lbl)) {
    addLabelReference(ctx, lbl, ctx->pc, updateBImm);
    emitU32(ctx, one_bt(op, 31) | fiv_bt(5, 26)); // The rest of the instruction
  } else {
    integer delta = lblDeltaRef(ctx, lbl);
    check(absolute(delta >> 2) < (1 << 26), "label out of range");
    uint32 ins = one_bt(op, 31) | fiv_bt(0x5, 26) | tsx_bt(delta >> 2, 0);
    emitU32(ctx, ins);
  }
}

void encodeCmpBr(uint1 b5, uint1 op, codeLblPo lbl, armReg Rt, assemCtxPo ctx) {
  if (!isLabelDefined(lbl)) {
    addLabelReference(ctx, lbl, ctx->pc, updateCondPc);
    emitU32(ctx, one_bt(b5, 31) | fiv_bt(0x1a, 25) | one_bt(op, 24) | fiv_bt(Rt, 0)); // The rest of the instruction
  } else {
    integer delta = lblDeltaRef(ctx, lbl);
    check(absolute(delta >> 2) < (1 << 19), "label out of range");
    uint32 ins = one_bt(b5, 31) | fiv_bt(0x1a, 25) | one_bt(op, 24) |
                 ntn_bt(delta >> 2, 5) | fiv_bt(Rt, 0);
    emitU32(ctx, ins);
  }
}

static void updateTestPc(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  assert(isLabelDefined(lbl));
  integer delta = (integer) labelTgt(lbl) - (pc + PLATFORM_PC_DELTA);
  uint32 oldIns = readCtxAtPc(ctx, pc);
  uint32 newIns =
    thi_bt((oldIns >> 19), 19) | ftn_bt(delta >> 2, 5) | fiv_bt(oldIns, 0);
  updateU32(ctx, pc, newIns);
}

void encodeTstBr(uint1 w, uint1 op, uint8 b40, codeLblPo lbl, armReg Rt, assemCtxPo ctx) {
  uint1 ww = (b40>=32?1:0);
  if (!isLabelDefined(lbl)) {
    addLabelReference(ctx, lbl, ctx->pc, updateTestPc);
    emitU32(ctx, one_bt(ww, 31) | six_bt(0x1b, 25) | one_bt(op, 24) | fiv_bt(b40, 19) |
                 ftn_bt(0, 5) | fiv_bt(Rt, 0)); // The rest of the instruction
  } else {
    integer delta = lblDeltaRef(ctx, lbl);
    check(absolute(delta >> 2) < (1 << 15), "label out of range");
    uint32 ins = one_bt(ww, 31) | six_bt(0x1b, 25) | one_bt(op, 24) | fiv_bt(b40, 19) |
                 ftn_bt(delta>>2, 5) | fiv_bt(Rt, 0);
    emitU32(ctx, ins);
  }
}

void encodeCasPr(uint8 w, uint1 L, armReg Rs, uint o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 30) | svn_bt(0x10, 23) | one_bt(L, 22) | one_bt(1, 21) |
               fiv_bt(Rs, 16) | one_bt(o0, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeCas(uint8 w, uint1 L, armReg Rs, uint o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(w, 30) | svn_bt(0x11, 23) | one_bt(L, 22) | one_bt(1, 21) |
               fiv_bt(Rs, 16) | one_bt(o0, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeExLdStPr(uint1 w, uint1 L, uint1 L2, armReg Rs, uint1 o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = one_bt(1, 31) | one_bt(w, 30) | svn_bt(0x10, 23) | one_bt(L, 22) | one_bt(L2, 21) |
               fiv_bt(Rs, 16) | one_bt(o0, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeExLdStRg(uint8 w, uint1 L, armReg Rs, uint1 o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(w, 30) | svn_bt(0x10, 23) | one_bt(L, 22) |
               fiv_bt(Rs, 16) | one_bt(o0, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdSt(uint8 sz, uint8 opc, uint16 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | ayt_bt(opc, 22) | twl_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeLdStOrd(uint8 w, uint1 L, armReg Rs, uint1 o0, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(w, 30) | svn_bt(0x11, 23) | one_bt(L, 22) |
               fiv_bt(Rs, 16) | one_bt(o0, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStUnscaled(uint8 w, uint8 opc, int16 imm9, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(w, 30) | six_bt(0x19, 24) | two_bt(opc, 22) |
               nin_bt(imm9, 12) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdPcLit(uint8 opc, uint1 V, codeLblPo lbl, armReg Rt, assemCtxPo ctx) {
  if (!isLabelDefined(lbl)) {
    addLabelReference(ctx, lbl, ctx->pc, updateCondPc);
    emitU32(ctx, two_bt(opc, 30) | thr_bt(3, 27) | one_bt(V, 26) | fiv_bt(Rt, 0));
  } else {
    integer delta = lblDeltaRef(ctx, lbl);
    check(absolute(delta >> 2) < (1 << 21), "label out of range");
    uint32 ins = two_bt(opc, 30) | thr_bt(3, 27) | one_bt(V, 26) |
                 ntn_bt(delta >> 2, 15) | fiv_bt(Rt, 0);
    emitU32(ctx, ins);
  }
}

void
encodeLd3Reg(uint8 w, uint1 w2, uint8 op1, uint1 V, uint8 op2, uint1 L, uint8 imm7, armReg Rt2, armReg Rn, armReg Rt,
             assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(w2, 30) | thr_bt(op1, 27) | one_bt(V, 26) | thr_bt(op2, 23) | one_bt(L, 22) |
               svn_bt(imm7, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStRegUnscaled(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStRegX(uint8 sz, uint1 V, uint8 opc, int16 imm, uint8 op2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | two_bt(op2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStRegPre(uint8 sz, uint1 V, uint8 opc, int16 imm, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm, 12) | two_bt(3, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdRegLit(uint8 sz, uint8 opc, int16 imm9, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | six_bt(0x19, 24) | two_bt(opc, 22) |
               nin_bt(imm9, 12) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStRegOff(uint8 opc, uint1 V, uint1 L, int8 imm7, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | thr_bt(0x5, 27) | one_bt(V, 26) | two_bt(2, 23) | one_bt(L, 22) |
               svn_bt(imm7, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeIxReg(uint8 w, uint1 V, int8 opc, int8 imm, armReg Rn, armReg Rt, ixMode ix, assemCtxPo ctx) {
  switch (ix) {
    case postIndex: {
      encodeLdStRegX(w, V, opc, imm, 0, Rn, Rt, ctx);
      return;
    }
    case preIndex: {
      encodeLdStRegPre(w, V, opc, imm, Rn, Rt, ctx);
      return;
    }
    case signedOff: {
      encodeLdStRegOff(w, V, opc, imm, XZR, Rn, Rt, ctx);
      return;
    }
    default: {
      check(False, "invalid index mode");
    }
  }
}

void encodeLdStUnPriv(uint8 sz, uint1 V, uint8 opc, int16 imm9, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) |
               nin_bt(imm9, 12) | one_bt(1, 11) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void
encodeALd3Reg(uint8 sz, uint8 opc, uint1 a, uint1 r, uint1 N, armReg Rs, uint8 opt, armReg Rn, armReg Rt,
              assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | two_bt(opc, 24) | one_bt(a, 23) | one_bt(r, 22) |
               one_bt(1, 21) | fiv_bt(Rs, 16) | thr_bt(opt, 12) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void
encodeLdSt3Reg(uint8 sz, uint1 V, uint8 opc, armReg Rm, uint8 opt, uint1 S, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | thr_bt(0x7, 27) | one_bt(V, 26) | two_bt(opc, 22) | one_bt(1, 21) |
               fiv_bt(Rm, 16) | thr_bt(opt, 13) | one_bt(S, 12) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeSz2OpcImm3Reg(uint8 sz, uint8 op, uint8 opc, uint1 N, armReg Rm, armExtent ex, uint1 S, uint8 S1, armReg Rn,
                         armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(sz, 30) | six_bt(op, 24) | two_bt(opc, 22) | one_bt(N, 21) |
               fiv_bt(Rm, 16) | thr_bt(ex, 13) | one_bt(S, 12) | two_bt(S1, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void
encode3Reg7Imm(uint1 w, uint8 opc, uint1 S, uint8 op, uint8 sh, uint1 L, armReg Rm, uint8 imm, armReg Rn, armReg Rd,
               assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(opc, 30) | one_bt(S, 29) | fiv_bt(op, 24) | two_bt(sh, 22) |
               one_bt(L, 21) | fiv_bt(Rm, 16) | six_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeImm3Reg(uint8 sz, uint8 opc, uint8 op, armReg Rm, uint8 imm, armReg Rn, armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(sz, 31) | two_bt(opc, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) | six_bt(imm, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeDPReg2Imm(uint1 wide, uint8 opc, uint8 op, uint1 N, uint8 imm1, uint8 imm2, armReg Rn, armReg Rd,
                     assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | two_bt(opc, 29)
               | six_bt(op, 23) | one_bt(N, 22) |
               six_bt(imm1, 16) | six_bt(imm2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encodeShift3Reg(uint1 wide, uint1 o, uint1 S, uint8 op, armShift sh, uint1 N, armReg Rm, uint8 imm, armReg Rn,
                     armReg Rd, assemCtxPo ctx) {
  uint32 ins = one_bt(wide, 31) | one_bt(o, 30)
               | one_bt(S, 29) | fiv_bt(op, 24) | two_bt(sh, 22) | one_bt(N, 21) |
               fiv_bt(Rm, 16) | six_bt(imm, 10) | fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void encode2Imm1Src(uint1 w, uint8 op, uint8 immr, uint8 imms, armReg R1, armReg RD, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | ayt_bt(op, 23)
               | one_bt(w, 22) | six_bt(immr, 16) | six_bt(imms, 10) |
               fiv_bt(R1, 5) | fiv_bt(RD, 0);
  emitU32(ctx, ins);
}

void encodeCnd3Reg(uint1 w, uint1 o, uint1 S, uint8 op, armReg Rm, armCond cond, uint1 o2, armReg Rn, armReg Rd,
                   assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(o, 30) | one_bt(S, 29) | ayt_bt(op, 21) |
               fiv_bt(Rm, 16) | for_bt(cond, 12) | one_bt(o2, 10) |
               fiv_bt(Rn, 5) | fiv_bt(Rd, 0);
  emitU32(ctx, ins);
}

void
encodeLdStPrPostIx(uint1 w, uint1 opc, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = one_bt(w, 31) | one_bt(opc, 30) | thr_bt(5, 27) | one_bt(V, 26) | thr_bt(1, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStPrPreIx(uint8 opc, uint1 V, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | thr_bt(5, 27) | one_bt(V, 26) | thr_bt(3, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encodeLdStPrOffset(uint8 opc, uint8 op, uint1 L, int8 imm, armReg Rt2, armReg Rn, armReg Rt, assemCtxPo ctx) {
  uint32 ins = two_bt(opc, 30) | thr_bt(5, 27) | thr_bt(op, 23) | one_bt(L, 22) |
               svn_bt(imm, 15) | fiv_bt(Rt2, 10) | fiv_bt(Rn, 5) | fiv_bt(Rt, 0);
  emitU32(ctx, ins);
}

void encode2SrcIxImmPrePost(uint8 sz, uint8 op, uint8 opc, ixMode ix, uint16 imm, armReg Rn, armReg Rt,
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
