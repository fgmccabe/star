/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include "x86_64P.h"
#include "jitP.h"

static uint8 encodeMod(uint8 mode, x64Reg fst, x64Reg snd);
static uint8 encodeModRR(x64Reg op1, x64Reg op2);
static uint8 encodeModRM(x64Reg fst, x64Reg snd, int64 disp, logical flag);
static void emitDisp(assemCtxPo ctx, int64 disp);
static uint8 encodeSIB(x64Reg base, x64Reg index, uint8 scale);
static uint8 encodeModRI(uint8 mode, x64Reg dst);
static uint8 encodeRex(uint8 rex, x64Reg dst, x64Reg index, x64Reg src);
static void binop_(uint8 op_rm_r, uint8 op_r_rm, uint8 op_rm_imm, uint8 rm_flag, x64Op dst, x64Op src, assemCtxPo ctx);


static __thread logical force_next_disp = False;
static __thread logical emit_sib_24 = False;

#define emitSIB(ctx, base, index, scale) do { \
  emit_sib_24 = False; \
  emitU8(ctx, encodeSIB(base, index, scale)); \
} while(0)

static inline uint8 rexBase(OpSize size) {
  return size == sz64 ? REX_W : REX;
}

static inline void emitRex(assemCtxPo ctx, uint8 rex) {
  if (rex != REX) {
    emitU8(ctx, rex);
  }
}

// Encode binary operators where one side must be a register or source may be immediate

void binop_(uint8 op_rm_r, uint8 op_r_rm, uint8 op_rm_imm, uint8 rm_flag, x64Op dst, x64Op src, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.based.base));
          emitU8(ctx, op_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
          emitDisp(ctx, dst.op.based.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          if (isI32(src.op.imm)) {
            emitRex(ctx, encodeRex(rexBase(size), 0, 4, dst.op.based.base));
            emitU8(ctx, op_rm_imm);
            emitU8(ctx, encodeModRM(rm_flag, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
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
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.reg));
          emitU8(ctx, op_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Immediate:
          emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.reg));
          if (isI32(src.op.imm)) {
            emitU8(ctx, op_rm_imm);
            emitU8(ctx, encodeModRR(dst.op.reg, rm_flag));
            emitU32(ctx, src.op.imm);
          } else {
            check(False, "immediate too large");
          }
          return;
        case Based:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, op_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, op_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          codeLblPo tgt = src.op.lbl;
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, 0x0));
          emitU8(ctx, op_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x5, 0, False));
          emitLblRef(ctx, tgt);
          return;
        }
      }
    case Immediate:
      check(False, "cannot target immediate operand");
      return;
    case Indexed:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, op_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
          emitDisp(ctx, dst.op.indexed.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          emitRex(ctx, encodeRex(rexBase(size), 0, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, op_rm_imm);
          emitU8(ctx, encodeModRM(rm_flag, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
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


// Encode unary operators

void unop_(uint8 op_rm, uint8 rm_flag, x64Op dst, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Based:
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.based.base));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRM(rm_flag, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Reg:
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.reg));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRR(dst.op.reg, rm_flag));
      return;
    case Indexed:
      emitRex(ctx, encodeRex(rexBase(size), 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRM(rm_flag, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    case Labeled: {
      codeLblPo tgt = dst.op.lbl;
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, 0x0));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRM(rm_flag, 5, 0, False));
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, "Not permitted");
      return;
  }
}

void adc_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(ADC_rm_r, ADC_r_rm, ADC_rm_imm, 2, dst, src, ctx);
}

void add_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(ADD_rm_r, ADD_r_rm, ADD_rm_imm, 0, dst, src, ctx);
}

void and_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(AND_rm_r, AND_r_rm, AND_rm_imm, 4, dst, src, ctx);
}

void call_(x64Op src, assemCtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, CALL_rm);
      emitU8(ctx, encodeModRR(src.op.reg, 0x2));
      return;
    case Labeled: {
      codeLblPo tgt = src.op.lbl;
      emitU8(ctx, CALL_lbl);
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void cmp_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(CMP_rm_r, CMP_r_rm, CMP_rm_imm, 7, dst, src, ctx);
}

void dec_(x64Op dst, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Reg:
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.reg));
      emitU8(ctx, DEC_rm);
      emitU8(ctx, encodeModRR(dst.op.reg, 0x1));
      return;
    case Based:
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.based.base));
      emitU8(ctx, DEC_rm);
      emitU8(ctx, encodeModRM(1, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      emitRex(ctx, encodeRex(rexBase(size), 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, DEC_rm);
      emitU8(ctx, encodeModRM(1, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void idiv_(x64Op src, assemCtxPo ctx) {
  unop_(IDIV, 7, src, ctx);
}

void cqo_(assemCtxPo ctx) {
  emitU8(ctx, REX_W);
  emitU8(ctx, CDQ);
}

void cdq_(assemCtxPo ctx) {
  emitU8(ctx, CDQ);
}

void ret_(int16 disp, assemCtxPo ctx) {
  if (disp == 0)
    emitU8(ctx, RET_near);
  else {
    emitU8(ctx, RET_i16);
    emitU16(ctx, (unsigned) disp);
  }
}

void imul_(x64Op dst, x64Op src, assemCtxPo ctx) {
  check(dst.mode == Reg, "destination must be register");
  x64Reg dstReg = dst.op.reg;
  OpSize size = dst.size;
  switch (src.mode) {
    case Reg: {
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, src.op.reg));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRR(src.op.reg, dstReg));
      return;
    }
    case Based:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, src.op.based.base));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRM(dstReg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRM(dstReg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Immediate: {
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, dstReg));
      emitU8(ctx, IMUL_r_rm_i32);
      emitU8(ctx, encodeMod(0xc0, dstReg, dstReg));
      emitU32(ctx, src.op.imm);
      return;
    }
    case Labeled: {
      codeLblPo tgt = src.op.lbl;
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, 0x0));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRM(dstReg, 0x5, 0, False));
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void inc_(x64Op dst, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Reg:
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.reg));
      emitU8(ctx, INC_rm);
      emitU8(ctx, encodeModRR(dst.op.reg, 0x0));
      return;
    case Based:
      emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.based.base));
      emitU8(ctx, INC_rm);
      emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      emitRex(ctx, encodeRex(rexBase(size), 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, INC_rm);
      emitU8(ctx, encodeModRM(0, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void j_cc_(codeLblPo dst, uint8 cc, assemCtxPo ctx) {
  emitU8(ctx, JCC);
  emitU8(ctx, 0x80u | cc);
  emitLblRef(ctx, dst);
}

void jmp_(x64Op src, assemCtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, JMP_rm);
      emitU8(ctx, encodeModRR(src.op.reg, 0x4));
      return;
    case Labeled: {
      codeLblPo tgt = src.op.lbl;
      emitU8(ctx, JMP_m);
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void lea_(x64Op dst, x64Op src, assemCtxPo ctx) {
  check(dst.mode == Reg, "destination must be register");
  x64Reg dstReg = dst.op.reg;
  OpSize size = dst.size;
  switch (src.mode) {
    case Based:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, src.op.based.base));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dstReg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dstReg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled: {
      codeLblPo tgt = src.op.lbl;
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, 0x0));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dstReg, 0x5, 0, False));
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void mov_(x64Op dst, x64Op src, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.based.base));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
          emitDisp(ctx, dst.op.based.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          if (isI32(src.op.imm)) {
            emitRex(ctx, encodeRex(rexBase(size), 0, 4, dst.op.based.base));
            emitU8(ctx, MOV_rm_imm);
            if (dst.op.based.base != RAX) {
              emitU8(ctx, encodeModRM(0, 0x4, dst.op.based.disp, isByte(dst.op.based.disp))); // 4 = no index
              emitSIB(ctx, dst.op.based.base, 4, 1);
            } else {
              emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
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
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.reg));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Immediate:
          emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.reg));
          if (size == sz32) {
            emitU8(ctx, (MOV_r_i32 & 0xf8u) | (((unsigned) (dst.op.reg)) & 0x7u));
            emitU32(ctx, (uint32)src.op.imm);
          } else {
            if (isI32(src.op.imm)) {
              emitU8(ctx, MOV_rm_imm);
              emitU8(ctx, encodeModRI(0xc0u, dst.op.reg));
              emitU32(ctx, src.op.imm);
            } else {
              emitU8(ctx, (MOV_r_i32 & 0xf8u) | (((unsigned) (dst.op.reg)) & 0x7u));
              emitU64(ctx, src.op.imm);
            }
          }
          return;
        case Based:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          codeLblPo tgt = src.op.lbl;
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, 0x0));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x5, 0, False));
          emitLblRef(ctx, tgt);
          return;
        }
      }
    case Immediate:
      check(False, "cannot target immediate operand");
      return;
    case Indexed:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
          emitDisp(ctx, dst.op.indexed.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          emitRex(ctx, encodeRex(rexBase(size), 0, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, MOV_rm_imm);
          emitU8(ctx, encodeModRM(0, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
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

static void genMovsxOp(uint8 scale, assemCtxPo ctx) {
  switch (scale) {
    case 1:
      emitU8(ctx, 0xf);
      emitU8(ctx, MOVBSX_r_rm);
      break;
    case 2:
      emitU8(ctx, 0xf);
      emitU8(ctx, MOVSSX_r_rm);
      break;
    case 4:
      emitU8(ctx, MOVLSX_r_rm);
      break;
    default:
      check(False, " invalid scale");
      return;
  }
}

void movsx_(x64Op dst, x64Op src, uint8 scale, assemCtxPo ctx) {
  check(dst.mode == Reg, "destination must be register");
  x64Reg dstReg = dst.op.reg;
  OpSize size = dst.size;
  switch (src.mode) {
    case Reg:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, src.op.reg));
      genMovsxOp(scale, ctx);
      emitU8(ctx, encodeModRR(src.op.reg, dstReg));
      return;
    case Based:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, src.op.based.base));
      genMovsxOp(scale, ctx);
      emitU8(ctx, encodeModRM(dstReg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      emitRex(ctx, encodeRex(rexBase(size), dstReg, src.op.indexed.index, src.op.indexed.base));
      genMovsxOp(scale, ctx);
      emitU8(ctx, encodeModRM(dstReg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled: {
      codeLblPo tgt = src.op.lbl;
      emitRex(ctx, encodeRex(rexBase(size), dstReg, 0, 0x0));
      genMovsxOp(scale, ctx);
      emitU8(ctx, encodeModRM(dstReg, 0x5, 0, False));
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void or_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(OR_rm_r, OR_r_rm, OR_rm_imm, 1, dst, src, ctx);
}

void neg_(x64Op dst, assemCtxPo ctx) {
  unop_(NEG_rm, 3, dst, ctx);
}

void not_(x64Op dst, assemCtxPo ctx) {
  unop_(NOT_rm, 2, dst, ctx);
}

void pop_(x64Op dst, assemCtxPo ctx) {
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
      emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      if (dst.op.indexed.base >= R8 || dst.op.indexed.index >= R8)
        emitU8(ctx, encodeRex(REX, 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, POP_rm);
      emitU8(ctx, encodeModRM(0x0, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void push_(x64Op src, assemCtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, PUSH_r + (src.op.reg & 0x7u));
      return;
    case Immediate: {
      if (isByte(src.op.imm)) {
        emitU8(ctx, PUSH_i8);
        emitU8(ctx, (uint8) src.op.imm);
      } else if (isI32(src.op.imm)) {
        emitU8(ctx, PUSH_i32);
        emitU32(ctx, (uint32) src.op.imm);
      } else
        check(False, "immediate too large");
      return;
    }
    case Based:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.based.base));

      emitU8(ctx, PUSH_rm);
      emitU8(ctx, encodeModRM(6, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      if (src.op.indexed.base >= R8 || src.op.indexed.index >= R8)
        emitU8(ctx, encodeRex(REX, 0, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, PUSH_rm);
      emitU8(ctx, encodeModRM(0x6, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled:
      check(False, "Not permitted");
      return;
  }
}

void setcc_(x64Reg dst, uint8 cc, assemCtxPo ctx) {
  emitU8(ctx, SET_CC_rm);
  emitU8(ctx, 0x90u | cc);
  emitU8(ctx, encodeModRR(dst, 0));
}

void sbb_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(SBB_rm_r, SBB_r_rm, SBB_rm_imm, 3, dst, src, ctx);
}

void sub_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(SUB_rm_r, SUB_r_rm, SUB_rm_imm, 5, dst, src, ctx);
}

void test_(x64Op dst, x64Op src, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.based.base));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
          emitDisp(ctx, dst.op.based.disp);
          return;
        case Based:
        case Indexed:
        case Labeled:
          check(False, "not permitted");
          return;
        case Immediate:
          if (isI32(src.op.imm)) {
            emitRex(ctx, encodeRex(rexBase(size), 0, 4, dst.op.based.base));
            emitU8(ctx, TEST_rm_imm);
            if (dst.op.based.base != RAX) {
              emitU8(ctx, encodeModRM(0, 0x4, dst.op.based.disp, isByte(dst.op.based.disp))); // 4 = no index
              emitSIB(ctx, dst.op.based.base, 4, 1);
            } else {
              emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
            }
            emitDisp(ctx, dst.op.based.disp);
            emitU32(ctx, src.op.imm);
          } else {
            check(False, "immediate too large");
          }
          return;
      }
    case Reg:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.reg));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Immediate:
          emitRex(ctx, encodeRex(rexBase(size), 0, 0, dst.op.reg));
          if (isI32(src.op.imm)) {
            if (dst.op.reg == RAX) {
              emitU8(ctx, TEST_RAX_imm);
            } else {
              emitU8(ctx, TEST_rm_imm);
              emitU8(ctx, encodeModRI(0xc0, dst.op.reg));
            }
            emitU32(ctx, src.op.imm);
          } else {
            check(False, "immediate too large");
          }
          return;
        case Based:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          codeLblPo tgt = src.op.lbl;
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, 0x0));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeMod(0x0, 0x4, dst.op.reg));
          emitU8(ctx, 0x25); // Special SIB
          emitLblRef(ctx, tgt);
          return;
        }
      }
    case Immediate:
    case Labeled:
    case Indexed:
      test_(src, dst, ctx);
      return;
  }
}

void xchg_(x64Op dst, x64Op src, assemCtxPo ctx) {
  OpSize size = dst.size;
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.based.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(src.op.reg, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
          emitDisp(ctx, dst.op.based.disp);
          return;
        case Based:
        case Indexed:
        case Immediate:
        case Labeled:
          check(False, "not permitted");
          return;
      }
    case Reg:
      switch (src.mode) {
        case Reg:
          if (src.op.reg == RAX) {
            emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.reg));
            emitU8(ctx, (unsigned) XCHG_EAX_r | (uint8) (dst.op.reg & 0x7u));
          } else if (dst.op.reg == RAX) {
            emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, src.op.reg));
            emitU8(ctx, (unsigned) XCHG_EAX_r | (uint8) (src.op.reg & 0x7u));
          } else {
            emitRex(ctx, encodeRex(rexBase(size), src.op.reg, 0, dst.op.reg));
            emitU8(ctx, XCHG_r_rm);
            emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          }
          return;
        case Immediate:
        case Labeled:
          check(False, "not permitted");
          return;
        case Based:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitRex(ctx, encodeRex(rexBase(size), dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitSIB(ctx, src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale);
          emitDisp(ctx, src.op.indexed.disp);
          return;
      }
    case Immediate:
      check(False, "cannot target immediate operand");
      return;
    case Indexed:
      switch (src.mode) {
        case Reg:
          emitRex(ctx, encodeRex(rexBase(size), src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitSIB(ctx, dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale);
          emitDisp(ctx, dst.op.indexed.disp);
          return;
        case Based:
        case Indexed:
        case Immediate:
        case Labeled:
          check(False, "not permitted");
          return;
      }
    case Labeled:
      check(False, "Not permitted");
      return;
  }
}

void xor_(x64Op dst, x64Op src, assemCtxPo ctx) {
  binop_(XOR_rm_r, XOR_r_rm, XOR_rm_imm, 6, dst, src, ctx);
}

uint8 encodeMod(uint8 mode, x64Reg fst, x64Reg snd) {
  uint8 dr = ((uint8) fst) & 0x7u;
  uint8 sr = ((uint8) snd) & 0x7u;

  return (mode | (unsigned) (sr << 3u) | dr);
}

uint8 encodeModRR(x64Reg op1, x64Reg op2) {
  uint8 dr = ((uint8) op1) & 0x7u;
  uint8 sr = ((uint8) op2) & 0x7u;

  return ((uint8) 0xc0) | (unsigned) (sr << 3u) | dr;
}


uint8 encodeModRM(x64Reg fst, x64Reg snd, int64 disp, logical flag) {
  uint8 fr = ((uint8) fst) & 0x7u;
  uint8 sr = ((uint8) snd) & 0x7u;

  if (sr == 4)
    emit_sib_24 = True;
  else
    emit_sib_24 = False;

  if (disp == 0 && sr == 5 && flag) {
    force_next_disp = True;
    return ((uint8) 0x40u) | (unsigned) (fr << 3u) | sr;
  } else if (disp == 0) {
    force_next_disp = False;
    return (unsigned) (fr << 3u) | sr;
  } else if (flag) {
    force_next_disp = False;
    return ((uint8) 0x40u) | (unsigned) (fr << 3u) | sr;
  } else {
    force_next_disp = False;
    return ((uint8) 0x80u) | (unsigned) (fr << 3u) | sr;
  }
}

void emitDisp(assemCtxPo ctx, int64 disp) {
  if (emit_sib_24) {
    emitU8(ctx, 0x24);
    emit_sib_24 = False;
  }
  if (disp != 0 || force_next_disp) {
    if (isByte(disp) || force_next_disp)
      emitU8(ctx, (unsigned) disp & 0xffu);
    else
      emitU32(ctx, disp);
  }
  force_next_disp = False;
}

uint8 encodeModRI(uint8 mode, x64Reg dst) {
  uint8 dr = ((uint8) dst) & 0x7u;

  return mode | dr;
}

uint8 encodeSIB(x64Reg base, x64Reg index, uint8 scale) {
  uint8 sib = ((index & 0x7u) << 3u) | (base & 0x7u);

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

uint8 encodeRex(uint8 rex, x64Reg dst, x64Reg index, x64Reg src) {
  uint8 dr = ((unsigned) (((uint8) dst) >> 3u) & 1u) << 2u;
  uint8 sr = ((unsigned) (((uint8) src) >> 3u) & 1u);
  uint8 sx = ((unsigned) (((uint8) index) >> 3u) & 1u) << 1u;

  return (unsigned) rex | dr | sx | sr;
}

static void shift_(uint8 regField, x64Op dst, x64Op src, assemCtxPo ctx) {
  OpSize size = dst.size;
  check(dst.mode == Reg, "dst must be register");

  if (src.mode == Immediate) {
    int8 imm = (int8) src.op.imm;
    if (size == sz64) {
      emitRex(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
    } else if (dst.op.reg >= R8) {
      emitRex(ctx, encodeRex(REX, 0, 0, dst.op.reg));
    }
    if (imm == 1) {
      emitU8(ctx, 0xd1); // shift by 1
      emitU8(ctx, encodeModRR(dst.op.reg, regField));
    } else {
      emitU8(ctx, 0xc1); // shift by imm8
      emitU8(ctx, encodeModRR(dst.op.reg, regField));
      emitU8(ctx, (uint8)imm);
    }
  } else if (src.mode == Reg && src.op.reg == RCX) {
    if (size == sz64) {
      emitRex(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
    } else if (dst.op.reg >= R8) {
      emitRex(ctx, encodeRex(REX, 0, 0, dst.op.reg));
    }
    emitU8(ctx, 0xd3); // shift by CL
    emitU8(ctx, encodeModRR(dst.op.reg, regField));
  } else {
    check(False, "invalid shift operand");
  }
}

void sar_(x64Op dst, x64Op src, assemCtxPo ctx) {
  shift_(7, dst, src, ctx);
}

void shr_(x64Op dst, x64Op src, assemCtxPo ctx) {
  shift_(5, dst, src, ctx);
}

void sal_(x64Op dst, x64Op src, assemCtxPo ctx) {
  shift_(4, dst, src, ctx);
}

void shl_(x64Op dst, x64Op src, assemCtxPo ctx) {
  shift_(4, dst, src, ctx);
}

static void sse_op_(uint8 prefix, uint8 opcode, uint8 rex_base, FlexOp dst, FlexOp src, assemCtxPo ctx) {
  if (prefix != 0) {
    emitU8(ctx, prefix);
  }

  uint8 reg_dst = 0;
  uint8 reg_src = 0;

  if (dst.mode == Reg) reg_dst = dst.op.reg;
  else if (dst.mode == Fp) reg_dst = dst.op.fpReg;

  if (src.mode == Reg) reg_src = src.op.reg;
  else if (src.mode == Fp) reg_src = src.op.fpReg;
  else if (src.mode == Based) reg_src = src.op.based.base;

  uint8 rex = encodeRex(rex_base, reg_dst, 0, reg_src);
  emitRex(ctx, rex);

  emitU8(ctx, 0x0F);
  emitU8(ctx, opcode);

  if (src.mode == Reg || src.mode == Fp) {
    emitU8(ctx, encodeModRR(reg_src, reg_dst));
  } else if (src.mode == Based) {
    emitU8(ctx, encodeModRM(reg_dst, reg_src, src.op.based.disp, isByte(src.op.based.disp)));
    emitDisp(ctx, src.op.based.disp);
  } else {
    check(False, "invalid src mode for SSE op");
  }
}

void addsd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0xF2, ADDSD_x_xm, REX, dst, src, ctx);
}

void subsd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0xF2, SUBSD_x_xm, REX, dst, src, ctx);
}

void mulsd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0xF2, MULSD_x_xm, REX, dst, src, ctx);
}

void divsd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0xF2, DIVSD_x_xm, REX, dst, src, ctx);
}

void ucomisd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0x66, UCOMISD_x_xm, REX, dst, src, ctx);
}

void cvtsi2sd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  uint8 rex_base = (src.size == sz64) ? REX_W : REX;
  sse_op_(0xF2, CVTSI2SD_x_rm, rex_base, dst, src, ctx);
}

void cvttsd2si_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  uint8 rex_base = (dst.size == sz64) ? REX_W : REX;
  sse_op_(0xF2, CVTTSD2SI_r_xm, rex_base, dst, src, ctx);
}

void movsd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  if (dst.mode == Fp) {
    sse_op_(0xF2, MOVSD_x_xm, REX, dst, src, ctx);
  } else if (dst.mode == Based && src.mode == Fp) {
    uint8 prefix = 0xF2;
    uint8 opcode = MOVSD_xm_x;
    emitU8(ctx, prefix);

    uint8 reg_src = src.op.fpReg;
    uint8 reg_dst_base = dst.op.based.base;

    uint8 rex = encodeRex(REX, reg_src, 0, reg_dst_base);
    emitRex(ctx, rex);

    emitU8(ctx, 0x0F);
    emitU8(ctx, opcode);

    emitU8(ctx, encodeModRM(reg_src, reg_dst_base, dst.op.based.disp, isByte(dst.op.based.disp)));
    emitDisp(ctx, dst.op.based.disp);
  } else {
    check(False, "invalid operands for movsd");
  }
}

void movq_g2x_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  assert(dst.mode == Fp);
  sse_op_(0x66, MOVQ_x_rm, REX_W, dst, src, ctx);
}

void movq_x2g_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  assert(src.mode == Fp);
  emitU8(ctx, 0x66);

  uint8 reg_src = src.op.fpReg;
  uint8 reg_dst = 0;

  if (dst.mode == Reg) reg_dst = dst.op.reg;
  else if (dst.mode == Based) reg_dst = dst.op.based.base;

  uint8 rex = encodeRex(REX_W, reg_src, 0, reg_dst);
  emitRex(ctx, rex);

  emitU8(ctx, 0x0F);
  emitU8(ctx, MOVQ_rm_x);

  if (dst.mode == Reg) {
    emitU8(ctx, encodeModRR(reg_dst, reg_src));
  } else if (dst.mode == Based) {
    emitU8(ctx, encodeModRM(reg_src, reg_dst, dst.op.based.disp, isByte(dst.op.based.disp)));
    emitDisp(ctx, dst.op.based.disp);
  } else {
    check(False, "invalid dst for movq_x2g");
  }
}

void andpd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0x66, ANDPD_x_xm, REX, dst, src, ctx);
}

void xorpd_(FlexOp dst, FlexOp src, assemCtxPo ctx) {
  sse_op_(0x66, XORPD_x_xm, REX, dst, src, ctx);
}


