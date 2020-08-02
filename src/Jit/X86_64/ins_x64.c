/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include <utils.h>
#include <stdlib.h>
#include "x86_64P.h"

static u8 encodeMod(u8 mode, x64Reg fst, x64Reg snd);
static u8 encodeModRR(x64Reg dst, x64Reg src);
static u8 encodeModRM(x64Reg fst, x64Reg snd, i64 disp, logical flag);
static void emitDisp(x64CtxPo ctx, i64 disp);
static u8 encodeSIB(x64Reg base, x64Reg index, u8 scale);
static u8 encodeModRI(u8 mode, x64Reg dst);
static u8 encodeRex(u8 rex, x64Reg dst, x64Reg index, x64Reg src);
static void binop_(u8 op_rm_r, u8 op_r_rm, u8 op_rm_imm, u8 rm_flag, x64Op dst, x64Op src, x64CtxPo ctx);


// Encode binary operators where one side must be a register or source may be immediate

void binop_(u8 op_rm_r, u8 op_r_rm, u8 op_rm_imm, u8 rm_flag, x64Op dst, x64Op src, x64CtxPo ctx) {
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.based.base));
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
            emitU8(ctx, encodeRex(REX_W, 0, 4, dst.op.based.base));
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
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.reg));
          emitU8(ctx, op_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Immediate:
          emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
          if (isI32(src.op.imm)) {
            emitU8(ctx, op_rm_imm);
            emitU8(ctx, encodeModRR(dst.op.reg, rm_flag));
            emitU32(ctx, src.op.imm);
          } else {
            check(False, "immediate too large");
          }
          return;
        case Based:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, op_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, op_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          x64LblPo tgt = src.op.lbl;
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, 0x0));
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
          emitU8(ctx, encodeRex(REX_W, src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, op_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
          emitDisp(ctx, dst.op.indexed.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          emitU8(ctx, encodeRex(REX_W, 0, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, op_rm_imm);
          emitU8(ctx, encodeModRM(rm_flag, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
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


// Encode unary operators

void unop_(u8 op_rm, u8 rm_flag, x64Op dst, x64CtxPo ctx) {
  switch (dst.mode) {
    case Based:
      emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.based.base));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRM(rm_flag, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Reg:
      emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRR(dst.op.reg, rm_flag));
      return;
    case Indexed:
      emitU8(ctx, encodeRex(REX_W, 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, op_rm);
      emitU8(ctx, encodeModRM(rm_flag, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    case Labeled: {
      x64LblPo tgt = dst.op.lbl;
      emitU8(ctx, encodeRex(REX_W, 0, 0, 0x0));
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

void adc_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(ADC_rm_r, ADC_r_rm, ADC_rm_imm, 2, dst, src, ctx);
}

void add_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(ADD_rm_r, ADD_r_rm, ADD_rm_imm, 0, dst, src, ctx);
}

void and_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(AND_rm_r, AND_r_rm, AND_rm_imm, 4, dst, src, ctx);
}

void call_(x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, CALL_rm);
      emitU8(ctx, encodeModRR(src.op.reg, 0x2));
      return;
    case Labeled: {
      x64LblPo tgt = src.op.lbl;
      emitU8(ctx, CALL_lbl);
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void cmp_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(CMP_rm_r, CMP_r_rm, CMP_rm_imm, 7, dst, src, ctx);
}

void dec_(x64Op dst, x64CtxPo ctx) {
  switch (dst.mode) {
    case Reg:
      emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
      emitU8(ctx, DEC_rm);
      emitU8(ctx, encodeModRR(dst.op.reg, 0x1));
      return;
    case Based:
      emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.based.base));
      emitU8(ctx, DEC_rm);
      emitU8(ctx, encodeModRM(1, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      emitU8(ctx, encodeRex(REX_W, 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, DEC_rm);
      emitU8(ctx, encodeModRM(1, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void idiv_(x64Op src, x64CtxPo ctx) {
  unop_(IDIV, 7, src, ctx);
}

void ret_(i16 disp, x64CtxPo ctx) {
  if (disp == 0)
    emitU8(ctx, RET_near);
  else {
    emitU8(ctx, RET_i16);
    emitU16(ctx, (unsigned) disp);
  }
}

void imul_(x64Reg dst, x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Reg: {
      emitU8(ctx, encodeRex(REX_W, dst, 0, src.op.reg));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRR(src.op.reg, dst));
      return;
    }
    case Based:
      emitU8(ctx, encodeRex(REX_W, dst, 0, src.op.based.base));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRM(dst, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      emitU8(ctx, encodeRex(REX_W, dst, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRM(dst, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Immediate: {
      emitU8(ctx, encodeRex(REX_W, dst, 0, dst));
      emitU8(ctx, IMUL_r_rm_i32);
      emitU8(ctx, encodeMod(0xc0, dst, dst));
      emitU32(ctx, src.op.imm);
      return;
    }
    case Labeled: {
      x64LblPo tgt = src.op.lbl;
      emitU8(ctx, encodeRex(REX_W, dst, 0, 0x0));
      emitU8(ctx, IMUL_r_rm_1);
      emitU8(ctx, IMUL_r_rm_2);
      emitU8(ctx, encodeModRM(dst, 0x5, 0, False));
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void inc_(x64Op dst, x64CtxPo ctx) {
  switch (dst.mode) {
    case Reg:
      emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
      emitU8(ctx, INC_rm);
      emitU8(ctx, encodeModRR(dst.op.reg, 0x0));
      return;
    case Based:
      emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.based.base));
      emitU8(ctx, INC_rm);
      emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      emitU8(ctx, encodeRex(REX_W, 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, INC_rm);
      emitU8(ctx, encodeModRM(0, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void j_cc_(x64LblPo dst, u8 cc, x64CtxPo ctx) {
  emitU8(ctx, JCC);
  emitU8(ctx, 0x80u | cc);
  emitLblRef(ctx, dst);
}

void jmp_(x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, JMP_rm);
      emitU8(ctx, encodeModRR(src.op.reg, 0x4));
      return;
    case Labeled: {
      x64LblPo tgt = src.op.lbl;
      emitU8(ctx, JMP_m);
      emitLblRef(ctx, tgt);
      return;
    }
    default:
      check(False, " source mode not permitted");
      return;
  }
}

void lea_(x64Reg dst, x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Based:
      emitU8(ctx, encodeRex(REX_W, dst, 0, src.op.based.base));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dst, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      emitU8(ctx, encodeRex(REX_W, dst, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dst, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled: {
      x64LblPo tgt = src.op.lbl;
      emitU8(ctx, encodeRex(REX_W, dst, 0, 0x0));
      emitU8(ctx, LEA_r_m);
      emitU8(ctx, encodeModRM(dst, 0x5, 0, False));
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
          emitU8(ctx, encodeModRM(src.op.reg, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
          emitDisp(ctx, dst.op.based.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          if (isI32(src.op.imm)) {
            emitU8(ctx, encodeRex(REX_W, 0, 4, dst.op.based.base));
            emitU8(ctx, MOV_rm_imm);
            if (dst.op.based.base != RAX) {
              emitU8(ctx, encodeModRM(0, 0x4, dst.op.based.disp, isByte(dst.op.based.disp))); // 4 = no index
              emitU8(ctx, encodeSIB(dst.op.based.base, 4, 1));
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
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.reg));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Immediate:
          emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
          if (isI32(src.op.imm)) {
            emitU8(ctx, MOV_rm_imm);
            emitU8(ctx, encodeModRI(0xc0u, dst.op.reg));
            emitU32(ctx, src.op.imm);
          } else {
            emitU8(ctx, MOV_r_i32 | (((unsigned) (dst.op.reg)) & 0x7u));
            emitU64(ctx, src.op.imm);
          }
          return;
        case Based:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, MOV_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          x64LblPo tgt = src.op.lbl;
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, 0x0));
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
          emitU8(ctx, encodeRex(REX_W, src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, MOV_rm_r);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
          emitDisp(ctx, dst.op.indexed.disp);
          return;
        case Based:
        case Indexed:
          check(False, "not permitted");
          return;
        case Immediate:
          emitU8(ctx, encodeRex(REX_W, 0, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, MOV_rm_imm);
          emitU8(ctx, encodeModRM(0, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
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

void or_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(OR_rm_r, OR_r_rm, OR_rm_imm, 1, dst, src, ctx);
}

void neg_(x64Op dst, x64CtxPo ctx) {
  unop_(NEG_rm, 3, dst, ctx);
}

void not_(x64Op dst, x64CtxPo ctx) {
  unop_(NOT_rm, 2, dst, ctx);
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
      emitU8(ctx, encodeModRM(0, dst.op.based.base, dst.op.based.disp, isByte(dst.op.based.disp)));
      emitDisp(ctx, dst.op.based.disp);
      return;
    case Indexed:
      if (dst.op.indexed.base >= R8 || dst.op.indexed.index >= R8)
        emitU8(ctx, encodeRex(REX, 0, dst.op.indexed.index, dst.op.indexed.base));
      emitU8(ctx, POP_rm);
      emitU8(ctx, encodeModRM(0x0, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
      emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
      emitDisp(ctx, dst.op.indexed.disp);
      return;
    default:
      check(False, "not permitted");
  }
}

void push_(x64Op src, x64CtxPo ctx) {
  switch (src.mode) {
    case Reg:
      if (src.op.reg >= R8)
        emitU8(ctx, encodeRex(REX, 0, 0, src.op.reg));
      emitU8(ctx, PUSH_r + (src.op.reg & 0x7u));
      return;
    case Immediate: {
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
      emitU8(ctx, encodeModRM(6, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
      emitDisp(ctx, src.op.based.disp);
      return;
    case Indexed:
      if (src.op.indexed.base >= R8 || src.op.indexed.index >= R8)
        emitU8(ctx, encodeRex(REX, 0, src.op.indexed.index, src.op.indexed.base));
      emitU8(ctx, PUSH_rm);
      emitU8(ctx, encodeModRM(0x6, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
      emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
      emitDisp(ctx, src.op.indexed.disp);
      return;
    case Labeled:
      check(False, "Not permitted");
      return;
  }
}

void setcc_(x64Reg dst, u8 cc, x64CtxPo ctx) {
  emitU8(ctx, SET_CC_rm);
  emitU8(ctx, 0x90u | cc);
  emitU8(ctx, encodeModRR(dst, 0));
}

void sbb_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(SBB_rm_r, SBB_r_rm, SBB_rm_imm, 3, dst, src, ctx);
}

void sub_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(SUB_rm_r, SUB_r_rm, SUB_rm_imm, 5, dst, src, ctx);
}

void test_(x64Op dst, x64Op src, x64CtxPo ctx) {
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.based.base));
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
            emitU8(ctx, encodeRex(REX_W, 0, 4, dst.op.based.base));
            emitU8(ctx, TEST_rm_imm);
            if (dst.op.based.base != RAX) {
              emitU8(ctx, encodeModRM(0, 0x4, dst.op.based.disp, isByte(dst.op.based.disp))); // 4 = no index
              emitU8(ctx, encodeSIB(dst.op.based.base, 4, 1));
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
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.reg));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          return;
        case Immediate:
          emitU8(ctx, encodeRex(REX_W, 0, 0, dst.op.reg));
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
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, TEST_rm_r);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
          emitDisp(ctx, src.op.indexed.disp);
          return;
        case Labeled: {
          x64LblPo tgt = src.op.lbl;
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, 0x0));
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

void xchg_(x64Op dst, x64Op src, x64CtxPo ctx) {
  switch (dst.mode) {
    case Based:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.based.base));
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
            emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.reg));
            emitU8(ctx, (unsigned) XCHG_EAX_r | (u8) (dst.op.reg & 0x7u));
          } else if (dst.op.reg == RAX) {
            emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.reg));
            emitU8(ctx, (unsigned) XCHG_EAX_r | (u8) (src.op.reg & 0x7u));
          } else {
            emitU8(ctx, encodeRex(REX_W, src.op.reg, 0, dst.op.reg));
            emitU8(ctx, XCHG_r_rm);
            emitU8(ctx, encodeModRR(dst.op.reg, src.op.reg));
          }
          return;
        case Immediate:
        case Labeled:
          check(False, "not permitted");
          return;
        case Based:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, 0, src.op.based.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, src.op.based.base, src.op.based.disp, isByte(src.op.based.disp)));
          emitDisp(ctx, src.op.based.disp);
          return;
        case Indexed:
          emitU8(ctx, encodeRex(REX_W, dst.op.reg, src.op.indexed.index, src.op.indexed.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(dst.op.reg, 0x4, src.op.indexed.disp, isByte(src.op.indexed.disp)));
          emitU8(ctx, encodeSIB(src.op.indexed.base, src.op.indexed.index, src.op.indexed.scale));
          emitDisp(ctx, src.op.indexed.disp);
          return;
      }
    case Immediate:
      check(False, "cannot target immediate operand");
      return;
    case Indexed:
      switch (src.mode) {
        case Reg:
          emitU8(ctx, encodeRex(REX_W, src.op.reg, dst.op.indexed.index, dst.op.indexed.base));
          emitU8(ctx, XCHG_r_rm);
          emitU8(ctx, encodeModRM(src.op.reg, 0x4, dst.op.indexed.disp, isByte(dst.op.indexed.disp)));
          emitU8(ctx, encodeSIB(dst.op.indexed.base, dst.op.indexed.index, dst.op.indexed.scale));
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

void xor_(x64Op dst, x64Op src, x64CtxPo ctx) {
  binop_(XOR_rm_r, XOR_r_rm, XOR_rm_imm, 6, dst, src, ctx);
}

u8 encodeMod(u8 mode, x64Reg fst, x64Reg snd) {
  u8 dr = ((u8) fst) & 0x7u;
  u8 sr = ((u8) snd) & 0x7u;

  return (mode | (unsigned) (sr << 3u) | dr);
}

u8 encodeModRR(x64Reg dst, x64Reg src) {
  u8 dr = ((u8) dst) & 0x7u;
  u8 sr = ((u8) src) & 0x7u;

  return ((u8) 0xc0) | (unsigned) (sr << 3u) | dr;
}

u8 encodeModRM(x64Reg fst, x64Reg snd, i64 disp, logical flag) {
  u8 fr = ((u8) fst) & 0x7u;
  u8 sr = ((u8) snd) & 0x7u;

  if (disp == 0)
    return (unsigned) (fr << 3u) | sr;
  else if (flag)
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

u8 encodeModRI(u8 mode, x64Reg dst) {
  u8 dr = ((u8) dst) & 0x7u;

  return mode | dr;
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
