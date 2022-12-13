#ifndef ARM64_H_
#define ARM64_H_

#include <config.h>
#include "utils.h"

typedef struct assem_ctx *assemCtxPo;
typedef struct assem_lbl *codeLblPo;

typedef enum {
  X0 = 0,
  X1 = 1,
  X2 = 2,
  X3 = 3,
  X4 = 4,
  X5 = 5,
  X6 = 6,
  X7 = 7,
  X8 = 8,
  X9 = 9,
  X10 = 10,
  X11 = 11,
  X12 = 12,
  X13 = 13,
  X14 = 14,
  X15 = 15,
  X16 = 16,
  X17 = 17,
  X18 = 18,
  X19 = 19,
  X20 = 20,
  X21 = 21,
  X22 = 22,
  X23 = 23,
  X24 = 24,
  X25 = 25,
  X26 = 26,
  X27 = 27,
  X28 = 28,
  X29 = 29,
  LR = 30,
  XZR = 31,
  SP = 31
} armReg;

typedef enum {
  EQ = 0,    // Equal
  NE = 0x1,  // Note equal
  CS = 0x2,  // Carry set or Higher condition
  CC = 0x3,  // Carry clear or less than
  MI = 0x4,  // Negative, or less then
  PL = 0x5,  // Plus, positive or zero
  VS = 0x6,  // Overflow
  VC = 0x7,  // No overflow
  HI = 0x8,  // Unsigned higher
  LS = 0x9,  // Unsigned lower or same
  GE = 0xa,  // Signed greater than or equal
  LT = 0xb,  // Signed less than
  GT = 0xc,  // Signed greater than
  LE = 0xd,  // Signed less than or equal
  AL = 0xe,  // Always
  NV = 0xf   // Always
} armCond;

typedef enum {
  Reg,
  Immediate,
  Based,
  PreIndexed,
  PostIndexed,
  Relative
} armOpMode;

typedef struct {
  armOpMode mode;
  union {
    armReg reg;
    struct {
      armReg base;
      int disp;
    } based;
    struct {
      armReg base;
      armReg index;
      int8 scale;
    } indexed;
    int64 imm;
    int16 scale;
    uint8 fpReg;
    codeLblPo lbl;
  } op;
} armOp;

typedef enum {
  imm,  // Immediate value
  lsli, // Logical shift left immediate
  lsri, // logical shift right immediate
  asri, // arithmetic shift right immediate
  rori, // rotate right immediate
  reg, // register
  rox, // rotate right extended
  lslr, // logical shift left register
  lsrr, // logical shift right register
  asrr, // arithmetic shift right register
  rorr // rotate right register
} FlexibleMode;

typedef struct {
  FlexibleMode mode;
  uint32 immediate;
  uint1 hiLo;
  armReg reg;
  uint8 shift;
} FlexOp;

typedef armOp registerSpec;
#define PLATFORM_PC_DELTA (0)

typedef enum {
  LSL = 0,
  LSR = 1,
  ASR = 2,
  ROR = 3
} armShift;

typedef enum {
  U_XTB = 0,
  U_XTH = 1,
  U_XTW = 2,
  U_XTX = 3,
  S_XTB = 4,
  S_XTH = 5,
  S_XTW = 6,
  S_XTX = 7
} armExtent;

typedef enum {
  postIndex = 1,
  preIndex = 3,
  signedOff = 2,
  unsignedOff = 0
} ixMode;

void adc_(uint1 wide, armReg rd, armReg s1, armReg s2, assemCtxPo ctx);
void adcs_(uint1 wide, armReg rd, armReg s1, armReg s2, assemCtxPo ctx);
void add_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
void add_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx);
void adds_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx);
void adds_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);

void adr_(armReg Rd, codeLblPo lbl, assemCtxPo ctx);
void adrp_(armReg Rd, codeLblPo lbl, assemCtxPo ctx);

void and_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
void ands_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
void asr_reg_(uint1 w, armReg RD, armReg S1, armReg S2, assemCtxPo ctx);
void asr_imm(uint1 w, armReg Rd, armReg Rn, uint8 sh, assemCtxPo ctx);
void asrv_(uint1 w, armReg RD, armReg S1, armReg S2, assemCtxPo ctx);

void b_cond_(armCond cond, codeLblPo lbl, assemCtxPo ctx);
void b_(codeLblPo lbl, assemCtxPo ctx);
void bfc_(uint1 w, armReg RD, uint8 width, uint8 bit, assemCtxPo ctx);
void bfi_(uint1 w, armReg RD, armReg S, uint8 width, uint8 bit, assemCtxPo ctx);
void bfxil_(uint1 w, armReg RD, armReg S, uint8 width, uint8 bit, assemCtxPo ctx);
void bics_(uint1 w, armShift tp, armReg RD, armReg Rn, armReg Rm, uint8 shift, assemCtxPo ctx);

void bl_(codeLblPo lbl, assemCtxPo ctx);
void blr_(armReg reg, assemCtxPo ctx);
void br_(armReg reg, assemCtxPo ctx);
void brk_(uint16 bkpt, assemCtxPo ctx);

void sub_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);

void orn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void orr_imm(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void orr_sh_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);

void eor_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void mvn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void bic_(uint1 w, armShift tp, armReg RD, armReg Rn, armReg Rm, uint8 shift, assemCtxPo ctx);
void mov_(armReg d, armOp s1, assemCtxPo ctx);
void mul_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
void sdiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
void udiv_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);

void lsl_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
void lsr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);

void cmp_(armReg s1, armOp s2, assemCtxPo ctx);
void tst_(armReg s1, armOp s2, assemCtxPo ctx);

void ret_(armReg reg, assemCtxPo ctx);

void cbnz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
void cbz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
void tbnz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx);
void tbz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx);

void clrex_(assemCtxPo ctx);
void dmb_(assemCtxPo ctx);
void dsb_(assemCtxPo ctx);
void isb_(assemCtxPo ctx);

void csdb_(assemCtxPo ctx);
void esb_(assemCtxPo ctx);

void extr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, uint8 lsb, assemCtxPo ctx);

void ldr_(uint1 w, armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx);
void ldrb_imm(armReg Rt, armReg Rn, uint16 imm, ixMode ix, assemCtxPo ctx);
void ldrsb_(armReg dst, armOp src, assemCtxPo ctx);
void ldrh_(armReg dst, armOp src, assemCtxPo ctx);
void ldrsh_(armReg dst, armOp src, assemCtxPo ctx);
void ldrw_(armReg dst, armOp src, assemCtxPo ctx);
void ldrsw_(armReg dst, armOp src, assemCtxPo ctx);

void ldur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void ldurb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void ldursb_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void ldurh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void ldursh_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void ldurw_(armReg dst, armOp src, assemCtxPo ctx);
void ldursw_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);

void bfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx);
void sbfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx);
void ubfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx);

void str_(uint1 w, armReg Rt, armReg Rn, ixMode ix, int16 imm, assemCtxPo ctx);
void strb_(armReg Rt, armReg Rn, int16 imm, ixMode ix, assemCtxPo ctx);
void strh_(armReg Rt, armReg Rn, int16 imm, ixMode ix, assemCtxPo ctx);
void strb_r_(armReg Rt, armReg Rn, armReg Rm, armExtent ex, uint1 shft, assemCtxPo ctx);

void stur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void sturb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
void sturh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);

void ldnp(uint1 w, armReg Rt, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx);
void ldp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, int8 imm, ixMode ix, assemCtxPo ctx);
void ldpsw_(armReg Rt, armReg Rt2, armReg Rn, int8 imm, ixMode ix, assemCtxPo ctx);
void stp_(uint1 w, armReg Rt, armReg Rt2, armReg Rn, int8 imm, ixMode ix, assemCtxPo ctx);

void ldnp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx);
void stnp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx);

void ldxr_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx);
void ldxrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldxrh_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldxap_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx);
void ldxp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, assemCtxPo ctx);

void stxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx);
void stxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx);

void ldapr_(armReg dst, armOp src, assemCtxPo ctx);
void ldaprb_(armReg dst, armOp src, assemCtxPo ctx);
void ldaprh_(armReg dst, armOp src, assemCtxPo ctx);
void ldapur_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapurb_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapurh_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapursb_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapursh_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapursw_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldar_(armReg dst, armOp src, assemCtxPo ctx);
void ldarb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldarh_(armReg Rt, armReg Rn, assemCtxPo ctx);
void stlr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
void stlrb_(armReg Rd, armReg Rn, assemCtxPo ctx);
void stlrh_(armReg Rd, armReg Rn, assemCtxPo ctx);
void stlurb_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx);
void stlurh_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx);

void ldaxr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
void ldaxrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldaxrh_(armReg Rt, armReg Rn, assemCtxPo ctx);

void ldlarb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldlarh_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldlar_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx);
void stllrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void stllrh_(armReg Rd, armReg Rn, assemCtxPo ctx);
void stllr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);

void casab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caslb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casah_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casalh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void cash_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caslh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caspa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caspal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caspl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void cas_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);

#endif
