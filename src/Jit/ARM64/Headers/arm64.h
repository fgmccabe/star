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
  ZR = 31
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

typedef armOp registerSpec;
#define PLATFORM_PC_DELTA 0

#define RG(Rg) {.mode=Reg, .op.reg=(Rg)}
#define IM(Vl, Scale) {.mode=Immediate, .op.imm=(Vl), .op.scale=(Scale)}
#define BS(Rg, Off) {.mode=Based, .op.based.base=(Rg), .op.based.disp=(Off)}
#define IX(Rg, Ix, Sc) {.mode=Indexed, .op.indexed.base = (Rg), .op.indexed.index=(Ix),  .op.indexed.scale=(Sc)}
#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

typedef enum{
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

typedef enum{
  postIndex = 1,
  preIndex = 3,
  signedOff = 2,
  unsignedOff = 0
} prePostIndex;

void add_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void sub_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void and_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void orr_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void eor_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void neg_(armReg d, armOp s1, assemCtxPo ctx);
void mvn_(armReg d, armOp s1, assemCtxPo ctx);
void bic_(uint1 w, armShift tp, armReg RD, armReg Rn, armReg Rm, uint8 shift, assemCtxPo ctx);
void mov_(armReg d, armOp s1, assemCtxPo ctx);
void mul_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void sdiv_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void udiv_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);

void lsl_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void lsr_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);

void cmp_(armReg s1, armOp s2, assemCtxPo ctx);
void tst_(armReg s1, armOp s2, assemCtxPo ctx);

void b_cond_(armCond cond, codeLblPo lbl, assemCtxPo ctx);
void b_(codeLblPo lbl, assemCtxPo ctx);

void bl_(codeLblPo lbl, assemCtxPo ctx);
void blr_(armReg reg, assemCtxPo ctx);
void br_(armReg reg, assemCtxPo ctx);
void brk_(uint16 bkpt,assemCtxPo ctx);
void ret_(armReg reg, assemCtxPo ctx);

void cbnz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
void cbz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
void tbnz_(codeLblPo lbl, assemCtxPo ctx);
void tbz_(codeLblPo lbl, assemCtxPo ctx);

void clrex_(assemCtxPo ctx);
void dmb_(assemCtxPo ctx);
void dsb_(assemCtxPo ctx);
void isb_(assemCtxPo ctx);

void csdb_(assemCtxPo ctx);
void esb_(assemCtxPo ctx);

void adr(armReg d, armOp src, assemCtxPo ctx);

void ldr_(uint1 w,armReg Rt,armReg Rn,uint16 imm,prePostIndex ix,assemCtxPo ctx);
void ldrb_imm(armReg Rt, armReg Rn, uint16 imm, prePostIndex ix, assemCtxPo ctx);
void ldrsb_(armReg dst, armOp src, assemCtxPo ctx);
void ldrh_(armReg dst, armOp src, assemCtxPo ctx);
void ldrsh_(armReg dst, armOp src, assemCtxPo ctx);
void ldrw_(armReg dst, armOp src, assemCtxPo ctx);
void ldrsw_(armReg dst, armOp src, assemCtxPo ctx);

void ldur_(armReg dst, armOp src, assemCtxPo ctx);
void ldurb_(armReg dst, armOp src, assemCtxPo ctx);
void ldursb_(armReg dst, armOp src, assemCtxPo ctx);
void ldurh_(armReg dst, armOp src, assemCtxPo ctx);
void ldursh_(armReg dst, armOp src, assemCtxPo ctx);
void ldurw_(armReg dst, armOp src, assemCtxPo ctx);
void ldursw_(armReg dst, armOp src, assemCtxPo ctx);

void str_(armReg dst, armOp src, assemCtxPo ctx);
void strb_(armReg dst, armOp src, assemCtxPo ctx);
void strh_(armReg dst, armOp src, assemCtxPo ctx);

void stur_(armReg dst, armOp src, assemCtxPo ctx);
void sturb_(armReg dst, armOp src, assemCtxPo ctx);
void sturh_(armReg dst, armOp src, assemCtxPo ctx);


void ldp_(uint1 w,armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, prePostIndex ix, assemCtxPo ctx);
void ldpsw_(armReg Rt1,armReg Rt2, armReg Rn, uint8 imm,prePostIndex ix,assemCtxPo ctx);
void stp_(armReg dst, armOp src, assemCtxPo ctx);

void ldnp_(armReg dst, armOp src, assemCtxPo ctx);
void stnp_(armReg dst, armOp src, assemCtxPo ctx);

void ldxr_(armReg dst, armOp src, assemCtxPo ctx);
void ldxrb_(armReg dst, armOp src, assemCtxPo ctx);
void ldxrh_(armReg dst, armOp src, assemCtxPo ctx);
void ldxp_(armReg dst, armOp src, assemCtxPo ctx);
void stxr_(armReg dst, armOp src, assemCtxPo ctx);
void stxrb_(armReg dst, armOp src, assemCtxPo ctx);
void stxrh_(armReg dst, armOp src, assemCtxPo ctx);
void stxp(armReg dst, armOp src, assemCtxPo ctx);

void ldapr_(armReg dst, armOp src, assemCtxPo ctx);
void ldaprb_(armReg dst, armOp src, assemCtxPo ctx);
void ldaprh_(armReg dst, armOp src, assemCtxPo ctx);
void ldapur_(armReg dst, armOp src, assemCtxPo ctx);
void ldapurb_(armReg dst, armOp src, assemCtxPo ctx);
void ldapurh_(armReg dst, armOp src, assemCtxPo ctx);
void ldapursb_(armReg dst, armOp src, assemCtxPo ctx);
void ldapursh_(armReg dst, armOp src, assemCtxPo ctx);
void ldapursw_(armReg dst, armOp src, assemCtxPo ctx);
void ldar_(armReg dst, armOp src, assemCtxPo ctx);
void ldarb_(armReg dst, armOp src, assemCtxPo ctx);
void ldarh_(armReg dst, armOp src, assemCtxPo ctx);
void stlr_(armReg dst, armOp src, assemCtxPo ctx);
void stlrb_(armReg dst, armOp src, assemCtxPo ctx);
void stlrh_(armReg dst, armOp src, assemCtxPo ctx);
void stlurb_(armReg dst, armOp src, assemCtxPo ctx);
void stlurh_(armReg dst, armOp src, assemCtxPo ctx);

void ldaxr_(armReg dst, armOp src, assemCtxPo ctx);
void ldaxrb_(armReg dst, armOp src, assemCtxPo ctx);
void ldaxrh_(armReg dst, armOp src, assemCtxPo ctx);
void stlxr_(armReg dst, armOp src, assemCtxPo ctx);
void stlxrb_(armReg dst, armOp src, assemCtxPo ctx);
void stlxrh_(armReg dst, armOp src, assemCtxPo ctx);
void stlxp_(armReg dst, armOp src, assemCtxPo ctx);

void ldlarb_(armReg dst, armOp src, assemCtxPo ctx);
void ldlarh_(armReg dst, armOp src, assemCtxPo ctx);
void ldlar_(armReg dst, armOp src, assemCtxPo ctx);
void stllrb_(armReg dst, armOp src, assemCtxPo ctx);
void stllrh_(armReg dst, armOp src, assemCtxPo ctx);
void stllr_(armReg dst, armOp src, assemCtxPo ctx);

#endif
