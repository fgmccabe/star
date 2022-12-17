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
  uint64 immediate;
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

#define RG(Rg) {.mode=reg, .reg=(Rg)}
#define IM(Vl) {.mode=imm, .immediate=(Vl), .hiLo=0}
#define IMH(Vl) {.mode=imm, .immediate=(Vl), .hiLo=1}
#define LS(Rg, Amnt) {.mode=lsli, .reg=Rg, .immediate=(Amnt)}
#define RS(Rg, Amnt) {.mode=lsri, .reg=Rg, .immediate=(Amnt)}
#define AS(Rg, Amnt) {.mode=asri, .reg=Rg, .immediate=(Amnt)}
#define RR(Rg, Amnt) {.mode=rori, .reg=Rg, .immediate=(Amnt)}

#define BS(Rg, Off) {.mode=Based, .op.based.base=(Rg), .op.based.disp=(Off)}
#define IX(Rg, Ix, Sc, Off) {.mode=Indexed, .op.indexed.base = (Rg), .op.indexed.index=(Ix), .op.indexed.disp=(Off), .op.indexed.scale=(Sc)}
#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

void adc_(uint1 wide, armReg rd, armReg s1, armReg s2, assemCtxPo ctx);
#define adc(rd, s1, s2, ctx) adc_(1, rd, s1, s2,ctx)
void adcs_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define adcs(rd, s1, s2, ctx) adcs_(1, rd, s1, s2,ctx)

void add_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define add(rd, s1, s2, ctx) do{ FlexOp s=s2; add_(1, rd, s1, s, ctx); } while(False)

void add_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx);
void adds_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx);
void adds_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define adds(rd, s1, s2, ctx) do{ FlexOp s=s2; adds_(1, rd, s1, s, ctx); } while(False)

void adr_(armReg Rd, codeLblPo lbl, assemCtxPo ctx);
#define adr(Rd, lbl, ctx) do{ adr_(Rd, lbl, ctx); } while(False)

void adrp_(armReg Rd, codeLblPo lbl, assemCtxPo ctx);

void and_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define and(rd, s1, s2, ctx) do{ FlexOp s=s2; and_(1, rd, s1, s, ctx); } while(False)

void ands_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define ands(rd, s1, s2, ctx) do{ FlexOp s=s2; ands_(1, rd, s1, s, ctx); } while(False)

void asr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define asr(rd, s1, s2, ctx) do{ FlexOp s=s2; asr_(1, rd, s1, s, ctx); } while(False)

void b_cond_(armCond cond, codeLblPo lbl, assemCtxPo ctx);
#define beq(lbl, ctx) do{ b_cond_(EQ, lbl, ctx); } while(False)
#define bne(lbl, ctx) do{ b_cond_(NE, lbl, ctx); } while(False)
#define bcs(lbl, ctx) do{ b_cond_(CS, lbl, ctx); } while(False)
#define bcc(lbl, ctx) do{ b_cond_(CC, lbl, ctx); } while(False)
#define bmi(lbl, ctx) do{ b_cond_(MI, lbl, ctx); } while(False)
#define bpl(lbl, ctx) do{ b_cond_(PL, lbl, ctx); } while(False)
#define bvs(lbl, ctx) do{ b_cond_(VS, lbl, ctx); } while(False)
#define bvc(lbl, ctx) do{ b_cond_(VC, lbl, ctx); } while(False)
#define bhi(lbl, ctx) do{ b_cond_(HI, lbl, ctx); } while(False)
#define bls(lbl, ctx) do{ b_cond_(LS, lbl, ctx); } while(False)
#define bge(lbl, ctx) do{ b_cond_(GE, lbl, ctx); } while(False)
#define blt(lbl, ctx) do{ b_cond_(LT, lbl, ctx); } while(False)
#define bgt(lbl, ctx) do{ b_cond_(GT, lbl, ctx); } while(False)
#define ble(lbl, ctx) do{ b_cond_(LE, lbl, ctx); } while(False)
#define bal(lbl, ctx) do{ b_cond_(AL, lbl, ctx); } while(False)
#define bnv(lbl, ctx) do{ b_cond_(NV, lbl, ctx); } while(False)

void b_(codeLblPo lbl, assemCtxPo ctx);
#define b(lbl, ctx) do{ b_(lbl, ctx); } while(False)

void bfc_(uint1 w, armReg RD, uint8 bit, uint8 width, assemCtxPo ctx);
#define bfc(Rd, Wdth, Bit, ctx) do {bfc_(1, Rd, Wdth, Bit, ctx); } while(False)

void bfi_(uint1 w, armReg RD, armReg Rn, uint8 bit, uint8 width, assemCtxPo ctx);
#define bfi(Rd, Rn, Wdth, Bit, ctx) do {bfi_(1, Rd, Rn, Wdth, Bit, ctx); } while(False)

void bfxil_(uint1 w, armReg RD, armReg Rn, uint8 bit, uint8 width, assemCtxPo ctx);
#define bfxil(Rd, Rn, Wdth, Bit, ctx) do {bfxil_(1, Rd, Rn, Wdth, Bit, ctx); } while(False)

void bic_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define bic(Rd, Rn, S2, ctx) do {FlexOp s=S2; bic_(1, Rd, Rn, s, ctx); } while(False)

void bics_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define bics(Rd, Rn, S2, ctx) do {FlexOp s=S2;  bics_(1, Rd, Rn, s, ctx); } while(False)

void bl_(codeLblPo lbl, assemCtxPo ctx);
#define bl(lbl, ctx) do{ bl_(lbl, ctx); } while(False)

void blr_(armReg reg, assemCtxPo ctx);
#define blr(reg, ctx) do{ blr_(reg, ctx); } while(False)

void br_(armReg reg, assemCtxPo ctx);
#define br(reg, ctx) do{ br_(reg, ctx); } while(False)

void brk_(uint16 bkpt, assemCtxPo ctx);
#define brk(bkpt, ctx) do{ brk_(bkpt, ctx); } while(False)

void casab(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casalb(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casb(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caslb(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casah(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casalh(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void cash(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void caslh(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void casp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casp(Rs, Rt, Rn, cxt) casp_(1, Rs, Rt, Rn, ctx)
void caspa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caspa(Rs, Rt, Rn, cxt) caspa_(1, Rs, Rt, Rn, ctx)
void caspal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caspal(Rs, Rt, Rn, cxt) caspal_(1, Rs, Rt, Rn, ctx)
void caspl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caspl(Rs, Rt, Rn, cxt) caspl_(1, Rs, Rt, Rn, ctx)

void casa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casa(Rs, Rt, Rn, cxt) casa_(1, Rs, Rt, Rn, ctx)
void casal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casal(Rs, Rt, Rn, cxt) casal_(1, Rs, Rt, Rn, ctx)
void cas_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define cas(Rs, Rt, Rn, cxt) cas_(1, Rs, Rt, Rn, ctx)
void casl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casl(Rs, Rt, Rn, cxt) casl_(1, Rs, Rt, Rn, ctx)

void cbnz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
#define cbnz(Rt, Lbl, ctx) cbnz_(1, Rt, Lbl, ctx)
void cbz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
#define cbz(Rt, Lbl, ctx) cbz_(1, Rt, Lbl, ctx)

void sub_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);

void orn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void orr_imm(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void orr_sh_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);

void eor_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);
void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void mvn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void mov_(armReg d, armOp s1, assemCtxPo ctx);
void mul_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
void sdiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
void udiv_(armReg d, armReg s1, armOp s2, assemCtxPo ctx);

void lsl_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
void lsr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);

void cmp_(armReg s1, armOp s2, assemCtxPo ctx);
void tst_(armReg s1, armOp s2, assemCtxPo ctx);

void ret_(armReg reg, assemCtxPo ctx);


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


#endif
