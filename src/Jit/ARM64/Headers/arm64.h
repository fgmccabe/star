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
  X30 = 30,
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
  imm,  // Immediate value
  shft, // Shifting mode
  reg, // register
  extnd,  // extended
  postX, // post increment
  preX, // predecrement
  sOff, // signed offset
  uOff, // unsigned offset
  pcRel, // relative to PC
} FlexibleMode;

typedef enum {
  LSL = 0,
  LSR = 1,
  ASR = 2,
  ROR = 3
} armShift;

typedef enum {
  postIndex = 1,
  preIndex = 3,
  signedOff = 2,
  unsignedOff = 0
} ixMode;

typedef struct {
  FlexibleMode mode;
  uint64 immediate;
  uint1 hiLo;
  armReg reg;
  armReg rgm;
  armShift shift;
  armExtent ext;
  codeLblPo lbl;
} FlexOp;

typedef armOp registerSpec;
#define PLATFORM_PC_DELTA (0)

#define RG(Rg) {.mode=reg, .reg=(Rg)}
#define IM(Vl) {.mode=imm, .immediate=(Vl), .hiLo=0}
#define IMH(Vl) {.mode=imm, .immediate=(Vl), .hiLo=1}
#define LS(Rg, Amnt) {.mode=shft, .shift=LSL, .reg=Rg, .immediate=(Amnt)}
#define RS(Rg, Amnt) {.mode=shft, .shift=LSR, .reg=Rg, .immediate=(Amnt)}
#define AS(Rg, Amnt) {.mode=shft, .shift=ASR, .reg=Rg, .immediate=(Amnt)}
#define RR(Rg, Amnt) {.mode=shft, .shift=ROR, .reg=Rg, .immediate=(Amnt)}
#define EX(Rg, Md, Amnt) {.mode=extnd, .reg =Rg, .ext=Md, .immediate=(Amnt)}
#define EX2(Rg, Rgm, Md, Amnt) {.mode=extnd, .reg =Rg, .rgm=Rgm, .ext=Md, .immediate=(Amnt)}
#define PSX(Rg, Amnt) {.mode=postX, .reg=Rg, .immediate=(Amnt)}
#define PRX(Rg, Amnt) {.mode=preX,  .reg=Rg, .immediate=(Amnt)}
#define OF(Rg, Amnt) {.mode=sOff,  .reg=Rg, .immediate=(Amnt)}
#define UO(Rg, Amnt) {.mode=uOff, .reg=Rg, .immediate=(Amnt)}
#define PC(Lbl) {.mode=pcRel, .lbl=Lbl}

#define LB(l)  {.mode=Labeled, .op.lbl = (l)}

void adc_(uint1 wide, armReg rd, armReg s1, armReg s2, assemCtxPo ctx);
#define adc(rd, s1, s2, ctx) adc_(1, rd, s1, s2,ctx)
void adcs_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define adcs(rd, s1, s2, ctx) adcs_(1, rd, s1, s2,ctx)

void add_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define add(rd, s1, s2, ctx) do{ FlexOp s=s2; add_(1, rd, s1, s, ctx); } while(False)

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

void ccmn_(uint1 w, armReg Rn, armCond cnd, uint8 nzcv, FlexOp S2, assemCtxPo ctx);
void ccmp_(uint1 w, armReg Rn, armCond cnd, uint8 nzcv, FlexOp S2, assemCtxPo ctx);

void cinc_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx);
#define cinc(Rd, Rn, Cond, ctx) cinc_(1, Rd, Cond, Rn, ctx)

void cinv_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx);
#define cinv(Rd, Rn, Cond, ctx) cinv_(1, Rd, Cond, Rn, ctx)

void cls_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define cls(Rd, Rn, ctx) cls_(1,Rd, Rn, ctx)
void clz_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define clz(Rd, Rn, ctx) clz_(1,Rd, Rn, ctx)

void cmn_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define cmn(Rn, S2, ctx) do {FlexOp s=S2;  cmn_(1, Rn, s, ctx); } while(False)
void cmp_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define cmp(Rn, S2, ctx) do {FlexOp s=S2;  cmp_(1, Rn, s, ctx); } while(False)

void cneg_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx);
#define cneg(Rd, Rn, Cond, ctx) cneg_(1, Rd, Cond, Rn, ctx)
void csel_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csel(Rd, Rn, Rm, cond, ctx) csel_(1,Rd,Rn,Rm,cond,ctx);
void cset_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx);
#define cset(Rd, Cnd, Ctx) cset_(1,Rd,Cnd,Ctx);
void csetm_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx);
#define csetm(Rd, Cnd, Ctx) csetm_(1,Rd,Cnd,Ctx);
void csinc_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csinc(Rd, Rn, Rm, cond, ctx) csinc_(1,Rd,Rn,Rm,cond,ctx);
void csinv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csinv(Rd, Rn, Rm, cond, ctx) csinv_(1,Rd,Rn,Rm,cond,ctx);
void csneg_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csneg(Rd, Rn, Rm, cond, ctx) csneg_(1,Rd,Rn,Rm,cond,ctx);

void eor_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define eor(Rd, Rn, S2, ctx) do {FlexOp s=S2;  eor_(1, Rd, Rn, s, ctx); } while(False)

void extr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, uint8 lsb, assemCtxPo ctx);

void ld64b(armReg Rt, armReg Rn, assemCtxPo ctx);

void ldaddab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddab(Rs, Rt, Rn, ctx) ldaddab_(Rs, Rt, Rn, ctx)
void ldaddalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddalb(Rs, Rt, Rn, ctx) ldaddalb_(Rs, Rt, Rn, ctx)
void ldaddb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddb(Rs, Rt, Rn, ctx) ldaddb_(Rs, Rt, Rn, ctx)
void ldaddlb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddlb(Rs, Rt, Rn, ctx) ldaddlb_(Rs, Rt, Rn, ctx)

void ldadd_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldadd(Rs, Rt, Rn, ctx) ldadd_(1, Rs, Rt, Rn, ctx)
void ldadda_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldadda(Rs, Rt, Rn, ctx) ldadda_(1, Rs, Rt, Rn, ctx)
void ldaddal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddal(Rs, Rt, Rn, ctx) ldaddal_(1, Rs, Rt, Rn, ctx)
void ldaddl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddl(Rs, Rt, Rn, ctx) ldaddl_(1, Rs, Rt, Rn, ctx)

void ldapur_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapurb_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapurh_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapursb_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapursh_(uint1 w, armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldapursw_(armReg Rd, armReg Rn, int16 imm, assemCtxPo ctx);
void ldarb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldarh_(armReg Rt, armReg Rn, assemCtxPo ctx);

void ldaxr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
void ldaxrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldaxrh_(armReg Rt, armReg Rn, assemCtxPo ctx);

void ldlarb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldlarh_(armReg Rt, armReg Rn, assemCtxPo ctx);
void ldlar_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx);

void ldp_(uint1 w, armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx);
#define ldp(Rt, Rt2, S2, Ctx) do{FlexOp s=S2;  ldp_(1, Rt, Rt2, s, ctx); } while(False)

void ldpsw_(armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx);
#define ldpsw(Rt, Rt2, S2, Ctx) do{FlexOp s=S2;  ldpsw_(Rt, Rt2, s, ctx); } while(False)

void ldr_(uint1 w, armReg Rt, FlexOp Sn, assemCtxPo ctx);
#define ldr(Rt, S2, Ctx) do{FlexOp s=S2;  ldr_(1, Rt, s, ctx); } while(False)

void ldrb_(armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrb(Rt, S2, Ctx) do{FlexOp s=S2; ldrb_(Rt,s,ctx); } while(False)

void ldrh_(armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrh(Rt, S2, Ctx) do{FlexOp s=S2; ldrh_(Rt,s,ctx); } while(False)

void ldrsb_(uint1 w, armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrsb(Rt, S2, Ctx) do{FlexOp s=S2; ldrsb_(1, Rt,s,ctx); } while(False)

void ldrsh_(uint1 w, armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrsh(Rt, S2, Ctx) do{FlexOp s=S2; ldrsh_(1, Rt,s,ctx); } while(False)

void ldrsw_(armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrsw(Rt, S2, Ctx) do{FlexOp s=S2; ldrsw_(Rt,s,ctx); } while(False)

void ldur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldur(Rt, Rn, Imm, Ctx) ldur_(1,Rt,Rn,Imm,ctx)

void ldurb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldurb(Rt, Rn, Off, ctx) ldurb_(Rt,Rn,Off,ctx)
void ldursb_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldursb(Rt, Rn, Off, ctx) ldursb_(1, Rt,Rn,Off,ctx)
void ldurh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldurh(Rt, Rn, Off, ctx) ldurh_(Rt,Rn,Off,ctx)
void ldursh_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldursh(Rt, Rn, Off, ctx) ldursh_(1,Rt,Rn,Off,ctx)
void ldursw_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldursw(Rt, Rn, Off, ctx) ldursw_(Rt,Rn,Off,ctx)

void ldxr_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldxr(Rt, Rn, ctx) ldxr_(1,Rt,Rn,ctx)

void ldxrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldxrb(Rt, Rn, ctx) ldxrb_(Rt,Rn,ctx)
void ldxrh_(armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldxrh(Rt, Rn, ctx) ldxrh_(Rt,Rn,ctx)
void ldxp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, assemCtxPo ctx);
#define ldxp(Rt, Rt2, Rn, ctx) ldxp_(1,Rt,Rt2,Rn,ctx);

void lsl_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define lsl(Rd, Rn, S2, Ctx)  do{FlexOp s=S2; lsl_(1,Rd,Rn,s,ctx); } while(False)

void lsr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define lsr(Rd, Rn, S2, Ctx)  do{FlexOp s=S2; lsr_(1,Rd,Rn,s,ctx); } while(False)

void madd_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define madd(Rd, Rm, Rn, Ra, ctx) madd_(1,Rd,Rm,Rn,Ra,ctx)

void mov_(armReg d, FlexOp s1, assemCtxPo ctx);
#define mov(Rd, S, Ctx)  do{FlexOp s=S; mov_(Rd,s,ctx); } while(False)

void msub_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define msub(Rd, Rm, Rn, Ra, ctx) msub_(1,Rd,Rm,Rn,Ra,ctx)

void mul_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define mul(Rd, Rm, Rn, ctx) mul_(1,Rd,Rm,Rn,ctx)

void mvn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define mvn(Rd, Rm, sh, amnt, ctx) mvn_(1, Rd, Rm, sh, amnt, ctx)

void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define neg(Rd, Rm, sh, amnt, ctx) neg_(1, Rd, Rm, sh, amnt, ctx)

void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define negs(Rd, Rm, sh, amnt, ctx) negs_(1, Rd, Rm, sh, amnt, ctx)

void ngc_(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx);
#define ngc(Rd, Rm, ctx) ngc_(1,Rd,Rm,ctx)

void ngcs_(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx);
#define ngcs(Rd, Rm, ctx) ngcs_(1,Rd,Rm,ctx)

void nop(assemCtxPo ctx);

void orn_(uint1 w, armReg Rd, armReg RN, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define orn(Rd, Rn, Rm, sh, amnt, ctx) orn_(1, Rd, Rn, Rm, sh, amnt, ctx)

void orr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define orr(Rd, Rn, S, Ctx)  do{FlexOp s=S; orr_(1, Rd,Rn,s,ctx); } while(False)

void sub_(armReg d, armReg s1, FlexOp s2, assemCtxPo ctx);

void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void rbit_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define rbit(Rd, Rn, Ctx) rbit_(1, Rd, Rn, Ctx)

void ret_(armReg reg, assemCtxPo ctx);
#define ret(Rg, Ctx) ret_(Rg,Ctx)

void rev_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define rev(Rd, Rn, Ctx) rev_(1, Rd, Rn, Ctx)

void rev16_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define rev16(Rd, Rn, Ctx) rev16_(1, Rd, Rn, Ctx)

void rev32_(armReg Rd, armReg Rn, assemCtxPo ctx);
#define rev32(Rd, Rn, Ctx) rev32_(Rd, Rn, Ctx)

void ror_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define ror(Rd, Rn, S2, Ctx) do{FlexOp s=S2; ror_(1, Rd,Rn,s,ctx); } while(False)

void sbc_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define sbc(Rd, Rn, Rm, Ctx) sbc_(1, Rd, Rn, Rm, Ctx)

void sbcs_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define sbcs(Rd, Rn, Rm, Ctx) sbcs_(1, Rd, Rn, Rm, Ctx)

void sdiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define sdiv(Rd, Rn, Rm, Ctx) sdiv_(1, Rd, Rn, Rm, Ctx)

void smaddl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define smaddl(Rd, Rm, Rn, Ra, ctx) smaddl_(Rd,Rm,Rn,Ra,ctx)

void smulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define smulh(Rd, Rm, Rn, ctx) smulh_(Rd,Rm,Rn,ctx)

void smull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define smull(Rd, Rm, Rn, ctx) smull_(Rd,Rm,Rn,ctx)

void stp_(uint1 w, armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx);
#define stp(Rt, Rt2, S2, Ctx) do{FlexOp s=S2; stp_(1, Rt, Rt2, s,ctx); } while(False)

void udiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define udiv(Rd, Rn, Rm, Ctx) udiv_(1, Rd, Rn, Rm, Ctx)

void tbnz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx);
void tbz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx);

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

void ldnp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx);
void stnp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, uint8 imm, assemCtxPo ctx);

void stxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxr_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxrb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxrh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
void stlxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx);
void stxp_(uint1 w, armReg Rd, armReg Rt2, armReg Rn, assemCtxPo ctx);

void stlr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
void stlrb_(armReg Rd, armReg Rn, assemCtxPo ctx);
void stlrh_(armReg Rd, armReg Rn, assemCtxPo ctx);
void stlurb_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx);
void stlurh_(armReg Rd, armReg Rn, uint16 imm, assemCtxPo ctx);

void stllrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
void stllrh_(armReg Rd, armReg Rn, assemCtxPo ctx);
void stllr_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);

#endif
