#ifndef ARM64_H_
#define ARM64_H_

#include "config.h"
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
  PLE = 28,
  X29 = 29,
  FP = 29,
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
  int64 immediate;
  armReg reg;
  armReg rgm;
  armShift shift;
  armExtent ext;
  codeLblPo lbl;
} FlexOp;

typedef FlexOp registerSpec;

#define PLATFORM_PC_DELTA (0)

#define RG(Rg) ((FlexOp){.mode=reg, .reg=(Rg)})
#define IM(Vl) ((FlexOp){.mode=imm, .immediate=(Vl)})
#define LS(Rg, Amnt) ((FlexOp){.mode=shft, .shift=LSL, .reg=Rg, .immediate=(Amnt)})
#define RS(Rg, Amnt) ((FlexOp){.mode=shft, .shift=LSR, .reg=Rg, .immediate=(Amnt)})
#define AS(Rg, Amnt) ((FlexOp){.mode=shft, .shift=ASR, .reg=Rg, .immediate=(Amnt)})
#define RR(Rg, Amnt) ((FlexOp){.mode=shft, .shift=ROR, .reg=Rg, .immediate=(Amnt)})
#define EX(Rg, Md, Amnt) ((FlexOp){.mode=extnd, .reg =Rg, .ext=Md, .immediate=(Amnt)})
#define EX2(Rg, Rgm, Md, Amnt) ((FlexOp){.mode=extnd, .reg =Rg, .rgm=Rgm, .ext=Md, .immediate=(Amnt)})
#define PSX(Rg, Amnt) ((FlexOp){.mode=postX, .reg=Rg, .immediate=(Amnt)})
#define PRX(Rg, Amnt) ((FlexOp){.mode=preX,  .reg=Rg, .immediate=(Amnt)})
#define OF(Rg, Amnt) ((FlexOp){.mode=sOff,  .reg=Rg, .immediate=(Amnt)})
#define PC(Lbl) ((FlexOp){.mode=pcRel, .lbl=(Lbl)})

codeLblPo preamble(assemCtxPo ctx, int32 lclSize);
retCode postamble(assemCtxPo ctx);

void adc_(uint1 wide, armReg rd, armReg s1, armReg s2, assemCtxPo ctx);
#define adc(rd, s1, s2) adc_(1, rd, s1, s2,ctx)
void adcs_(uint1 wide, armReg rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define adcs(rd, s1, s2) adcs_(1, rd, s1, s2,ctx)

void add_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define add(rd, s1, s2) do{ FlexOp s=s2; add_(1, rd, s1, s, ctx); } while(False)

void adds_x(uint1 w, armReg Rd, armReg Rn, armReg Rm, armExtent ex, uint8 shift, assemCtxPo ctx);
void adds_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define adds(rd, s1, s2) do{ FlexOp s=s2; adds_(1, rd, s1, s, ctx); } while(False)

void adr_(armReg Rd, codeLblPo lbl, assemCtxPo ctx);
#define adr(Rd, lbl) do{ adr_(Rd, lbl, ctx); } while(False)

void adrp_(armReg Rd, codeLblPo lbl, assemCtxPo ctx);

void and_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define and(rd, s1, s2) do{ FlexOp s=s2; and_(1, rd, s1, s, ctx); } while(False)

void ands_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define ands(rd, s1, s2) do{ FlexOp s=s2; ands_(1, rd, s1, s, ctx); } while(False)

void asr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define asr(rd, s1, s2) do{ FlexOp s=s2; asr_(1, rd, s1, s, ctx); } while(False)

void b_cond_(armCond cond, codeLblPo lbl, assemCtxPo ctx);
#define beq(lbl) do{ b_cond_(EQ, lbl, ctx); } while(False)
#define bne(lbl) do{ b_cond_(NE, lbl, ctx); } while(False)
#define bcs(lbl) do{ b_cond_(CS, lbl, ctx); } while(False)
#define bcc(lbl) do{ b_cond_(CC, lbl, ctx); } while(False)
#define bmi(lbl) do{ b_cond_(MI, lbl, ctx); } while(False)
#define bpl(lbl) do{ b_cond_(PL, lbl, ctx); } while(False)
#define bvs(lbl) do{ b_cond_(VS, lbl, ctx); } while(False)
#define bvc(lbl) do{ b_cond_(VC, lbl, ctx); } while(False)
#define bhi(lbl) do{ b_cond_(HI, lbl, ctx); } while(False)
#define bls(lbl) do{ b_cond_(LS, lbl, ctx); } while(False)
#define bge(lbl) do{ b_cond_(GE, lbl, ctx); } while(False)
#define blt(lbl) do{ b_cond_(LT, lbl, ctx); } while(False)
#define bgt(lbl) do{ b_cond_(GT, lbl, ctx); } while(False)
#define ble(lbl) do{ b_cond_(LE, lbl, ctx); } while(False)
#define bal(lbl) do{ b_cond_(AL, lbl, ctx); } while(False)
#define bnv(lbl) do{ b_cond_(NV, lbl, ctx); } while(False)

void b_(codeLblPo lbl, assemCtxPo ctx);
#define b(lbl) do{ b_(lbl, ctx); } while(False)

void bfc_(uint1 w, armReg RD, uint8 bit, uint8 width, assemCtxPo ctx);
#define bfc(Rd, Wdth, Bit) do {bfc_(1, Rd, Wdth, Bit, ctx); } while(False)

void bfi_(uint1 w, armReg RD, armReg Rn, uint8 bit, uint8 width, assemCtxPo ctx);
#define bfi(Rd, Rn, Wdth, Bit) do {bfi_(1, Rd, Rn, Wdth, Bit, ctx); } while(False)

void bfxil_(uint1 w, armReg RD, armReg Rn, uint8 bit, uint8 width, assemCtxPo ctx);
#define bfxil(Rd, Rn, Wdth, Bit) do {bfxil_(1, Rd, Rn, Wdth, Bit, ctx); } while(False)

void bic_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define bic(Rd, Rn, S2) do {FlexOp s=S2; bic_(1, Rd, Rn, s, ctx); } while(False)

void bics_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define bics(Rd, Rn, S2) do {FlexOp s=S2;  bics_(1, Rd, Rn, s, ctx); } while(False)

void bl_(codeLblPo lbl, assemCtxPo ctx);
#define bl(lbl) do{ bl_(lbl, ctx); } while(False)

void blr_(armReg reg, assemCtxPo ctx);
#define blr(reg) do{ blr_(reg, ctx); } while(False)

void br_(armReg reg, assemCtxPo ctx);
#define br(reg) do{ br_(reg, ctx); } while(False)

void brk_(uint16 bkpt, assemCtxPo ctx);
#define brk(bkpt) do{ brk_(bkpt, ctx); } while(False)

void casab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casab(Rs, Rt, Rn) casab_(Rs, Rt, Rn, ctx)

void casalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casalb(Rs, Rt, Rn) casalb_(Rs, Rt, Rn, ctx)

void casb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casb(Rs, Rt, Rn) casb_(Rs, Rt, Rn, ctx)

void caslb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caslb(Rs, Rt, Rn) caslb_(Rs, Rt, Rn, ctx)

void casah_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casah(Rs, Rt, Rn) casah_(Rs, Rt, Rn, ctx)

void casalh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casalh(Rs, Rt, Rn) casalh_(Rs, Rt, Rn, ctx)

void cash_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define cash(Rs, Rt, Rn) cash_(Rs, Rt, Rn, ctx)

void caslh_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caslh(Rs, Rt, Rn) caslh_(Rs, Rt, Rn, ctx)

void casp_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casp(Rs, Rt, Rn) casp_(1, Rs, Rt, Rn, ctx)
void caspa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caspa(Rs, Rt, Rn) caspa_(1, Rs, Rt, Rn, ctx)
void caspal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caspal(Rs, Rt, Rn) caspal_(1, Rs, Rt, Rn, ctx)
void caspl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define caspl(Rs, Rt, Rn) caspl_(1, Rs, Rt, Rn, ctx)

void casa_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casa(Rs, Rt, Rn) casa_(1, Rs, Rt, Rn, ctx)
void casal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casal(Rs, Rt, Rn) casal_(1, Rs, Rt, Rn, ctx)
void cas_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define cas(Rs, Rt, Rn) cas_(1, Rs, Rt, Rn, ctx)
void casl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define casl(Rs, Rt, Rn) casl_(1, Rs, Rt, Rn, ctx)

void cbnz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
#define cbnz(Rt, Lbl) cbnz_(1, Rt, Lbl, ctx)
void cbz_(uint1 w, armReg Rt, codeLblPo lbl, assemCtxPo ctx);
#define cbz(Rt, Lbl) cbz_(1, Rt, Lbl, ctx)

void ccmn_(uint1 w, armReg Rn, armCond cnd, uint8 nzcv, FlexOp S2, assemCtxPo ctx);
void ccmp_(uint1 w, armReg Rn, armCond cnd, uint8 nzcv, FlexOp S2, assemCtxPo ctx);

void cinc_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx);
#define cinc(Rd, Rn, Cond) cinc_(1, Rd, Cond, Rn, ctx)

void cinv_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx);
#define cinv(Rd, Rn, Cond) cinv_(1, Rd, Cond, Rn, ctx)

void cls_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define cls(Rd, Rn) cls_(1,Rd, Rn, ctx)
void clz_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define clz(Rd, Rn) clz_(1,Rd, Rn, ctx)

void cmn_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define cmn(Rn, S2) do {FlexOp s=S2;  cmn_(1, Rn, s, ctx); } while(False)
void cmp_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define cmp(Rn, S2) do {FlexOp s=S2;  cmp_(1, Rn, s, ctx); } while(False)

void cneg_(uint1 w, armReg Rd, armCond cond, armReg Rn, assemCtxPo ctx);
#define cneg(Rd, Rn, Cond) cneg_(1, Rd, Cond, Rn, ctx)
void csel_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csel(Rd, Rn, Rm, cond) csel_(1,Rd,Rn,Rm,cond,ctx);
void cset_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx);
#define cset(Rd, Cnd) cset_(1,Rd,Cnd,ctx);
void csetm_(uint1 w, armReg Rd, armCond cond, assemCtxPo ctx);
#define csetm(Rd, Cnd) csetm_(1,Rd,Cnd,ctx);
void csinc_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csinc(Rd, Rn, Rm, cond) csinc_(1,Rd,Rn,Rm,cond,ctx);
void csinv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csinv(Rd, Rn, Rm, cond) csinv_(1,Rd,Rn,Rm,cond,ctx);
void csneg_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armCond cond, assemCtxPo ctx);
#define csneg(Rd, Rn, Rm, cond) csneg_(1,Rd,Rn,Rm,cond,ctx);

void eor_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define eor(Rd, Rn, S2) do {FlexOp s=S2;  eor_(1, Rd, Rn, s, ctx); } while(False)

void extr_(uint1 w, armReg Rd, armReg Rn, armReg Rm, uint8 lsb, assemCtxPo ctx);

void ld64b(armReg Rt, armReg Rn, assemCtxPo ctx);

void ldaddab_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddab(Rs, Rt, Rn) ldaddab_(Rs, Rt, Rn, ctx)
void ldaddalb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddalb(Rs, Rt, Rn) ldaddalb_(Rs, Rt, Rn, ctx)
void ldaddb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddb(Rs, Rt, Rn) ldaddb_(Rs, Rt, Rn, ctx)
void ldaddlb_(armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddlb(Rs, Rt, Rn) ldaddlb_(Rs, Rt, Rn, ctx)

void ldadd_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldadd(Rs, Rt, Rn) ldadd_(1, Rs, Rt, Rn, ctx)
void ldadda_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldadda(Rs, Rt, Rn) ldadda_(1, Rs, Rt, Rn, ctx)
void ldaddal_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddal(Rs, Rt, Rn) ldaddal_(1, Rs, Rt, Rn, ctx)
void ldaddl_(uint1 w, armReg Rs, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldaddl(Rs, Rt, Rn) ldaddl_(1, Rs, Rt, Rn, ctx)

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
#define ldp(Rt, Rt2, S2) do{FlexOp s=S2;  ldp_(1, Rt, Rt2, s, ctx); } while(False)

void ldpsw_(armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx);
#define ldpsw(Rt, Rt2, S2) do{FlexOp s=S2;  ldpsw_(Rt, Rt2, s, ctx); } while(False)

void ldr_(uint1 w, armReg Rt, FlexOp Sn, assemCtxPo ctx);
#define ldr(Rt, S2) do{FlexOp s=S2;  ldr_(1, Rt, s, ctx); } while(False)

void ldrb_(armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrb(Rt, S2) do{FlexOp s=S2; ldrb_(Rt,s,ctx); } while(False)

void ldrh_(armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrh(Rt, S2) do{FlexOp s=S2; ldrh_(Rt,s,ctx); } while(False)

void ldrsb_(uint1 w, armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrsb(Rt, S2) do{FlexOp s=S2; ldrsb_(1, Rt,s,ctx); } while(False)

void ldrsh_(uint1 w, armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrsh(Rt, S2) do{FlexOp s=S2; ldrsh_(1, Rt,s,ctx); } while(False)

void ldrsw_(armReg Rt, FlexOp S2, assemCtxPo ctx);
#define ldrsw(Rt, S2) do{FlexOp s=S2; ldrsw_(Rt,s,ctx); } while(False)

void ldur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldur(Rt, Rn, Imm) ldur_(1,Rt,Rn,Imm,ctx)

void ldtr_(uint1 w, armReg Rt, armReg Rn, int16 off, assemCtxPo ctx);
#define ldtr(Rt, Rn, Off) ldtr_(1,Rt,Rn,Off,ctx)

void ldurb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldurb(Rt, Rn, Off) ldurb_(Rt,Rn,Off,ctx)
void ldursb_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldursb(Rt, Rn, Off) ldursb_(1, Rt,Rn,Off,ctx)
void ldurh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldurh(Rt, Rn, Off) ldurh_(Rt,Rn,Off,ctx)
void ldursh_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldursh(Rt, Rn, Off) ldursh_(1,Rt,Rn,Off,ctx)
void ldursw_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define ldursw(Rt, Rn, Off) ldursw_(Rt,Rn,Off,ctx)

void ldxr_(uint1 w, armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldxr(Rt, Rn) ldxr_(1,Rt,Rn,ctx)

void ldxrb_(armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldxrb(Rt, Rn) ldxrb_(Rt,Rn,ctx)
void ldxrh_(armReg Rt, armReg Rn, assemCtxPo ctx);
#define ldxrh(Rt, Rn) ldxrh_(Rt,Rn,ctx)
void ldxp_(uint1 w, armReg Rt1, armReg Rt2, armReg Rn, assemCtxPo ctx);
#define ldxp(Rt, Rt2, Rn) ldxp_(1,Rt,Rt2,Rn,ctx);

void lsl_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define lsl(Rd, Rn, S2)  do{FlexOp s=S2; lsl_(1,Rd,Rn,s,ctx); } while(False)

void lsr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define lsr(Rd, Rn, S2)  do{FlexOp s=S2; lsr_(1,Rd,Rn,s,ctx); } while(False)

void madd_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define madd(Rd, Rm, Rn, Ra) madd_(1,Rd,Rm,Rn,Ra,ctx)

void mov_(armReg d, FlexOp s1, assemCtxPo ctx);
#define mov(Rd, S)  do{FlexOp s=S; mov_(Rd,s,ctx); } while(False)

void msub_(uint1 w, armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define msub(Rd, Rm, Rn, Ra) msub_(1,Rd,Rm,Rn,Ra,ctx)

void mul_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define mul(Rd, Rm, Rn) mul_(1,Rd,Rm,Rn,ctx)

void mvn_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define mvn(Rd, Rm, sh, amnt) mvn_(1, Rd, Rm, sh, amnt, ctx)

void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define neg(Rd, Rm, sh, amnt) neg_(1, Rd, Rm, sh, amnt, ctx)

void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define negs(Rd, Rm, sh, amnt) negs_(1, Rd, Rm, sh, amnt, ctx)

void ngc_(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx);
#define ngc(Rd, Rm) ngc_(1,Rd,Rm,ctx)

void ngcs_(uint1 w, armReg Rd, armReg Rm, assemCtxPo ctx);
#define ngcs(Rd, Rm) ngcs_(1,Rd,Rm,ctx)

void nop(assemCtxPo ctx);

void orn_(uint1 w, armReg Rd, armReg RN, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
#define orn(Rd, Rn, Rm, sh, amnt) orn_(1, Rd, Rn, Rm, sh, amnt, ctx)

void orr_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define orr(Rd, Rn, S)  do{FlexOp s=S; orr_(1, Rd,Rn,s,ctx); } while(False)

void neg_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void negs_(uint1 w, armReg Rd, armReg Rm, armShift sh, int8 amnt, assemCtxPo ctx);
void rbit_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define rbit(Rd, Rn) rbit_(1, Rd, Rn, ctx)

void ret_(armReg reg, assemCtxPo ctx);
#define ret(Rg) ret_(Rg, ctx)

void rev_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define rev(Rd, Rn) rev_(1, Rd, Rn, ctx)

void rev16_(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);
#define rev16(Rd, Rn) rev16_(1, Rd, Rn, ctx)

void rev32_(armReg Rd, armReg Rn, assemCtxPo ctx);
#define rev32(Rd, Rn) rev32_(Rd, Rn, ctx)

void ror_(uint1 w, armReg Rd, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define ror(Rd, Rn, S2) do{FlexOp s=S2; ror_(1, Rd,Rn,s,ctx); } while(False)

void sbc_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define sbc(Rd, Rn, Rm) sbc_(1, Rd, Rn, Rm, ctx)

void sbcs_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define sbcs(Rd, Rn, Rm) sbcs_(1, Rd, Rn, Rm, ctx)

void sdiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define sdiv(Rd, Rn, Rm) sdiv_(1, Rd, Rn, Rm, ctx)

void smaddl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define smaddl(Rd, Rm, Rn, Ra) smaddl_(Rd,Rm,Rn,Ra,ctx)

void smulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define smulh(Rd, Rm, Rn) smulh_(Rd,Rm,Rn,ctx)

void smull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define smull(Rd, Rm, Rn) smull_(Rd,Rm,Rn,ctx)

void stp_(uint1 w, armReg Rt, armReg Rt2, FlexOp Sn, assemCtxPo ctx);
#define stp(Rt, Rt2, S2) do{FlexOp s=S2; stp_(1, Rt, Rt2, s,ctx); } while(False)

void str_(uint1 w, armReg Rt, FlexOp Sn, assemCtxPo ctx);
#define str(Rt, S2) do{FlexOp s=S2; str_(1, Rt, s,ctx); } while(False)

void strb_(armReg Rt, FlexOp Sn, assemCtxPo ctx);
#define strb(Rt, S2) do{FlexOp s=S2; strb_( Rt, s,ctx); } while(False)

void strh_(armReg Rt, FlexOp Sn, assemCtxPo ctx);
#define strh(Rt, S2) do{FlexOp s=S2; strh_( Rt, s,ctx); } while(False)

void sttr_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define sttr(Rt, Rn, Off) sttr_(1,Rt,Rn,Off,ctx)

void stur_(uint1 w, armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define stur(Rt, Rn, Off) stur_(1,Rt,Rn,Off,ctx)

void sturb_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define sturb(Rt, Rn, Off) sturb_(Rt,Rn,Off,ctx)

void sturh_(armReg Rt, armReg Rn, int16 imm, assemCtxPo ctx);
#define sturh(Rt, Rn, Off) sturh_(Rt,Rn,Off,ctx)

void sub_(uint1 w, armReg Rd, armReg Rn, FlexOp s2, assemCtxPo ctx);
#define sub(Rd, Rn, S) do{FlexOp s=S; sub_(1,Rd,Rn,s,ctx); } while(False)

void subs_(uint1 w, armReg Rd, armReg Rn, FlexOp s2, assemCtxPo ctx);
#define subs(Rd, Rn, S) do{FlexOp s=S; subs_(1,Rd,Rn,s,ctx); } while(False)

void sxtb(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);

void sxth(uint1 w, armReg Rd, armReg Rn, assemCtxPo ctx);

void sxtw(armReg Rd, armReg Rn, assemCtxPo ctx);

void tbnz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx);
#define tbnz(Rt, b, lbl) tbnz_(1,Rt,b,lbl,ctx)

void tbz_(uint1 w, armReg Rt, uint8 pos, codeLblPo lbl, assemCtxPo ctx);
#define tbz(Rt, b, lbl) tbz_(1,Rt,b,lbl,ctx)

void tst_(uint1 w, armReg Rn, FlexOp S2, assemCtxPo ctx);
#define tst(s1, s2) do{ FlexOp s=s2; tst_(1, s1, s, ctx); } while(False)

void udiv_(uint1 w, armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define udiv(Rd, Rn, Rm) udiv_(1, Rd, Rn, Rm, ctx)

void umaddl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define umaddl(Rd, Rm, Rn, Ra) umaddl_(Rd,Rm,Rn,Ra,ctx)

void umnegl_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define umnegl(Rd, Rm, Rn) umnegl_(Rd,Rm,Rn,ctx)

void umsubl_(armReg Rd, armReg Rn, armReg Rm, armReg Ra, assemCtxPo ctx);
#define umsubl(Rd, Rm, Rn, Ra) umsubl_(Rd,Rm,Rn,Ra,ctx)

void umulh_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define umulh(Rd, Rm, Rn) umulh_(Rd,Rm,Rn,ctx)

void umull_(armReg Rd, armReg Rn, armReg Rm, assemCtxPo ctx);
#define umull(Rd, Rm, Rn) umull_(Rd,Rm,Rn,ctx)

void bfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx);
void sbfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx);
void ubfm_(uint1 w, armReg Rd, armReg Rn, uint8 immr, uint8 imms, assemCtxPo ctx);

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

void labelConst(codeLblPo lbl, assemCtxPo ctx);

#endif
