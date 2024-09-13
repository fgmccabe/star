//
// Created by Francis McCabe on 12/12/22.
//
#include "unitTests.h"
#include "arm64P.h"
#include "test_infra.h"
#include "armRegSetTest.h"

static retCode test_adc() {
  assemCtxPo ctx = createCtx();

  adc(X12, X13, X14);
  adc(X1, X2, X3);
  adcs(X12, X13, X14);
  adcs(X1, X2, X3);

  add(X3, X4, RG(X6));
  add(X3, X5, IM(135));
  add(X3, X5, IM(0x87000));
  add(X2, X4, LS(X6, 3));
  add(X1, X12, RS(X18, 3));
  add(X1, X12, AS(X18, 3));

  adds(X3, X4, RG(X6));
  adds(X3, X5, IM(135));
  adds(X3, X5, IM(0x87000));
  adds(X2, X4, LS(X6, 3));
  adds(X1, X12, RS(X18, 3));
  adds(X1, X12, AS(X18, 3));

  uint8 tgt[] = {0xac, 0x01, 0x0e, 0x9a, // adc x12, x13, x14
                 0x41, 0x00, 0x03, 0x9a, // adc x1, x2, x3
                 0xac, 0x01, 0x0e, 0xba, // adcs x12, x13, x14
                 0x41, 0x00, 0x03, 0xba, // adcs x1, x2, x3
                 0x83, 0x00, 0x06, 0x8b, // add x3, x4, x6
                 0xa3, 0x1c, 0x02, 0x91, //	add X3, X5, 135
                 0xa3, 0x1c, 0x42, 0x91, //	add X3, X5, 135 lsl 12
                 0x82, 0x0c, 0x06, 0x8b, // add X2, X4, X6, lsl 3
                 0x81, 0x0d, 0x52, 0x8b, // add x1, x12, x18, lsr 3
                 0x81, 0x0d, 0x92, 0x8b, // add x1, x12, x18, asr 3
                 0x83, 0x00, 0x06, 0xab, //	adds X3, X4, X6
                 0xa3, 0x1c, 0x02, 0xb1, // adds x3, x5, 135
                 0xa3, 0x1c, 0x42, 0xb1, // adds x3, x5, 135, lsl 12
                 0x82, 0x0c, 0x06, 0xab, // adds x2, x4, x6, lsl 3
                 0x81, 0x0d, 0x52, 0xab, // adds x1, x12, x18, lsr 3
                 0x81, 0x0d, 0x92, 0xab // adds x1, x12, x18, asr 3
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_adr() {
  assemCtxPo ctx = createCtx();
  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);

  adr(X10, l0);
  adr(X12, l1);

  setLabel(ctx, l1);

  uint8 tgt[] = {0x0a, 0x00, 0x00, 0x10, // adr x10, adc
                 0x2c, 0x00, 0x00, 0x10, // adr 12, 1f
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_and() {
  assemCtxPo ctx = createCtx();

  and(X12, X13, IM(0x3333333333333333));
  and(X12, X13, IM(0xff00));
  and(X1, X2, RG(X3));
  and(X3, X5, LS(X6, 3));
  and(X3, X5, RS(X6, 3));
  and(X3, X5, RR(X6, 3));

  ands(X12, X13, IM(0x3333333333333333));
  ands(X12, X13, IM(0xff00));
  ands(X1, X2, RG(X3));
  ands(X3, X5, LS(X6, 3));
  ands(X3, X5, RS(X6, 3));
  ands(X3, X5, RR(X6, 3));

  asr(X12, X13, IM(0xf));
  asr(X1, X2, RG(X3));

  eor(X17, X9, IM(0x3333333333333333));
  eor(X18, X10, RG(X12));
  eor(X18, X10, RR(X12, 5));

  tst(X13, IM(0x3333333333333333));
  tst(X13, IM(0xff00));
  tst(X2, RG(X3));
  tst(X5, LS(X6, 3));
  tst(X5, RS(X6, 3));
  tst(X5, RR(X6, 3));

  uint8 tgt[] = {0xac, 0xe5, 0x00, 0x92,  //and	X12, X13, #0x3333333333333333
                 0xac, 0x1d, 0x78, 0x92, // and x12, x13, #0xff00
                 0x41, 0x00, 0x03, 0x8a,  // and x1, x2, x3
                 0xa3, 0x0c, 0x06, 0x8a, // and x3,x5,x6, lsl 3
                 0xa3, 0x0c, 0x46, 0x8a, // and x3,x5,x6, lsr 3
                 0xa3, 0x0c, 0xc6, 0x8a, // and x3,x5,x6, ror 3
                 0xac, 0xe5, 0x00, 0xf2,//ands	X12, X13, #0x3333333333333333
                 0xac, 0x1d, 0x78, 0xf2,// ands x12, x13, #0xff00
                 0x41, 0x00, 0x03, 0xea, // ands x1, x2, x3
                 0xa3, 0x0c, 0x06, 0xea,// ands x3,x5,x6, lsl 3
                 0xa3, 0x0c, 0x46, 0xea,// ands x3,x5,x6, lsr 3
                 0xa3, 0x0c, 0xc6, 0xea, // ands x3,x5,x6, ror 3
                 0xac, 0xfd, 0x4f, 0x93, // asr x12, x13, 0xf
                 0x41, 0x28, 0xc3, 0x9a, // asr x1, x2, x3
                 0x31, 0xe5, 0x00, 0xd2, // eor
                 0x52, 0x01, 0x0c, 0xca,
                 0x52, 0x15, 0xcc, 0xca,

                 0xbf, 0xe5, 0x00, 0xf2, // tst
                 0xbf, 0x1d, 0x78, 0xf2,
                 0x5f, 0x00, 0x03, 0xea,
                 0xbf, 0x0c, 0x06, 0xea,
                 0xbf, 0x0c, 0x46, 0xea,
                 0xbf, 0x0c, 0xc6, 0xea
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_b() {
  assemCtxPo ctx = createCtx();
  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  codeLblPo l2 = defineLabel(ctx, "l2", undefinedPc);
  codeLblPo l3 = defineLabel(ctx, "l3", undefinedPc);
  codeLblPo l4 = defineLabel(ctx, "l4", undefinedPc);

  beq(l0);
  bnv(l0);
  bhi(l1);
  b(l1);
  setLabel(ctx, l1);
  b(l1);
  setLabel(ctx, l2);
  bl(l2);
  bl(l3);
  blr(X12);
  br(X13);
  setLabel(ctx, l3);
  brk(1234);

  cbnz(X3, l3);
  cbz(X4, l3);

  tbnz(X3, 56, l4);
  tbz(X4, 23, l4);
  setLabel(ctx, l4);
  tbnz(X3, 23, l4);
  tbz(X4, 56, l4);

  uint8 tgt[] = {0x00, 0x00, 0x00, 0x54, // b.eq 1b
                 0xef, 0xff, 0xff, 0x54,   // b.nv 1b
                 0x48, 0x00, 0x00, 0x54, // b.hi 2f
                 0x01, 0x00, 0x00, 0x14, // b 2f
                 0x00, 0x00, 0x00, 0x14, // b 2b
                 0x00, 0x00, 0x00, 0x94, // bl 1b
                 0x03, 0x00, 0x00, 0x94, // bl 2f
                 0x80, 0x01, 0x3f, 0xd6, // blr x12
                 0xa0, 0x01, 0x1f, 0xd6, // br x13
                 0x40, 0x9a, 0x20, 0xd4, // brk #1234

                 0xe3, 0xff, 0xff, 0xb5, // cbnz x3, l3
                 0xc4, 0xff, 0xff, 0xb4, // cbz x4, l3

                 0x43, 0x00, 0xc0, 0xb7, // tbnz
                 0x24, 0x00, 0xb8, 0x36,
                 0x03, 0x00, 0xb8, 0x37,
                 0xe4, 0xff, 0xc7, 0xb6,

  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_bit() {
  assemCtxPo ctx = createCtx();

  bfc(X12, 23, 5);
  bfi(X12, X10, 23, 5);
  bfxil(X12, X10, 23, 5);
  bic(X5, X6, RG(7));
  bic(X8, X9, LS(X10, 4));
  bic(X8, X9, RS(X10, 4));
  bic(X8, X9, AS(X10, 4));
  bic(X8, X9, RR(X10, 4));
  bics(X5, X6, RG(7));
  bics(X8, X9, LS(X10, 4));
  bics(X8, X9, RS(X10, 4));
  bics(X8, X9, AS(X10, 4));
  bics(X8, X9, RR(X10, 4));

  uint8 tgt[] = {0xec, 0x13, 0x69, 0xb3,// bfc x12, 23, 5
                 0x4c, 0x11, 0x69, 0xb3, // bfi x12, x10, 23, 5
                 0x4c, 0x6d, 0x57, 0xb3, // bfxil x12, x10, 23,5
                 0xc5, 0x00, 0x27, 0x8a, // bic x5, x6, x7
                 0x28, 0x11, 0x2a, 0x8a, // bic x8, x9, x10 lsl #4
                 0x28, 0x11, 0x6a, 0x8a, // bic x8, x9, x10 lsr #4
                 0x28, 0x11, 0xaa, 0x8a, // bic x8, x9, x10 asr #4
                 0x28, 0x11, 0xea, 0x8a, // bic x8, x9, x10 ror #4
                 0xc5, 0x00, 0x27, 0xea, // bics x5, x6, x7
                 0x28, 0x11, 0x2a, 0xea, // bics x8, x9, x10 lsl #4
                 0x28, 0x11, 0x6a, 0xea, // bics x8, x9, x10 lsr #4
                 0x28, 0x11, 0xaa, 0xea, // bics x8, x9, x10 asr #4
                 0x28, 0x11, 0xea, 0xea // bics x8, x9, x10 ror #4
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_cas() {
  assemCtxPo ctx = createCtx();

  casb(X6, X8, X3);
  casab(X10, X3, X2);
  casab(X10, X3, SP);
  casalb(X10, X3, X5);
  caslb(X10, X3, X0);

  cash(X6, X8, X3);
  casah(X10, X3, X2);
  casah(X10, X3, SP);
  casalh(X10, X3, X5);
  caslh(X10, X3, X0);

  casp(X6, X8, X3);
  caspa(X10, X2, X2);
  caspa(X10, X2, SP);
  caspal(X10, X2, X5);
  caspl(X10, X2, X0);

  cas(X6, X8, X3);
  casa(X10, X3, X2);
  casa(X10, X3, SP);
  casal(X10, X3, X5);
  casl(X10, X3, X0);

  uint8 tgt[] = {
    0x68, 0x7c, 0xa6, 0x08, // casb w6, w8, [x3]
    0x43, 0x7c, 0xea, 0x08, // casab w10, w3, [x2]
    0xe3, 0x7f, 0xea, 0x08, // casab w10, w3, [sp]
    0xa3, 0xfc, 0xea, 0x08, // casalb w10, w3 [x5]
    0x03, 0xfc, 0xaa, 0x08, // calb w10, w3, [x0]

    0x68, 0x7c, 0xa6, 0x48, // cash w6, w8, [x3]
    0x43, 0x7c, 0xea, 0x48, // casah w10, w3, [x2]
    0xe3, 0x7f, 0xea, 0x48, // casah w10, w3, [sp]
    0xa3, 0xfc, 0xea, 0x48, // casalh w10, w3 [x5]
    0x03, 0xfc, 0xaa, 0x48, // calh w10, w3, [x0]]

    0x68, 0x7c, 0x26, 0x48, // casp x6/7, x8/9, [x3]
    0x42, 0x7c, 0x6a, 0x48, // caspa x10/11, x2/3, [x2]
    0xe2, 0x7f, 0x6a, 0x48, // caspa x10/11, x2/3, [sp]
    0xa2, 0xfc, 0x6a, 0x48, // csspal x10/11, x2/3, [x5]
    0x02, 0xfc, 0x2a, 0x48, // caspl x10/11, x2/3, [x0]

    0x68, 0x7c, 0xa6, 0xc8, // cas x6, x8, [x3]
    0x43, 0x7c, 0xea, 0xc8, // casa x10, x3, [x2]
    0xe3, 0x7f, 0xea, 0xc8, // casa x10, x3, [sp]
    0xa3, 0xfc, 0xea, 0xc8, // casal x10, x3, [x5]
    0x03, 0xfc, 0xaa, 0xc8, // casl x10, x3, [x0]
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_cbt() {
  assemCtxPo ctx = createCtx();

  cls(X11, X9);
  clz(X9, X7);

  cmn(X21, EX(X9, U_XTX, 3));
  cmn(X20, IM(0xff));
  cmn(X20, IM(0xff0000));
  cmn(X19, LS(X5, 5));
  cmn(X19, AS(X5, 5));

  cmp(X21, EX(X9, U_XTX, 3));
  cmp(X20, IM(0xff));
  cmp(X20, IM(0xff0000));
  cmp(X19, LS(X5, 5));
  cmp(X19, AS(X5, 5));

  cinc(X28, X20, EQ);
  cinv(X28, X20, CC);
  cneg(X28, X20, HI);

  csel(X17, X13, X20, LE);
  cset(X16, LE);
  csetm(X16, LE);

  csinc(X14, X13, X11, GT);
  csinv(X14, X13, X11, GT);
  csneg(X14, X13, X11, GT);

  uint8 tgt[] = {0x2b, 0x15, 0xc0, 0xda, // cls
                 0xe9, 0x10, 0xc0, 0xda, // cls

                 0xbf, 0x6e, 0x29, 0xab, // cmn
                 0x9f, 0xfe, 0x03, 0xb1,
                 0x9f, 0xc2, 0x7f, 0xb1,
                 0x7f, 0x16, 0x05, 0xab,
                 0x7f, 0x16, 0x85, 0xab,

                 0xbf, 0x6e, 0x29, 0xeb,  // cmp
                 0x9f, 0xfe, 0x03, 0xf1,
                 0x9f, 0xc2, 0x7f, 0xf1,
                 0x7f, 0x16, 0x05, 0xeb,
                 0x7f, 0x16, 0x85, 0xeb,

                 0x9c, 0x16, 0x94, 0x9a, // cinc
                 0x9c, 0x22, 0x94, 0xda, // cinv
                 0x9c, 0x96, 0x94, 0xda, // cneg
                 0xb1, 0xd1, 0x94, 0x9a, // csel
                 0xf0, 0xc7, 0x9f, 0x9a, // cset
                 0xf0, 0xc3, 0x9f, 0xda, // csetm

                 0xae, 0xc5, 0x8b, 0x9a, // csinc
                 0xae, 0xc1, 0x8b, 0xda, // csinv
                 0xae, 0xc5, 0x8b, 0xda, // csneg
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_ld() {
  assemCtxPo ctx = createCtx();

  ldaddab(X3, X6, X4);
  ldaddalb(X3, X6, X4);
  ldaddb(X3, X6, X4);
  ldaddlb(X3, X6, X4);

  ldadd(X3, X6, X4);
  ldadda(X3, X6, X4);
  ldaddal(X3, X6, X4);
  ldaddl(X3, X6, X4);

  ldp(X10, X11, PSX(X3, 8));
  ldp(X10, X8, PRX(X5, -8));
  ldp(X15, X20, OF(X20, 16));

  ldpsw(X10, X11, PSX(X3, 8));
  ldpsw(X10, X8, PRX(X5, -8));
  ldpsw(X15, X20, OF(X20, 16));

  ldr(X10, PSX(X3, 8));
  ldr(X10, PRX(X5, -8));
  ldr(X15, OF(X20, 16));

  codeLblPo l0 = defineLabel(ctx, "l0", undefinedPc);
  ldr(X19, PC(l0));
  setLabel(ctx, l0);
  ldr(X21, EX2(X22, X7, S_XTX, 3));

  ldrb(X19, PSX(X3, 8));
  ldrb(X10, PRX(X5, -8));
  ldrb(X15, OF(X20, 16));
  ldrb(X21, EX2(X22, X7, S_XTX, 0));

  ldrh(X19, PSX(X3, 8));
  ldrh(X10, PRX(X5, -8));
  ldrh(X15, OF(X20, 16));
  ldrh(X21, EX2(X22, X7, S_XTX, 0));

  ldrsb(X19, PSX(X3, 8));
  ldrsb(X10, PRX(X5, -8));
  ldrsb(X15, OF(X20, 16));
  ldrsb(X21, EX2(X22, X7, S_XTX, 0));

  ldrsh(X19, PSX(X3, 8));
  ldrsh(X10, PRX(X5, -8));
  ldrsh(X15, OF(X20, 16));
  ldrsh(X21, EX2(X22, X7, S_XTX, 0));

  ldrsw(X10, PSX(X3, 8));
  ldrsw(X10, PRX(X5, -8));
  ldrsw(X15, OF(X20, 16));
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  ldrsw(X19, PC(l1));
  ldrsw(X21, EX2(X22, X7, S_XTX, 3));
  setLabel(ctx, l1);

  ldur(X29, X3, -8);
  ldurb(X29, X3, -8);
  ldurh(X29, X3, -8);
  ldursb(X29, X3, -8);
  ldursh(X29, X3, -8);
  ldursw(X29, X3, -8);

  ldxp(X12, X16, X2);
  ldxr(X13, X9);
  ldxrb(X13, X9);
  ldxrh(X13, X9);

  ldtr(X0, X1, -8);
  ldr(X15, OF(X20, -16));

  uint8 tgt[] = {0x86, 0x00, 0xa3, 0x38,// ldaddab
                 0x86, 0x00, 0xe3, 0x38,// ldaddalb
                 0x86, 00, 0x23, 0x38, // ldaddb
                 0x86, 0x00, 0x63, 0x38, // ldaddlb

                 0x86, 0x00, 0x23, 0xf8,  // ldadd
                 0x86, 0x00, 0xa3, 0xf8, // ldadda
                 0x86, 0x00, 0xe3, 0xf8, // ldaddal
                 0x86, 0x00, 0x63, 0xf8, // ldaddl

                 0x6a, 0xac, 0xc0, 0xa8, // ldp postx
                 0xaa, 0xa0, 0xff, 0xa9, // ldp prex
                 0x8f, 0x52, 0x41, 0xa9, // ldp off

                 0x6a, 0x2c, 0xc1, 0x68, // ldpsw
                 0xaa, 0x20, 0xff, 0x69,
                 0x8f, 0x52, 0x42, 0x69,

                 0x6a, 0x84, 0x40, 0xf8, // ldr px
                 0xaa, 0x8c, 0x5f, 0xf8, // ldr prx
                 0x8f, 0x0a, 0x40, 0xf9, // ldr uo
                 0x33, 0x00, 0x00, 0x58, // ldr pc+off

                 0xd5, 0xfa, 0x67, 0xf8, // ldr x21, [x22, x7, sxtx #3]

                 0x73, 0x84, 0x40, 0x38, // ldrb
                 0xaa, 0x8c, 0x5f, 0x38,
                 0x8f, 0x42, 0x40, 0x39,
                 0xd5, 0xea, 0x67, 0x38,

                 0x73, 0x84, 0x40, 0x78, // ldrh
                 0xaa, 0x8c, 0x5f, 0x78,
                 0x8f, 0x22, 0x40, 0x79,
                 0xd5, 0xea, 0x67, 0x78,

                 0x73, 0x84, 0xc0, 0x38, // ldrsb
                 0xaa, 0x8c, 0xdf, 0x38,
                 0x8f, 0x42, 0xc0, 0x39,
                 0xd5, 0xea, 0xe7, 0x38,

                 0x73, 0x84, 0xc0, 0x78, // ldrsh
                 0xaa, 0x8c, 0xdf, 0x78,
                 0x8f, 0x22, 0xc0, 0x79,
                 0xd5, 0xea, 0xe7, 0x78,

                 0x6a, 0x84, 0x80, 0xb8, // ldrsw
                 0xaa, 0x8c, 0x9f, 0xb8,
                 0x8f, 0x12, 0x80, 0xb9,
                 0x53, 0x00, 0x00, 0x98,
                 0xd5, 0xfa, 0xa7, 0xb8,

                 0x7d, 0x80, 0x5f, 0xf8, // ldur
                 0x7d, 0x80, 0x5f, 0x38,
                 0x7d, 0x80, 0x5f, 0x78,
                 0x7d, 0x80, 0x9f, 0x38,
                 0x7d, 0x80, 0x9f, 0x78,
                 0x7d, 0x80, 0x9f, 0xb8,

                 0x4c, 0x40, 0x7f, 0xc8, // ldxp
                 0x2d, 0x7d, 0x5f, 0xc8,
                 0x2d, 0x7d, 0x5f, 0x08,
                 0x2d, 0x7d, 0x5f, 0x48,
                 0x20, 0x88, 0x5f, 0xf8, //ldtr
                 0x8f, 0x0a, 0x5f, 0xf8,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_lsl() {
  assemCtxPo ctx = createCtx();

  lsl(X12, X13, RG(X20));
  lsl(X12, X9, IM(23));

  lsr(X12, X13, RG(X20));
  lsr(X12, X9, IM(23));

  uint8 tgt[] = {0xac, 0x21, 0xd4, 0x9a, // lsl
                 0x2c, 0xa1, 0x69, 0xd3,
                 0xac, 0x25, 0xd4, 0x9a, // lsr
                 0x2c, 0xfd, 0x57, 0xd3,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_mt() {
  assemCtxPo ctx = createCtx();

  madd(X21, X29, X30, X1);
  mov(X21, RG(SP));
  mov(X21, IM(0xff230000));

  mov(X22, IM(0x1111222233334444));

  msub(X21, X29, X30, X1);
  mul(X1, X2, X3);
  mvn(X1, X10, ROR, 4);

  neg(X1, X10, ASR, 4);
  neg(X1, X10, LSR, 4);

  negs(X1, X10, ASR, 4);
  negs(X1, X10, LSR, 4);

  ngc(X1, X12);
  ngcs(X1, X12);

  nop(ctx);

  orn(X26, X24, X1, ROR, 20);

  orr(X10, X9, IM(0xff00));
  orr(X11, X12, RG(X11));
  orr(X11, X12, RR(X11, 8));

  rbit(X1, X2);
  ret(X12);

  rev(X3, X4);
  rev16(X5, X6);
  rev32(X7, X8);

  ror(X9, X10, RG(X11));
  ror(X12, X13, IM(4));

  sbc(X15, X16, X17);
  sbcs(X15, X16, X17);
  sdiv(X18, X19, X20);
  udiv(X21, X22, X23);

  smaddl(X1, X2, X3, X4);

  smulh(X5, X6, X7);
  smull(X5, X6, X8);

  sub(X3, X4, RG(X6));
  sub(X3, X5, IM(135));
  sub(X3, X5, IM(0x87000));
  sub(X2, X4, LS(X6, 3));
  sub(X1, X12, RS(X18, 3));
  sub(X1, X12, AS(X18, 3));

  subs(X3, X4, RG(X6));
  subs(X3, X5, IM(135));
  subs(X3, X5, IM(0x87000));
  subs(X2, X4, LS(X6, 3));
  subs(X1, X12, RS(X18, 3));
  subs(X1, X12, AS(X18, 3));

  umaddl(X1, X2, X3, X4);
  umnegl(X5, X6, X7);
  umsubl(X8, X9, X10, X11);
  umulh(X12, X13, X14);
  umull(X15, X16, X17);

  mov(X0, IM(0x55));

  uint8 tgt[] = {0xb5, 0x07, 0x1e, 0x9b,  // madd
                 0xf5, 0x03, 0x00, 0x91, // mov
                 0x75, 0xe4, 0xbf, 0xd2,
		 0x96, 0x88, 0x88, 0xD2,
		 0x76, 0x66, 0xA6, 0xF2,
		 0x56, 0x44, 0xC4, 0xF2,
		 0x36, 0x22, 0xE2, 0xF2,
                 0xb5, 0x87, 0x1e, 0x9b,  // msub
                 0x41, 0x7c, 0x03, 0x9b,
                 0xe1, 0x13, 0xea, 0xaa,
                 0xe1, 0x13, 0x8a, 0xcb,
                 0xe1, 0x13, 0x4a, 0xcb,
                 0xe1, 0x13, 0x8a, 0xeb,
                 0xe1, 0x13, 0x4a, 0xeb,

                 0xe1, 0x03, 0x0c, 0xda,
                 0xe1, 0x03, 0x0c, 0xfa,
                 0x1f, 0x20, 0x03, 0xd5,
                 0x1a, 0x53, 0xe1, 0xaa,

                 0x2a, 0x1d, 0x78, 0xb2, // orr
                 0x8b, 0x01, 0x0b, 0xaa,
                 0x8b, 0x21, 0xcb, 0xaa,
                 0x41, 0x00, 0xc0, 0xda, // rbit
                 0x80, 0x01, 0x5f, 0xd6, // ret

                 0x83, 0x0c, 0xc0, 0xda, // rev
                 0xc5, 0x04, 0xc0, 0xda,
                 0x07, 0x09, 0xc0, 0xda,

                 0x49, 0x2d, 0xcb, 0x9a, // ror
                 0xac, 0x11, 0xcd, 0x93,

                 0x0f, 0x02, 0x11, 0xda, // sbc
                 0x0f, 0x02, 0x11, 0xfa,

                 0x72, 0x0e, 0xd4, 0x9a,
                 0xd5, 0x0a, 0xd7, 0x9a,

                 0x41, 0x10, 0x23, 0x9b, // smaddl

                 0xc5, 0x7c, 0x47, 0x9b,
                 0xc5, 0x7c, 0x28, 0x9b,

                 0x83, 0x00, 0x06, 0xcb, // sub
                 0xa3, 0x1c, 0x02, 0xd1,
                 0xa3, 0x1c, 0x42, 0xd1,
                 0x82, 0x0c, 0x06, 0xcb,
                 0x81, 0x0d, 0x52, 0xcb,
                 0x81, 0x0d, 0x92, 0xcb,
                 0x83, 0x00, 0x06, 0xeb,
                 0xa3, 0x1c, 0x02, 0xf1,
                 0xa3, 0x1c, 0x42, 0xf1,
                 0x82, 0x0c, 0x06, 0xeb,
                 0x81, 0x0d, 0x52, 0xeb,
                 0x81, 0x0d, 0x92, 0xeb,

                 0x41, 0x10, 0xa3, 0x9b, // umull etc
                 0xc5, 0xfc, 0xa7, 0x9b,
                 0x28, 0xad, 0xaa, 0x9b,
                 0xac, 0x7d, 0xce, 0x9b,
                 0x0f, 0x7e, 0xb1, 0x9b,
                 0xA0, 0x0A, 0x80, 0xD2
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_st() {
  assemCtxPo ctx = createCtx();

  stp(X1, X2, PSX(X3, 32));
  stp(X1, X2, PRX(X3, 32));
  stp(X1, X2, OF(X3, 32));

  str(X1, PSX(X3, 32));
  str(X1, PRX(X3, 32));
  str(X1, OF(X3, 32));
  str(X2, EX2(X10, X7, S_XTX, 3));

  strb(X1, PSX(X3, 32));
  strb(X1, PRX(X3, 32));
  strb(X1, OF(X3, 32));
  strb(X2, EX2(X10, X7, S_XTX, 0));

  strh(X1, PSX(X3, 32));
  strh(X1, PRX(X3, 32));
  strh(X1, OF(X3, 32));
  strh(X2, EX2(X10, X7, S_XTX, 0));

  stur(X29, X3, -8);
  sturb(X29, X3, -8);
  sturh(X29, X3, -8);

  uint8 tgt[] = {0x61, 0x08, 0x82, 0xa8, // stp
                 0x61, 0x08, 0x82, 0xa9,
                 0x61, 0x08, 0x02, 0xa9,

                 0x61, 0x04, 0x02, 0xf8,
                 0x61, 0x0c, 0x02, 0xf8,
                 0x61, 0x10, 0x00, 0xf9,
                 0x42, 0xf9, 0x27, 0xf8,

                 0x61, 0x04, 0x02, 0x38,
                 0x61, 0x0c, 0x02, 0x38,
                 0x61, 0x80, 0x00, 0x39,
                 0x42, 0xe9, 0x27, 0x38,

                 0x61, 0x04, 0x02, 0x78,
                 0x61, 0x0c, 0x02, 0x78,
                 0x61, 0x40, 0x00, 0x79,
                 0x42, 0xe9, 0x27, 0x78,

                 0x7d, 0x80, 0x1f, 0xf8, // stur
                 0x7d, 0x80, 0x1f, 0x38,
                 0x7d, 0x80, 0x1f, 0x78,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

typedef int64 (*un_i64)(int64 x);
typedef int64 (*bin_i64)(int64 x, int64 y);

retCode test_addFun() {
  assemCtxPo ctx = createCtx();

  preamble(ctx, 0);
  add(X0, X0, RG(X1));
  postamble(ctx);

  bin_i64 fn = (bin_i64) createCode(ctx);
  discardCtx(ctx);

  int64 reslt = fn(3, 5);
  return checkReslt(reslt, 8, "addFn");
}

retCode test_factFun() {
  assemCtxPo ctx = createCtx();
  int lclCount = 1;

  codeLblPo fct = preamble(ctx, lclCount * 8);
  codeLblPo l0 = defineLabel(ctx, "nonZero", undefinedPc);
  codeLblPo lx = defineLabel(ctx, "exit", undefinedPc);
  sttr(X0, X29, -8);
  cmp(X0, IM(1));
  bne(l0);
  mov(X0, IM(1));
  b(lx);
  setLabel(ctx, l0);
  sub(X0, X0, IM(1));  // f(x-1)
  bl(fct);
  ldtr(X1, X29, -8);
  mul(X0, X0, X1);
  setLabel(ctx, lx);
  postamble(ctx);

  un_i64 fn = (un_i64) createCode(ctx);
  discardCtx(ctx);

  tryRet(checkReslt(fn(3), 6, "fact(3)"));
  tryRet(checkReslt(fn(10), 3628800, "fact(10)"));
  return Ok;
}

retCode all_tests() {
  tests_run = 0;

  tryRet(run_test(test_adc));
  tryRet(run_test(test_and));
  tryRet(run_test(test_adr));
  tryRet(run_test(test_b));
  tryRet(run_test(test_bit));
  tryRet(run_test(test_cas));
  tryRet(run_test(test_cbt));
  tryRet(run_test(test_ld));
  tryRet(run_test(test_lsl));
  tryRet(run_test(test_mt));
  tryRet(run_test(test_st));

  tryRet(run_test(test_addFun));
  tryRet(run_test(test_factFun));
  tryRet(regset_tests());

  return Ok;
}
