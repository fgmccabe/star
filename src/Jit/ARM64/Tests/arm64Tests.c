//
// Created by Francis McCabe on 12/12/22.
//
#include "unitTests.h"
#include "arm64P.h"
#include "jitP.h"

static retCode checkCode(uint8 *src, integer srcLen, assemCtxPo ctx);

static retCode test_adc() {
  assemCtxPo ctx = createCtx();

  adc(X12, X13, X14, ctx);
  adc(X1, X2, X3, ctx);
  adcs(X12, X13, X14, ctx);
  adcs(X1, X2, X3, ctx);

  add(X3, X4, RG(X6), ctx);
  add(X3, X5, IM(135), ctx);
  add(X3, X5, IMH(135), ctx);
  add(X2, X4, LS(X6, 3), ctx);
  add(X1, X12, RS(X18, 3), ctx);
  add(X1, X12, AS(X18, 3), ctx);

  adds(X3, X4, RG(X6), ctx);
  adds(X3, X5, IM(135), ctx);
  adds(X3, X5, IMH(135), ctx);
  adds(X2, X4, LS(X6, 3), ctx);
  adds(X1, X12, RS(X18, 3), ctx);
  adds(X1, X12, AS(X18, 3), ctx);

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

  adr(X10, l0, ctx);
  adr(X12, l1, ctx);

  setLabel(ctx, l1);

  uint8 tgt[] = {0x0a, 0x00, 0x00, 0x10, // adr x10, adc
                 0x2c, 0x00, 0x00, 0x10, // adr 12, 1f
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_and() {
  assemCtxPo ctx = createCtx();

  and(X12, X13, IM(0x3333333333333333), ctx);
  and(X12, X13, IM(0xff00), ctx);
  and(X1, X2, RG(X3), ctx);
  and(X3, X5, LS(X6, 3), ctx);
  and(X3, X5, RS(X6, 3), ctx);
  and(X3, X5, RR(X6, 3), ctx);

  ands(X12, X13, IM(0x3333333333333333), ctx);
  ands(X12, X13, IM(0xff00), ctx);
  ands(X1, X2, RG(X3), ctx);
  ands(X3, X5, LS(X6, 3), ctx);
  ands(X3, X5, RS(X6, 3), ctx);
  ands(X3, X5, RR(X6, 3), ctx);

  asr(X12, X13, IM(0xf), ctx);
  asr(X1, X2, RG(X3), ctx);

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
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_b() {
  assemCtxPo ctx = createCtx();
  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  codeLblPo l2 = defineLabel(ctx, "l2", undefinedPc);
  codeLblPo l3 = defineLabel(ctx, "l3", undefinedPc);

  beq(l0, ctx);
  bnv(l0, ctx);
  bhi(l1, ctx);
  b(l1, ctx);
  setLabel(ctx, l1);
  b(l1, ctx);
  setLabel(ctx, l2);
  bl(l2,ctx);
  bl(l3, ctx);
  blr(X12, ctx);
  br(X13, ctx);
  setLabel(ctx, l3);
  brk(1234, ctx);

  uint8 tgt[] = {0x00, 0x00, 0x00, 0x54, // b.eq 1b
                 0xef, 0xff, 0xff, 0x54,   // b.nv 1b
                 0x48, 0x00, 0x00, 0x54, // b.hi 2f
                 0x01, 0x00, 0x00, 0x14, // b 2f
                 0x00, 0x00, 0x00, 0x14, // b 2b
                 0x00, 0x00, 0x00, 0x94, // bl 1b
                 0x03, 0x00, 0x00, 0x94, // bl 2f
                 0x80, 0x01, 0x3f, 0xd6, // blr x12
                 0xa0, 0x01, 0x1f, 0xd6, // br x13
                 0x40, 0x9a, 0x20, 0xd4 // brk #1234
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_bit() {
  assemCtxPo ctx = createCtx();

  bfc(X12, 23, 5, ctx);
  bfi(X12, X10, 23, 5, ctx);
  bfxil(X12, X10, 23, 5, ctx);
  bic(X5, X6, RG(7), ctx);
  bic(X8, X9, LS(X10, 4), ctx);
  bic(X8, X9, RS(X10, 4), ctx);
  bic(X8, X9, AS(X10, 4), ctx);
  bic(X8, X9, RR(X10, 4), ctx);
  bics(X5, X6, RG(7), ctx);
  bics(X8, X9, LS(X10, 4), ctx);
  bics(X8, X9, RS(X10, 4), ctx);
  bics(X8, X9, AS(X10, 4), ctx);
  bics(X8, X9, RR(X10, 4), ctx);

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

typedef int64 (*un_i64)(int64 x);
typedef int64 (*bin_i64)(int64 x, int64 y);
//
//retCode test_addFun() {
//  assemCtxPo ctx = createCtx();
//
//  preamble(ctx, 0);
//  mov(RG(RAX), RG(RDI), ctx);
//  add(RG(RAX), RG(RSI), ctx);
//  postamble(ctx);
//
//  bin_i64 fn = (bin_i64) createCode(ctx);
//  int64 reslt = fn(3, 5);
//  return checkReslt(reslt, 8, "addFn");
//}
//
//retCode test_factFun() {
//  assemCtxPo ctx = createCtx();
//
//  codeLblPo fct = preamble(ctx, 0);
//  codeLblPo l0 = defineLabel(ctx, "nonZero", -1);
//  codeLblPo lx = defineLabel(ctx, "exit", -1);
//  cmp(RG(RDI), IM(1), ctx);
//  jg(l0, ctx);
//  mov(RG(RAX), IM(1), ctx);
//  jmp(LB(lx), ctx);
//  setLabel(ctx, l0);
//  push(RG(RDI), ctx);
//  dec(RG(RDI), ctx);
//  call(LB(fct), ctx);
//  pop(RG(RDI), ctx);
//  imul(RAX, RG(RDI), ctx);
//  setLabel(ctx, lx);
//  postamble(ctx);
//
//  un_i64 fn = (un_i64) createCode(ctx);
//  tryRet(checkReslt(fn(3), 6, "fact(3)"));
//  tryRet(checkReslt(fn(10), 3628800, "fact(10)"));
//  return Ok;
//}

retCode checkCode(uint8 *src, integer srcLen, assemCtxPo ctx) {
  retCode ret;
  if (ctx->pc != srcLen) {
    logMsg(logFile, "%d bytes expected, %d bytes generated", srcLen, ctx->pc);
    logMsg(logFile, "actual bytes: %.*X", ctx->pc, ctx->bytes);
    ret = Error;
  } else
    ret = cmpBytes(src, ctx->bytes, srcLen);
  discardCtx(ctx);
  return ret;
}

retCode all_tests() {
  tests_run = 0;

  tryRet(run_test(test_adc));
  tryRet(run_test(test_and));
  tryRet(run_test(test_adr));
  tryRet(run_test(test_b));
  tryRet(run_test(test_bit));

//  tryRet(run_test(test_addFun));
//  tryRet(run_test(test_factFun));

  return Ok;
}
