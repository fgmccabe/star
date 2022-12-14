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

static retCode test_and() {
  assemCtxPo ctx = createCtx();

  and(X12, X13, IM(0xff00), ctx);

  uint8 tgt[] = {0xac, 0x1d, 0x78, 0x92, // and x12, x13, #0xff00
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
//  tryRet(run_test(test_and));

//  tryRet(run_test(test_addFun));
//  tryRet(run_test(test_factFun));

  return Ok;
}
