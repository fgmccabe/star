//
// Created by Francis McCabe on 12/12/22.
//
#include "unitTests.h"
#include "arm64P.h"
#include "jitP.h"

static retCode checkCode(uint8 *src, integer srcLen, assemCtxPo ctx);

static retCode test_adc() {
  assemCtxPo ctx = createCtx();

  adc_(1, X12, X13, X14, ctx);
  adc_(1, X1, X2, X3, ctx);
  adcs_(1, X12, X13, X14, ctx);
  adcs_(1, X1, X2, X3, ctx);


//  adc(RG(R10), IM(0x11223344), ctx);
//  adc(RG(RCX), IM(0x55667788), ctx);
//
//  adc(RG(R10), BS(RAX, 0x11223344), ctx);
//  adc(BS(RAX, 0x11223344), RG(R10), ctx);
//  adc(BS(RCX, 0x11223344), IM(0x55667788), ctx);
//  adc(BS(R10, 0x11223344), IM(0x55667788), ctx);
//  adc(BS(R9, 0x11223344), IM(0x55667788), ctx);
//  adc(BS(RAX, 0x11223344), IM(0x55667788), ctx);
//
//  adc(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
//  adc(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
//  adc(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  uint8 tgt[] = {0xac, 0x01, 0x0e, 0x9a, // adc x12, x13, x14
                 0x41, 0x00, 0x03, 0x9a, // adc x1, x2, x3
                  0xac, 0x01, 0x0e, 0xba, // adcs x12, x13, x14
                 0x41, 0x00, 0x03, 0xba, // adcs x1, x2, x3
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

//  tryRet(run_test(test_addFun));
//  tryRet(run_test(test_factFun));

  return Ok;
}
