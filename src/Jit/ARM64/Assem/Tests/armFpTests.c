//
// Created by Francis McCabe on 6/6/25.
//
#include "unitTests.h"
#include "Assem/Headers/arm64P.h"
#include "test_infra.h"
#include "armFpTests.h"

static retCode test_fmov() {
  assemCtxPo ctx = createCtx();

  fmov(RG(X12), FP(F23));
  fmov(FP(F22), RG(X14));
  fmov(FP(F22), FP(F14));

  uint8 tgt[] = {
    0xec, 0x02, 0x66, 0x9e, // fmov x12,d23
    0xd6, 0x01, 0x67, 0x9e, // fmov d22, x14
    0xd6, 0x41, 0x60, 0x1e, // fmov d22, d14
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_ldrfp() {
  assemCtxPo ctx = createCtx();

  fldr(F5, OF(X23,96));
  fldr(F5, OF(X23,16));
  fldr(F5, PSX(X23,16));
  fldr(F5, PRX(X21,16));

  uint8 tgt[] = {
    0xe5, 0x32, 0x40, 0xfd,
    0xe5, 0x0a, 0x40, 0xfd,
    0xe5, 0x06, 0x41, 0xfc,
    0xa5, 0x0e, 0x41, 0xfc,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_strfp() {
  assemCtxPo ctx = createCtx();

  fstr(F5, OF(X23,96));
  fstr(F5, OF(X23,16));
  fstr(F5, PSX(X23,16));
  fstr(F5, PRX(X21,16));

  uint8 tgt[] = {
    0xe5, 0x32, 0x0, 0xfd, // str d5,[x23, #96]
    0xe5, 0x0a, 0x00, 0xfd, // str d5,[X23, #16]
    0xe5, 0x06, 0x01, 0xfc, // str d5, [x23], #16
    0xa5, 0x0e, 0x01, 0xfc, // str d5, [x21, #16]!
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_ldpfp() {
  assemCtxPo ctx = createCtx();

  ldpf(F10, F11, PSX(X3, 8));
  ldpf(F10, F8, PRX(X5, -8));
  ldpf(F15, F20, OF(X20, 16));
  ldpf(F0, F1, PSX(SP,16));

  uint8 tgt[] = {
    0x6a, 0xac, 0xc0, 0x6c, // ldp D10,D11, [X3], #8
    0xaa, 0xa0, 0xff, 0x6d, // ldp D10, d8, [X5, #8]!
    0x8f, 0x52, 0x41, 0x6d, // ldp D15, D20, [X20, #16]
    0xe0, 0x07, 0xc1, 0x6c, // ldp d0,d1,[sp],#16
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_stpfp() {
  assemCtxPo ctx = createCtx();

  stpf(F10, F11, PSX(X3, 8));
  stpf(F10, F8, PRX(X5, -8));
  stpf(F0, F0, PRX(SP, -16));
  stpf(F15, F20, OF(X20, 16));

  uint8 tgt[] = {
    0x6a, 0xac, 0x80, 0x6c, // stp D10,D11, [X3], #8
    0xaa, 0xa0, 0xbf, 0x6d, // stp D10, d8, [X5, #8]!
    0xe0, 0x03, 0xbf, 0x6d, // stp D0, d0, [sp, #-16]
    0x8f, 0x52, 0x01, 0x6d, // stp D15, D20, [X20, #16]
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_fops() {
  assemCtxPo ctx = createCtx();

  fadd(F0, F2, F4);
  fsub(F4, F2, F12);
  fmul(F3, F6, F8);
  fdiv(F5, F8, F17);

  uint8 tgt[] = {
    0x40, 0x28, 0x64, 0x1e, 0x44, 0x38, 0x6c, 0x1e,
    0xc3, 0x08, 0x68, 0x1e, 0x05, 0x19, 0x71, 0x1e,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

retCode fp_tests() {
  tryRet(run_test(test_fmov));
  tryRet(run_test(test_ldrfp));
  tryRet(run_test(test_strfp));
  tryRet(run_test(test_ldpfp));
  tryRet(run_test(test_stpfp));

  tryRet(run_test(test_fops));

  return Ok;
}
