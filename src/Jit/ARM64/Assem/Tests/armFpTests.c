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
  fmov(FP(F22),FP(F14));

  uint8 tgt[] = {0xec, 0x02, 0x66, 0x9e, // fmov x12,d23
                 0xd6, 0x01, 0x67, 0x9e, // fmov d22, x14
                 0xd6, 0x41, 0x60, 0x1e, // fmov d22, d14
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

retCode fp_tests() {
  tryRet(run_test(test_fmov));

  return Ok;
}
