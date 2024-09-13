//
// Created by Francis McCabe on 09/10/2024
//


#include "armRegSetTest.h"
#include "test_infra.h"

static registerMap mapFromArray(armReg regs[], int count) {
  registerMap set = emptyRegSet();

  for (int ix = 0; ix < count; ix++)
    set = addReg(set, regs[ix]);

  return set;
}

retCode test_sv1() {
  assemCtxPo ctx = createCtx();

  armReg someRegs[] = {X0, X3, X4, X5, X10, X20, X17};

  registerMap map = mapFromArray(someRegs, NumberOf(someRegs));

  saveRegisters(ctx, map);

  uint8 tgt[] = {0xE0, 0x0F, 0xBF, 0xA9, 0xE4, 0x17, 0xBF, 0xA9, 0xEA, 0x47, 0xBF, 0xA9, 0xF4, 0x7f, 0xBF, 0xA9};

  return checkCode(tgt, NumberOf(tgt), ctx);
}

retCode test_rs1() {
  assemCtxPo ctx = createCtx();

  armReg someRegs[] = {X0, X3, X4, X5, X10, X20, X17};

  registerMap map = mapFromArray(someRegs, NumberOf(someRegs));

  restoreRegisters(ctx, map);

  uint8 tgt[] = {0xf4, 0x7f, 0xc1, 0xa8, 0xea, 0x47, 0xc1, 0xa8, 0xe4, 0x17, 0xc1, 0xa8, 0xe0, 0xf, 0xc1, 0xa8};

  return checkCode(tgt, NumberOf(tgt), ctx);
}

typedef uint64 (*zr_i64)();

retCode test_svrs1() {
  assemCtxPo ctx = createCtx();

  armReg someRegs[] = {X0, X3, X4, X5, X10, X8, X9};
  registerMap map = mapFromArray(someRegs, NumberOf(someRegs));

  preamble(ctx, 0);

  for (int ix = 0; ix < X11; ix++)
    mov(ix, IM(0x55));

  for (int ix = 0; ix < NumberOf(someRegs); ix++)
    mov(someRegs[ix], IM(1 << ix));

  saveRegisters(ctx, map);
  for (int ix = 0; ix < X11; ix++)
    mov(ix, IM(0x33));

  restoreRegisters(ctx, map);

  mov(X21, IM(0));

  for (int ix = 0; ix < NumberOf(someRegs); ix++)
    orr(X21, X21, RG(ix));

  mov(X0, RG(X21));

  postamble(ctx);

  zr_i64 fn = (zr_i64) createCode(ctx);
  discardCtx(ctx);

  uint64 reslt = fn();
  return checkReslt((int64)reslt, 0x3f, "test_svrs1");
}

retCode regset_tests() {
  tryRet(run_test(test_sv1));
  tryRet(run_test(test_rs1));
  tryRet(run_test(test_svrs1));

  return Ok;
}
  

