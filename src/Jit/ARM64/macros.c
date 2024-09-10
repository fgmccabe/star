//
// Created by Francis McCabe on 9/8/24.
//

#include "macros.h"

registerMap defltAvailRegSet() {
  return callerSaved() | calleeSaved() | stackRegs();
}

registerMap emptyRegSet() {
  return 0;
}

registerMap allocReg(registerMap from, armReg Rg) {
  check((from & (1u << Rg)) != 0, "register not free");
  return (from & (~(1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  check((from & (1u << Rg)) != 0, "register already free");
  return (from | (1u << Rg));
}

registerMap addReg(registerMap from, armReg Rg) {
  return (from | (1u << Rg));
}

armReg nxtAvailReg(registerMap from) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = 1u << ix;
    if ((from & mask) != 0)
      return ix;
  }
  return XZR;
}

void processRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = 1u << ix;
    if ((set & mask) != 0)
      proc((armReg) ix, cl);
  }
}

void revProcessRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 64u; ix > 0;) {
    ix--;
    uint64 mask = 1u << ix;
    if ((set & mask) != 0)
      proc((armReg) ix, cl);
  }
}

static void svRegisters(assemCtxPo ctx, registerMap regs, armReg Rg) {
  armReg nxt = nxtAvailReg(regs);

  if (nxt == XZR) {
    if (Rg != XZR)
      stp(Rg, XZR, PRX(SP, -16));
  } else if (Rg == XZR)
    svRegisters(ctx, allocReg(regs, nxt), nxt);
  else {
    stp(nxt, Rg, PRX(SP, -16));
    svRegisters(ctx, allocReg(regs, nxt), XZR);
  }
}

void saveRegisters(assemCtxPo ctx, registerMap regs) {
  svRegisters(ctx,reg,XZR);
}

static void restRegisters(assemCtxPo ctx, registerMap regs, armReg Rg) {
  armReg nxt = nxtAvailReg(regs);

  if (nxt == XZR) {
    if (Rg != XZR)
      ldp(Rg, XZR, PSX(SP, 16));
  } else if (Rg == XZR)
    restRegisters(ctx, allocReg(regs, nxt), nxt);
  else {
    restRegisters(ctx, allocReg(regs, nxt), XZR);
    ldp(nxt, Rg, PSX(SP, 16));
  }
}

void restoreRegisters(assemCtxPo ctx, registerMap regs) {
  restRegisters(ctx,regs,XZR);
}
