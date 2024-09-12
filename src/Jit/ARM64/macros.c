//
// Created by Francis McCabe on 9/8/24.
//

#include "macros.h"
#include "code.h"

registerMap defltAvailRegSet() {
  return callerSaved() | calleeSaved() | stackRegs();
}

registerMap emptyRegSet() {
  return 0;
}

registerMap nonSpillSet(integer arity) {
  registerMap set = emptyRegSet();
  switch (arity) {
    default:
    case 9:
      set = addReg(set, X8);
    case 8:
      set = addReg(set, X7);
    case 7:
      set = addReg(set, X6);
    case 6:
      set = addReg(set, X5);
    case 5:
      set = addReg(set, X4);
    case 4:
      set = addReg(set, X3);
    case 3:
      set = addReg(set, X2);
    case 2:
      set = addReg(set, X1);
    case 1:
      set = addReg(set, X0);
    case 0:
      return set;
  }
}

registerMap allocReg(registerMap from, armReg Rg) {
  check((from & (1u << Rg)) != 0, "register not free");
  return (from & (~(1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  check((from & (1u << Rg)) != 0, "register already free");
  return (from | (1u << Rg));
}

registerMap dropReg(registerMap map, armReg Rg) {
  return (map & (~(1u << Rg)));
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
    svRegisters(ctx, dropReg(regs, nxt), nxt);
  else {
    stp(Rg, nxt, PRX(SP, -16));
    svRegisters(ctx, dropReg(regs, nxt), XZR);
  }
}

void saveRegisters(assemCtxPo ctx, registerMap regs) {
  svRegisters(ctx, regs, XZR);
}

static void restRegisters(assemCtxPo ctx, registerMap regs, armReg Rg) {
  armReg nxt = nxtAvailReg(regs);

  if (nxt == XZR) {
    if (Rg != XZR)
      ldp(Rg, XZR, PSX(SP, 16));
  } else if (Rg == XZR)
    restRegisters(ctx, dropReg(regs, nxt), nxt);
  else {
    restRegisters(ctx, dropReg(regs, nxt), XZR);
    ldp(Rg, nxt, PSX(SP, 16));
  }
}

void restoreRegisters(assemCtxPo ctx, registerMap regs) {
  restRegisters(ctx, regs, XZR);
}

