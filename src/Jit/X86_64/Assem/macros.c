//
// Created by Francis McCabe on 9/30/25.
//

#include "x86-64P.h"
#include "macros.h"

registerMap emptyRegSet() {
  return 0;
}

registerMap allRegisters(){
    1u << RAX | 1u << RCX | 1u << RDX | 1u << RBX | 1u<< RSP | 1u << RBP | 1u << RSI | 1u << RDI
  | 1u << R8 | 1u << R9 | 1u << R10  | 1u << R11 | 1u << R12 | 1u << R13 | 1u << R14 | 1u << R15
}

registerMap defltAvailRegSet() {
  return 1u << RAX | 1u << R | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7 | 1u << X8 | 1u << X9 |
         1u << X10;
}

registerMap fixedRegSet(armReg Rg) {
  return 1u << Rg;
}

registerMap allocReg(registerMap from, armReg Rg) {
  check((from & ((uint64)1u << Rg)) != 0, "register not free");
  return (from & (~((uint64) 1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  check((from & ((uint64)1u << Rg)) == 0, "register already free");
  return (from | ((uint64) 1u << Rg));
}

registerMap dropReg(registerMap map, armReg Rg) {
  return (map & (~((uint64) 1u << Rg)));
}

registerMap addReg(registerMap from, armReg Rg) {
  return (from | ((uint64) 1u << Rg));
}

logical isRegInMap(registerMap from, armReg Rg) {
  return ((from & ((uint64) 1u << Rg)) != 0);
}

armReg nxtAvailReg(registerMap from) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = (uint64) 1u << ix;
    if ((from & mask) != 0)
      return ix;
  }
  return XZR;
}

void processRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = (uint64) 1u << ix;
    if ((set & mask) != 0)
      proc((armReg) ix, cl);
  }
}

void revProcessRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 64u; ix > 0;) {
    ix--;
    uint64 mask = (uint64) 1u << ix;
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

void showReg(armReg rg, void *cl) {
  outMsg((ioPo) cl, "%R ", rg);
}

void dRegisterMap(registerMap regs) {
  outMsg(logFile, "registers: {");
  processRegisterMap(regs, showReg, logFile);
  outMsg(logFile, "}\n");
  flushOut();
}

retCode loadCGlobal(assemCtxPo ctx, armReg reg, void *address) {
  mov(reg, IM((integer) address));
  ldr(reg, OF(reg, 0));
  return Ok;
}

void load(assemCtxPo ctx, armReg dst, armReg src, int64 offset) {
  if (is9bit(offset))
    ldur(dst, src, offset);
  else {
    mov(dst, IM(offset));
    ldr(dst, EX2(src, dst, U_XTX, 0));
  }
}

void store(assemCtxPo ctx, armReg src, armReg dst, int64 offset, registerMap freeRegs) {
  if (is9bit(offset))
    stur(src, dst, offset);
  else {
    armReg tmp = nxtAvailReg(freeRegs);
    mov(tmp, IM(offset));
    str(src, EX2(dst, tmp, U_XTX, 0));
  }
}

void move(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap freeRegs) {
  switch (dst.mode) {
    case reg: {
      switch (src.mode) {
        case reg:
        case imm:
          mov(dst.reg, src);
          return;
        case sOff:
          load(ctx, dst.reg, src.reg, src.immediate);
          return;
        default:
          check(False, "unsupported source mode");
          return;
      }
    }
    case sOff: {
      switch (src.mode) {
        case reg:
          store(ctx, src.reg, dst.reg, dst.immediate, freeRegs);
          return;
        case sOff: {
          if (src.immediate != dst.immediate || src.reg != dst.reg) {
            armReg tmp = nxtAvailReg(freeRegs);
            load(ctx, tmp, src.reg, src.immediate);
            store(ctx, tmp, dst.reg, dst.immediate, dropReg(freeRegs, tmp));
          }
          return;
        }
        case imm: {
          armReg tmp = nxtAvailReg(freeRegs);
          mov(tmp, src);
          store(ctx, tmp, dst.reg, dst.immediate, dropReg(freeRegs, tmp));
          return;
        }
        default: {
          check(False, "unsupported source mode");
          return;
        }
      }
    }
    default:
      check(False, "unsupported destination mode");
  }
}
