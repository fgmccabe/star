//
// Created by Francis McCabe on 9/8/24.
//

#include "arm64P.h"
#include "macros.h"
#include "jitP.h"

registerMap defltAvailRegSet() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7 | 1u << X8 | 1u << X9 |
         1u << X10 | 1u << X11;
}

registerMap emptyRegSet() {
  return 0;
}

registerMap fixedRegSet(armReg Rg) {
  return 1u << Rg;
}

registerMap mapUnion(registerMap a, registerMap b) {
  return a | b;
}

registerMap allocReg(registerMap from, armReg Rg) {
  check((from & ((uint64)1u << Rg)) != 0, "register not free");
  return (from & (~((uint64) 1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  // check((from & ((uint64)1u << Rg)) == 0, "register already free");
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

typedef struct {
  ioPo out;
  char *sep;
} ShowRegInfo;

static void showReg(armReg rg, void *cl) {
  ShowRegInfo *info = (ShowRegInfo *) cl;
  outMsg(info->out, "%s%R", info->sep, rg);
  info->sep = ", ";
}

void dRegisterMap(registerMap regs) {
  showRegisterMap(logFile, regs);
}

void showRegisterMap(ioPo out, registerMap regs) {
  outMsg(logFile, "registers: {");
  ShowRegInfo info = {.out = out, .sep = ""};
  processRegisterMap(regs, showReg, &info);
  outMsg(logFile, "}\n%_");
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
