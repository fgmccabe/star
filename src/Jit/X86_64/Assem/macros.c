//
// Created by Francis McCabe on 9/30/25.
//

#include "x86_64P.h"
#include "macros.h"

#define XZR 16 // Define XZR as 16 (invalid register for x64) to satisfy nxtAvailReg

registerMap emptyRegSet() {
  return 0;
}

registerMap allRegisters(){
  return 1u << RAX | 1u << RCX | 1u << RDX | 1u << RBX | 1u<< RSP | 1u << RBP | 1u << RSI | 1u << RDI
  | 1u << R8 | 1u << R9 | 1u << R10  | 1u << R11 | 1u << R12 | 1u << R13 | 1u << R14 | 1u << R15;
}

registerMap defltAvailRegSet() {
  return 1u << RAX | 1u << RCX | 1u << RDX | 1u << RBX | 1u << RSI | 1u << RDI | 1u << R8 | 1u << R9 |
         1u << R10 | 1u << R11 | 1u << R12 | 1u << R13 | 1u << R14 | 1u << R15;
}

registerMap fixedRegSet(mcRegister Rg) {
  return 1u << Rg;
}

registerMap allocReg(registerMap from, mcRegister Rg) {
  check((from & ((uint64)1u << Rg)) != 0, "register not free");
  return (from & (~((uint64) 1u << Rg)));
}

registerMap freeReg(registerMap from, mcRegister Rg) {
  check((from & ((uint64)1u << Rg)) == 0, "register already free");
  return (from | ((uint64) 1u << Rg));
}

registerMap dropReg(registerMap map, mcRegister Rg) {
  return (map & (~((uint64) 1u << Rg)));
}

registerMap addReg(registerMap from, mcRegister Rg) {
  return (from | ((uint64) 1u << Rg));
}

logical isRegInMap(registerMap from, mcRegister Rg) {
  return ((from & ((uint64) 1u << Rg)) != 0);
}

mcRegister nxtAvailReg(registerMap from) {
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
      proc((mcRegister) ix, cl);
  }
}

void revProcessRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 64u; ix > 0;) {
    ix--;
    uint64 mask = (uint64) 1u << ix;
    if ((set & mask) != 0)
      proc((mcRegister) ix, cl);
  }
}

void saveRegisters(assemCtxPo ctx, registerMap regs) {
  for (int ix = 0; ix < 64; ix++) {
    uint64 mask = (uint64) 1u << ix;
    if ((regs & mask) != 0) {
      push(RG((x64Reg)ix));
    }
  }
}

void restoreRegisters(assemCtxPo ctx, registerMap regs) {
  for (int ix = 63; ix >= 0; ix--) {
    uint64 mask = (uint64) 1u << ix;
    if ((regs & mask) != 0) {
      pop(RG((x64Reg)ix));
    }
  }
}

void showReg(mcRegister rg, void *cl) {
  outMsg((ioPo) cl, "%R ", rg);
}

void dRegisterMap(registerMap regs) {
  outMsg(logFile, "registers: {");
  processRegisterMap(regs, showReg, logFile);
  outMsg(logFile, "}\n");
  flushOut();
}

retCode loadCGlobal(assemCtxPo ctx, mcRegister reg, void *address) {
  mov(RG(reg), IM((integer) address));
  mov(RG(reg), BS(reg, 0));
  return Ok;
}

void load(assemCtxPo ctx, mcRegister dst, mcRegister src, int64 offset) {
  if (isI32(offset)) {
    mov(RG(dst), BS(src, offset));
  } else {
    mov(RG(dst), IM(offset));
    mov(RG(dst), IX(src, dst, 1, 0));
  }
}

void store(assemCtxPo ctx, mcRegister src, mcRegister dst, int64 offset, registerMap freeRegs) {
  if (isI32(offset)) {
    mov(BS(dst, offset), RG(src));
  } else {
    mcRegister tmp = nxtAvailReg(freeRegs);
    mov(RG(tmp), IM(offset));
    mov(IX(dst, tmp, 1, 0), RG(src));
  }
}

void move(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap freeRegs) {
  switch (dst.mode) {
    case Reg: {
      switch (src.mode) {
        case Reg:
        case Immediate:
          mov(dst, src);
          return;
        case Based:
          load(ctx, dst.op.reg, src.op.based.base, src.op.based.disp);
          return;
        default:
          check(False, "unsupported source mode");
          return;
      }
    }
    case Based: {
      switch (src.mode) {
        case Reg:
          store(ctx, src.op.reg, dst.op.based.base, dst.op.based.disp, freeRegs);
          return;
        case Based: {
          if (src.op.based.disp != dst.op.based.disp || src.op.based.base != dst.op.based.base) {
            mcRegister tmp = nxtAvailReg(freeRegs);
            load(ctx, tmp, src.op.based.base, src.op.based.disp);
            store(ctx, tmp, dst.op.based.base, dst.op.based.disp, dropReg(freeRegs, tmp));
          }
          return;
        }
        case Immediate: {
          if (isI32(src.op.imm)) {
            mov(dst, src);
          } else {
            mcRegister tmp = nxtAvailReg(freeRegs);
            mov(RG(tmp), src);
            store(ctx, tmp, dst.op.based.base, dst.op.based.disp, dropReg(freeRegs, tmp));
          }
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
