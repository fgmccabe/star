//
// Created by Francis McCabe on 9/8/24.
//

#include "macros.h"

registerMap defltAvailRegSet() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7 | 1u << X8 | 1u << X9 |
         1u << X10;
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
  check((from & ((uint64)1u << Rg)) != 0, "register not free");
  return (from & (~((uint64)1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  check((from & ((uint64)1u << Rg)) == 0, "register already free");
  return (from | ((uint64)1u << Rg));
}

registerMap dropReg(registerMap map, armReg Rg) {
  return (map & (~((uint64)1u << Rg)));
}

registerMap addReg(registerMap from, armReg Rg) {
  return (from | ((uint64)1u << Rg));
}

logical isRegInMap(registerMap from, armReg Rg) {
  return ((from & ((uint64)1u << Rg)) != 0);
}

armReg nxtAvailReg(registerMap from) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = (uint64)1u << ix;
    if ((from & mask) != 0)
      return ix;
  }
  return XZR;
}

void processRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = (uint64)1u << ix;
    if ((set & mask) != 0)
      proc((armReg) ix, cl);
  }
}

void revProcessRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 64u; ix > 0;) {
    ix--;
    uint64 mask = (uint64)1u << ix;
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

static char *regNames[] = {
  "X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15",
  "X16", "X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "FP", "LR", "SP"
};

void showReg(armReg rg, void *cl) {
  outMsg((ioPo) cl, "%s ", regNames[(uint8) rg]);
}

void dRegisterMap(registerMap regs) {
  outMsg(logFile, "registers: {");
  processRegisterMap(regs, showReg, logFile);
  outMsg(logFile, "}\n");
  flushOut();
}

void sortArgs(FlexOp *args, int32 *index, int32 arity) {
  for (int32 ix=0;ix<arity;ix++)
    index[ix] = ix;

  for (int32 ix=0;ix<arity;ix++) {
armReg Rx = (armReg)ix;
    for (int32 jx=ix+1;jx<arity;jx++){
      if (sameFlexOp(args[index[jx]], RG(Rx))) {

      }
    }
  }
}

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, integer arity, ...) {
  va_list args;
  va_start(args, arity); /* start the variable argument sequence */
  FlexOp operands[arity];

  for (integer ix = 0; ix < arity; ix++) {
    operands[ix] = (FlexOp) va_arg(args, FlexOp);
  }
  va_end(args);

  saveRegisters(ctx, saveMap);

  switch (arity) {
    case 8:
      if (!sameFlexOp(RG(X7), operands[7]))
        mov(X7, operands[7]);
    case 7:
      if (!sameFlexOp(RG(X6), operands[6]))
        mov(X6, operands[6]);
    case 6:
      if (!sameFlexOp(RG(X5), operands[5]))
        mov(X5, operands[5]);
    case 5:
      if (!sameFlexOp(RG(X4), operands[4]))
        mov(X4, operands[4]);
    case 4:
      if (!sameFlexOp(RG(X3), operands[3]))
        mov(X3, operands[3]);
    case 3:
      if (!sameFlexOp(RG(X2), operands[2]))
        mov(X2, operands[2]);
    case 2:
      if (!sameFlexOp(RG(X1), operands[1]))
        mov(X1, operands[1]);
    case 1: {
      if (!sameFlexOp(RG(X0), operands[0]))
        mov(X0, operands[0]);
    }
    case 0: {
      mov(X16, IM((integer) fn));
      blr(X16);
      restoreRegisters(ctx, saveMap);
      return Ok;
    }
    default:
      return Error;
  }
}

retCode loadCGlobal(assemCtxPo ctx, armReg reg, void *address) {
  mov(reg, IM((integer) address));
  ldr(reg, OF(reg, 0));
  return Ok;
}
