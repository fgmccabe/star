//
// Created by Francis McCabe on 9/8/24.
//

#ifndef STAR_MACROS_H
#define STAR_MACROS_H

#include "jit.h"
#include "arm64.h"

typedef uint64 registerMap;

registerMap defltAvailRegSet();
registerMap emptyRegSet();

static inline registerMap callerSaved() {
  return 1u << X9 | 1u << X10 | 1u << X11 | 1u << X12 | 1u << X13 | 1u << X14 | 1u << X15;
}

static inline registerMap calleeSaved() {
  return 1u << X19 | 1u << X20 | 1u << X21 | 1u << X22 | 1u << X23 | 1u << X24 | 1u << X25 | 1u << X26;
}

static inline registerMap stackRegs() {
  return 1u << X9 | 1u << X10 | 1u << X11 | 1u << X12;
}

registerMap allocReg(registerMap from, armReg Rg);
registerMap freeReg(registerMap from, armReg Rg);
registerMap addReg(registerMap from, armReg Rg);

armReg nxtAvailReg(registerMap from);

void saveRegisters(assemCtxPo ctx, registerMap regs);
void restoreRegisters(assemCtxPo ctx, registerMap regs);

typedef void (*regProc)(armReg rg, void *cl);

void processRegisterMap(registerMap set, regProc proc, void *cl);

#endif //STAR_MACROS_H
