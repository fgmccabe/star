//
// Created by Francis McCabe on 9/8/24.
//

#ifndef STAR_MACROS_H
#define STAR_MACROS_H

#include "jit.h"
#include "assem.h"
#include "arm64.h"

typedef uint64 registerMap;

registerMap defltAvailRegSet();
registerMap emptyRegSet();
registerMap fixedRegSet(armReg Rg);
registerMap mapUnion(registerMap a, registerMap b);

static inline registerMap scratchRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3;
}

static inline registerMap callerSaved() {
  return 1u << X8 | 1u << X9 | 1u << X10 | 1u << X11 | 1u << X12 | 1u << X13 | 1u << X14 | 1u << X15;
}

static inline registerMap calleeSaved() {
  return 1u << X19 | 1u << X20 | 1u << X21 | 1u << X22 | 1u << X23 | 1u << X24 | 1u << X25 | 1u << X26 | 1u << X27 | 1u
         << X28;
}

static inline registerMap stackControlRegs() {
  return 1u << X26 | 1u << X27 | 1u << X28 | 1u << X29 | 1u << X30;
}

registerMap allocReg(registerMap from, armReg Rg);
registerMap freeReg(registerMap from, armReg Rg);
registerMap dropReg(registerMap map, armReg Rg);
registerMap addReg(registerMap from, armReg Rg);
logical isRegInMap(registerMap from, armReg Rg);

armReg nxtAvailReg(registerMap from);

void saveRegisters(assemCtxPo ctx, registerMap regs);
void restoreRegisters(assemCtxPo ctx, registerMap regs);

typedef void (*regProc)(armReg rg, void *cl);

void processRegisterMap(registerMap set, regProc proc, void *cl);

void dRegisterMap(registerMap regs);
void showRegisterMap(ioPo out, registerMap regs);

typedef integer (*runtimeFn)();

void load(assemCtxPo ctx, armReg dst, armReg src, int64 offset);
void store(assemCtxPo ctx, armReg src, armReg dst, int64 offset, registerMap freeRegs);

void move(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap freeRegs);

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, int32 arity, ...);
retCode loadCGlobal(assemCtxPo ctx, armReg reg, void *address);

void immModulo(assemCtxPo ctx, armReg rg, int64 imm, registerMap freeRegs);

#endif //STAR_MACROS_H
