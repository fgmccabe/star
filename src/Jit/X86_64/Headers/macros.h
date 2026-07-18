//
// Created by Francis McCabe on 9/8/24.
//

#ifndef STAR_MACROS_H
#define STAR_MACROS_H

#include "jit.h"
#include "assem.h"
#include "x86_64.h"

typedef uint64 registerMap;

registerMap defltAvailRegSet();
registerMap emptyRegSet();
registerMap fixedRegSet(mcRegister Rg);
registerMap mapUnion(registerMap a, registerMap b);

static inline registerMap scratchRegs() {
  return 1u << RAX | 1u << RCX;
}

static inline registerMap callerSaved() {
  return 1u << RAX | 1u << RCX | 1u << RDX | 1u << RSI | 1u << RDI | 1u << R8 | 1u << R9 | 1u << R10 | 1u << R11;
}

static inline registerMap calleeSaved() {
  return 1u << RBX | 1u << RSP | 1u << RBP | 1u << R12 | 1u << R13 | 1u << R14 | 1u << R15;
}

static inline registerMap stackControlRegs() {
  return 1u << RSP | 1u << RBP;
}

registerMap allocReg(registerMap from, mcRegister Rg);
registerMap freeReg(registerMap from, mcRegister Rg);
registerMap dropReg(registerMap map, mcRegister Rg);
registerMap addReg(registerMap from, mcRegister Rg);
logical isRegInMap(registerMap from, mcRegister Rg);

mcRegister nxtAvailReg(registerMap from);

void saveRegisters(assemCtxPo ctx, registerMap regs);
void restoreRegisters(assemCtxPo ctx, registerMap regs);

typedef void (*regProc)(mcRegister rg, void *cl);

void processRegisterMap(registerMap set, regProc proc, void *cl);

void dRegisterMap(registerMap regs);
void showRegisterMap(ioPo out, registerMap regs);

typedef integer (*runtimeFn)();

void load(assemCtxPo ctx, mcRegister dst, mcRegister src, int64 offset);
void store(assemCtxPo ctx, mcRegister src, mcRegister dst, int64 offset);

void move(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap freeRegs);

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, int32 arity, ...);
retCode loadCGlobal(assemCtxPo ctx, mcRegister reg, void *address);

void immModulo(assemCtxPo ctx, mcRegister rg, int64 imm, registerMap freeRegs);

#endif //STAR_MACROS_H
