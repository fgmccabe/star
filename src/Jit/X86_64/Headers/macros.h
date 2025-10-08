//
// Created by Francis McCabe on 6/25/25.
//

#ifndef MACROS_H
#define MACROS_H

#include "ooio.h"
#include "x86_64.h"

typedef uint64 registerMap;

registerMap defltAvailRegSet();
registerMap emptyRegSet();

static inline registerMap scratchRegs() {
  return 1u << RAX | 1u << RCX | 1u << RDX | 1u << RBX | 1u << R8 | 1u << R9;
}

static inline registerMap callerSaved() {
  return 1u << RAX | 1u << RCX | 1u << RDX | 1u << R8 | 1u << R9 | 1u << R10 | 1u << R11;
}

static inline registerMap calleeSaved() {
  return 1u << RBX | 1u << RBP | 1u << RDI | 1u << RSP | 1u << R12 | 1u << R13 | 1u << R14 | 1u << R15;
}

static inline registerMap stackControlRegs() {
  return 1u << RSP;
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

codeLblPo newLabel(assemCtxPo ctx);
codeLblPo here_(assemCtxPo ctx);
#define here() here_(ctx)
codeLblPo defineLabel(assemCtxPo ctx, integer pc);

codeLblPo setLabel_(assemCtxPo ctx, codeLblPo lbl);
#define bind(lbl) setLabel_(ctx,lbl)

logical isLabelDefined(codeLblPo lbl);
uint64 labelTgt(codeLblPo lbl);
retCode cleanupLabels(assemCtxPo ctx);

static retCode updateLblEntry(void *entry, integer ix, void *cl);
integer lblDeltaRef(assemCtxPo ctx, codeLblPo tgt);
void emitLblRef(assemCtxPo ctx, codeLblPo tgt);
void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc);

typedef integer (*runtimeFn)();

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, integer arity, ...);
retCode loadCGlobal(assemCtxPo ctx, mcRegister reg, void *address);


#endif //MACROS_H
