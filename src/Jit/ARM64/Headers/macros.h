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

static inline registerMap scratchRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3;
}

static inline registerMap callerSaved() {
  return 1u << X8 | 1u << X9 | 1u << X10 | 1u << X11 | 1u << X12 | 1u << X13 | 1u << X14 | 1u << X15;
}

static inline registerMap calleeSaved() {
  return 1u << X19 | 1u << X20 | 1u << X21 | 1u << X22 | 1u << X23 | 1u << X24 | 1u << X25 | 1u << X26 | 1u << X27 | 1u << X28;
}

static inline registerMap stackControlRegs() {
  return 1u << X26 | 1u << X27 | 1u << X28 | 1u << X29 | 1u << X30;
}

registerMap nonSpillSet(integer arity);
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

codeLblPo newLabel(assemCtxPo ctx);
codeLblPo currentPcLabel(assemCtxPo ctx);
codeLblPo defineLabel(assemCtxPo ctx, integer pc);
void setLabel(assemCtxPo ctx, codeLblPo lbl);
logical isLabelDefined(codeLblPo lbl);
uint64 labelTgt(codeLblPo lbl);
retCode cleanupLabels(assemCtxPo ctx);

static retCode updateLblEntry(void *entry, integer ix, void *cl);
integer lblDeltaRef(assemCtxPo ctx, codeLblPo tgt);
void emitLblRef(assemCtxPo ctx, codeLblPo tgt);
void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc);

typedef integer (*runtimeFn)();

retCode callIntrinsic(assemCtxPo ctx, runtimeFn fn, integer arity, ...);
retCode loadCGlobal(assemCtxPo ctx, armReg reg, void *address);

#endif //STAR_MACROS_H
