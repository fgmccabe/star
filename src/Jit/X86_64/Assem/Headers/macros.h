//
// Created by Francis McCabe on 9/8/24.
//

#ifndef STAR_MACROS_H
#define STAR_MACROS_H

#include "jit.h"
#include "x64-64.h"

typedef uint64 registerMap;

registerMap defltAvailRegSet();
registerMap emptyRegSet();
registerMap allRegisters();
registerMap fixedRegSet(armReg Rg);

static inline registerMap scratchRegs() {
  return 1u << RAX | 1u << RCX;
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

void load(assemCtxPo ctx, armReg dst, armReg src, int64 offset);
void store(assemCtxPo ctx, armReg src, armReg dst, int64 offset, registerMap freeRegs);

void move(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap freeRegs);

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, int32 arity, ...);
retCode loadCGlobal(assemCtxPo ctx, armReg reg, void *address);

#endif //STAR_MACROS_H
