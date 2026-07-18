//
// Created by Francis McCabe on 9/8/24.
//

#ifndef STAR_MACROS_H
#define STAR_MACROS_H

#include "jit.h"
#include "x86_64.h"

typedef uint64 registerMap;

registerMap defltAvailRegSet();
registerMap emptyRegSet();
registerMap allRegisters();
registerMap fixedRegSet(mcRegister Rg);

static inline registerMap scratchRegs() {
  return 1u << RAX | 1u << RCX;
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
integer lblDeltaRef(assemCtxPo ctx, codeLblPo tgt);
void emitLblRef(assemCtxPo ctx, codeLblPo tgt);
void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc);

typedef integer (*runtimeFn)();

void load(assemCtxPo ctx, x64Reg dst, x64Reg src, int64 offset);
void store(assemCtxPo ctx, x64Reg src, x64Reg dst, int64 offset, registerMap freeRegs);

void move(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap freeRegs);


retCode loadCGlobal(assemCtxPo ctx, x64Reg reg, void *address);

#endif //STAR_MACROS_H
