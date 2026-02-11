//
// Created by Francis McCabe on 2/10/26.
//

#ifndef STAR_LOWERAP_H
#define STAR_LOWERAP_H

#include "abort.h"
#include "lower.h"
#include "arm64P.h"
#include "macros.h"
#include "term.h"
#include "arith.h"
#include "char.h"
#include "jitP.h"

#define CO (X12)
#define AG  (X13)
#define STK (X14)
#define PR (X15)

typedef struct {
  armReg Rg;
  int32 stkOff;
  int32 fromPc;
  int32 toPc;
  logical stashed; // Is the value in memory?
  logical live;    // Is the value in a register?
  logical inited;
} LocalVar, *localVarPo;

typedef struct jitBlock_ *blockPo;

typedef struct jitBlock_ {
  jitCompPo jit;
  int32 startPc;
  int32 endPc;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  blockPo parent;
} JitBlock;

retCode stackCheck(jitCompPo jit, methodPo mtd);

#define pointerSize ((int32)sizeof(integer))

retCode bailOut(jitCompPo jit, ExitCode code);

retCode getIntVal(jitCompPo jit, armReg rg);
retCode mkIntVal(jitCompPo jit, armReg rg);
void getFltVal(jitCompPo jit, armReg rg, fpReg tgt);

retCode jitError(jitCompPo jit, char *msg, ...);

blockPo breakBlock(blockPo block, insPo code, int32 tgt, OpCode blockType);
codeLblPo getABreakLbl(blockPo block, int32 pc);
codeLblPo breakLabel(blockPo block);
codeLblPo loopLabel(blockPo block);

retCode breakOutEq(blockPo block, insPo code, int32 tgt);
retCode breakOutNe(blockPo block, insPo code, int32 tgt);
retCode breakOut(blockPo block, blockPo tgtBlock);

void stash(blockPo block);
void stashRegisters(jitCompPo jit, int32 stackLevel);
void unstash(jitCompPo jit);

void loadOffset(jitCompPo jit, armReg tgt, armReg base, int32 ix);
void storeOffset(jitCompPo jit, armReg src, armReg base, int32 lclNo);

void loadVarble(jitCompPo jit, armReg tgt, int32 varNo);
void storeVarble(jitCompPo jit, armReg src, int32 varNo);

void loadConstant(jitCompPo jit, int32 key, armReg tgt);

retCode showStackSlot(ioPo f, void *data, long depth, long precision, logical alt);
int32 trueStackDepth(valueStackPo stack);
void setStackDepth(valueStackPo stack, jitCompPo jit, int32 depth);
retCode propagateStack(jitCompPo jit, valueStackPo srcStack, valueStackPo tgtStack, int32 tgtHeight);
localVarPo pushBlank(valueStackPo stack);
void pushValue(valueStackPo stack, LocalEntry var);
void pushRegister(valueStackPo stack, armReg rg);
void pushConstant(jitCompPo jit, valueStackPo stack, int32 key);
void forcePush(jitCompPo jit, valueStackPo stack, armReg rg) ;
armReg popValue(valueStackPo stack, jitCompPo jit);
armReg topValue(valueStackPo stack, jitCompPo jit);
armReg stackValue(valueStackPo stack, jitCompPo jit, int32 depth);
void dropValue(valueStackPo stack, jitCompPo jit);
void dropValues(valueStackPo stack, jitCompPo jit, int32 count);
void spillStack(valueStackPo stack, jitCompPo jit);
void frameOverride(blockPo block, int arity);
void setLocal(valueStackPo stack, int32 lclNo, LocalEntry entry);

localVarPo argSlot(valueStackPo stack, int32 slot);
localVarPo localSlot(valueStackPo stack, int32 slot);
localVarPo stackSlot(valueStackPo stack, int32 slot);

void loadStack(jitCompPo jit, armReg tgt, int32 depth);
void storeStack(jitCompPo jit, armReg src, int32 depth);

registerMap criticalRegs();

static inline logical isSmall(termPo x) {
  if (isInteger(x))
    return is16bit(integerVal(x));
  else if (isChar(x))
    return is16bit((integer) charVal(x));
  else
    return False;
}

#endif //STAR_LOWERAP_H
