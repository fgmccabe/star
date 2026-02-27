//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "abort.h"
#include "lower.h"
#include "arm64P.h"
#include "macros.h"
#include "term.h"
#include "arith.h"
#include "char.h"
#include "jitP.h"

/* Register allocation for arm64:
 *
 * X0 = return register
 * X0-X10 = argument registers & scratch registers
 * X12 = Constants vector
 * AG = X13 = args pointer
 * STK = X14 = current stack structure pointer
 * X15 = current process structure
 * X16-X17 = intra procedure call scratch registers
 * X18 = platform register
 * X19-X28 = callee saved registers
 * FP = X29 = frame pointer
 * LR = X30 = link register
 * SP = X31 = system stack pointer
 */

#define CO (X12)
#define AG  (X13)
#define STK (X14)
#define PR (X15)

typedef enum {
  inReg,
  isLocal,
  inStack
} valueKind;

typedef struct {
  valueKind kind;
  armReg Rg;
  int32 stkOff;
  logical inited;
} LocalEntry, *localVarPo;

typedef struct {
  localVarPo locals;
  int32 lclCount;
  int32 argPnt;
  int32 stackPnt;
  int32 vTop;
} ValueStack, *valueStackPo;

typedef struct jitBlock_ *jitBlockPo;

typedef struct jitBlock_ {
  jitCompPo jit;
  int32 startPc;
  int32 endPc;
  int32 entryHeight;
  int32 exitHeight;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  ValueStack stack;
  jitBlockPo parent;
} JitBlock;

retCode stackCheck(jitCompPo jit, methodPo mtd);

#define pointerSize ((int32)sizeof(integer))

retCode bailOut(jitCompPo jit, ExitCode code);

retCode getIntVal(jitCompPo jit, armReg rg);
retCode mkIntVal(jitCompPo jit, armReg rg);
void getFltVal(jitCompPo jit, armReg rg, fpReg tgt);

armReg mkFloat(jitBlockPo block);

retCode jitError(jitCompPo jit, char *msg, ...);

jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt, OpCode blockType);
codeLblPo getABreakLbl(jitBlockPo block, int32 pc);
codeLblPo breakLabel(jitBlockPo block);
codeLblPo loopLabel(jitBlockPo block);

retCode breakOutEq(jitBlockPo block, insPo code, int32 tgt);
retCode breakOutNe(jitBlockPo block, insPo code, int32 tgt);
retCode breakOut(assemCtxPo ctx, jitBlockPo tgtBlock);

void stash(jitBlockPo block);
void stashEngineState(jitCompPo jit, int32 stackLevel);
void unstashEngineState(jitCompPo jit);

void loadElement(jitCompPo jit, armReg tgt, armReg base, int32 ix);
void storeElement(jitCompPo jit, armReg src, armReg base, int32 ix);

void loadVarble(jitCompPo jit, armReg tgt, int32 varNo);
void storeVarble(jitCompPo jit, armReg src, int32 varNo);

void loadConstant(jitCompPo jit, int32 key, armReg tgt);

void dumpStack(valueStackPo stack);
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
void frameOverride(jitBlockPo block, int arity);
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
#endif //STAR_LOWERP_H
