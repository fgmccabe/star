//
// Created by Francis McCabe on 10/08/25.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "abort.h"
#include "jitP.h"
#include "lower.h"
#include "macros.h"
#include "x86_64P.h"
#include "arith.h"
#include "char.h"

/* Register allocation for x64:
 *
 * RAX = return register
 * RDI 1st argument
 * RSI 2nd argument
 * RDX 3rd argument
 * RCX 4th argument
 * R8  Constants vector
 * R9  Args pointer
 * R10 Current stack
 * R11 Current engine
 * RBP Frame pointer
 * RSP = system stack pointer
 */

#define CO (R8)
#define AG  (R9)
#define STK (R10)
#define PR (R11)
#define FP (RBP)

typedef enum {
  inRegister,
  isLocal,
  inStack
} valueKind;

typedef struct {
  valueKind kind;
  mcRegister Rg;
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
  int32 exitHeight;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  ValueStack stack;
  jitBlockPo parent;
} JitBlock;

retCode stackCheck(jitCompPo jit, methodPo mtd);

#define pointerSize ((int32)sizeof(integer))

retCode bailOut(jitCompPo jit, ExitCode code);

retCode getIntVal(jitCompPo jit, mcRegister rg);
retCode mkIntVal(jitCompPo jit, mcRegister rg);
retCode getFltVal(jitCompPo jit, mcRegister rg);
retCode mkFltVal(jitCompPo jit, mcRegister rg);

retCode jitError(jitCompPo jit, char *msg, ...);

jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt, OpCode blockType);
codeLblPo getABreakLbl(jitBlockPo block, int32 pc);
codeLblPo breakLabel(jitBlockPo block);
codeLblPo loopLabel(jitBlockPo block);

retCode breakOutEq(jitBlockPo block, insPo code, int32 tgt);
retCode breakOutNe(jitBlockPo block, insPo code, int32 tgt);
retCode breakOut(jitBlockPo block, jitBlockPo tgtBlock);

void stash(jitBlockPo block);
void stashRegisters(jitCompPo jit, int32 stackLevel);
void unstash(jitCompPo jit);

void loadLocal(jitCompPo jit, mcRegister tgt, int32 lclNo);
void storeLocal(jitCompPo jit, mcRegister src, int32 lclNo);
void loadConstant(jitCompPo jit, int32 key, mcRegister tgt);

void dumpStack(valueStackPo stack);
int32 trueStackDepth(valueStackPo stack);
void setStackDepth(valueStackPo stack, jitCompPo jit, int32 depth);
retCode propagateStack(jitCompPo jit, valueStackPo srcStack, valueStackPo tgtStack, int32 tgtHeight);
void propagateVar(jitCompPo jit, localVarPo src, localVarPo dst);
void pushBlank(valueStackPo stack);
void pushValue(valueStackPo stack, LocalEntry var);
void pushRegister(valueStackPo stack, mcRegister rg);
mcRegister popValue(valueStackPo stack, jitCompPo jit);
mcRegister topValue(valueStackPo stack, jitCompPo jit);
void dropValue(valueStackPo stack, jitCompPo jit);
void dropValues(valueStackPo stack, jitCompPo jit, int32 count);
void spillLocals(valueStackPo stack, jitCompPo jit);
void spillStack(valueStackPo stack, jitCompPo jit);
void frameOverride(jitBlockPo block, int arity);
void setLocal(valueStackPo stack, int32 lclNo, LocalEntry entry);

localVarPo argSlot(valueStackPo stack, int32 slot);
localVarPo localSlot(valueStackPo stack, int32 slot);
localVarPo stackSlot(valueStackPo stack, int32 slot);

void storeStack(jitCompPo jit, mcRegister src, int32 depth);

retCode testResult(jitBlockPo block, jitBlockPo tgtBlock);
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
