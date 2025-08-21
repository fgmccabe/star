//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "hash.h"
#include "lower.h"
#include "arm64P.h"
#include "macros.h"
#include "term.h"
#include "arith.h"
#include "char.h"
#include "jitP.h"

static inline logical isSmall(termPo x) {
  if (isInteger(x))
    return is16bit(integerVal(x));
  else if (isChar(x))
    return is16bit((integer) charVal(x));
  else
    return False;
}

typedef struct {
  FlexOp src;
} StackEntry;

typedef struct {
  StackEntry valueStack[128];
  int32 stackHeight;
} ValueStack, *valueStackPo;

typedef struct jitBlock_ *jitBlockPo;

typedef struct jitBlock_ {
  jitCompPo jit;
  int32 startPc;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  jitBlockPo parent;
} JitBlock;

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

retCode stackCheck(jitCompPo jit, methodPo mtd);

#define pointerSize ((int32)sizeof(integer))

retCode bailOut(jitCompPo jit, ExitCode code);

retCode getIntVal(jitCompPo jit, armReg rg);
retCode mkIntVal(jitCompPo jit, armReg rg);
retCode getFltVal(jitCompPo jit, armReg rg);

retCode mkFltVal(jitCompPo jit, armReg rg);

retCode jitError(jitCompPo jit, char *msg, ...);

jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt);
codeLblPo breakLabel(jitBlockPo block);
codeLblPo loopLabel(jitBlockPo block);

retCode breakOutEq(jitBlockPo block, insPo code, int32 tgt);
retCode breakOutNe(jitBlockPo block, insPo code, int32 tgt);
retCode breakOut(jitBlockPo block, insPo code, int32 tgt, logical keepTop);

void stash(jitCompPo jit);
void stashRegisters(jitCompPo jit, int32 stackLevel);
void unstash(jitCompPo jit);

void loadOffset(jitCompPo jit, armReg tgt, armReg base, int32 ix);
void storeOffset(jitCompPo jit, armReg src, armReg base, int32 lclNo);

void loadLocal(jitCompPo jit, armReg tgt, int32 lclNo);
void storeLocal(jitCompPo jit, armReg src, int32 lclNo);
void loadConstant(jitCompPo jit, int32 key, armReg tgt);

armReg allocSmallStruct(jitCompPo jit, clssPo class, integer amnt);

void setStackDepth(jitCompPo jit, int32 depth);
armReg popStkOp(jitCompPo jit, armReg tgt);
armReg topStkOp(jitCompPo jit);
void pushStkOp(jitCompPo jit, armReg src);
void pushStk(jitCompPo jit, VarRecord var);
armReg popStk(jitCompPo jit);

void storeStack(jitCompPo jit, armReg src, int32 depth);

void pshFrame(jitCompPo jit, assemCtxPo ctx, armReg mtdRg);
void overrideFrame(jitCompPo jit, assemCtxPo ctx, int arity);

retCode testResult(jitBlockPo block, insPo code, int32 tgt);
registerMap criticalRegs();

#endif //STAR_LOWERP_H
