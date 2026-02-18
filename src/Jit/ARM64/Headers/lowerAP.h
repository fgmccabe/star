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
#include "analyseP.h"

#define CO (X12)
#define AG  (X13)
#define STK (X14)
#define PR (X15)

typedef struct {
  FlexOp src;
  int32 stkOff;
  logical stashed; // Is the value in the stack frame?
  logical live;    // Is the variable in use?
  logical inited;
  varDescPo varDesc;
} LocalVar, *localVarPo;

typedef struct {
  methodPo mtd;
  analysisPo analysis;
  jitCompPo jit;
  localVarPo locals;
  int32 numLocals;
  int32 argPt;
  int32 top;
  varDescPo *stack;
} CodeGenState, *codeGenPo;

typedef struct jitBlock_ *blockPo;

typedef struct jitBlock_ {
  int32 startPc;
  int32 endPc;
  varDescPo phiVar;
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
void frameOverride(blockPo block, int arity);

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
