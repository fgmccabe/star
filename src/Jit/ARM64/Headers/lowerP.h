//
// Created by Francis McCabe on 2/10/26.
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
#include "analyseP.h"

#define CO (X12)
#define AG  (X13)
#define STK (X14)
#define PR (X15)
#define RTV (X10)
#define RTS (X11)

typedef struct {
  FlexOp src;
  int32 stkOff;
  logical stashed; // Is the value in the stack frame?
  logical inited;  // Has the variable ever been written to?
  logical live;    // Is the variable in use?
  varDescPo desc;
} LocalVar, *localVarPo;

typedef struct {
  methodPo mtd;
  analysisPo analysis;
  jitCompPo jit;
  ssaInsPo code;
  localVarPo locals;
  logical* voided;
  int32 numLocals;
} CodeGenState, *codeGenPo;

typedef struct jitBlock_* blockPo;

typedef struct jitBlock_ {
  ssaOp blockType;
  int32 startPc;
  int32 endPc;
  int32 phiCnt;
  localVarPo *phiVars;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  blockPo parent;
} JitBlock;

void stackCheck(codeGenPo state, int32 pc, int32 argCnt, int32 lclCnt);

#define pointerSize ((int32)sizeof(integer))

void bailOut(codeGenPo state, int32 pc, ExitCode code);

retCode getIntVal(jitCompPo jit, armReg rg);
retCode mkIntVal(jitCompPo jit, armReg rg);
void getFltVal(jitCompPo jit, armReg rg, fpReg tgt);

retCode jitError(jitCompPo jit, char* msg, ...);

registerMap defaultArgRegs();
registerMap systemArgRegs();
registerMap lambdaArgRegs();
int32 maxArgRegister;
armReg findARegister(codeGenPo state, int32 pc);
void loadRegister(codeGenPo state, armReg rg, FlexOp src);

void invokeIntrinsic(codeGenPo state, int32 pc, int32 livePc, runtimeFn fn, int32 arity, FlexOp args[],
                     logical moveOwnership, int32 rsCnt, FlexOp results[]);

codeLblPo breakLabel(blockPo block);
codeLblPo loopLabel(blockPo block);
void breakOut(codeGenPo state, int32 nextPc, blockPo tgt);

blockPo targetBlock(blockPo block, int32 tgt, ssaOp blockType);

void storeFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt);
void loadFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt);
FlexOp constantFlex(int32 index);
FlexOp varFlex(int32 index);
void argMove(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap* freeRegs);

logical liveVar(localVarPo var, int32 pc);
int32 stashLiveLocals(codeGenPo state, int32 pc, logical moveOwnership);
registerMap registerLocals(codeGenPo state, int32 pc);
void restoreStashedLocals(codeGenPo state, int32 pc);
localVarPo localSource(codeGenPo state, int32 pc, int32 lx);
localVarPo localTarget(codeGenPo state, int32 pc, int32 lx);
logical allLocalsStashed(codeGenPo state, int32 pc);

void dumpState(codeGenPo state, int32 pc);
retCode showLocalVar(ioPo out, void* data, long depth, long precision, logical alt);

void voidOutFrameLocals(codeGenPo state, int32 pc, int32 minOffset);

localVarPo findSpareLocal(codeGenPo state, int32 pc);
int32 nextStkOff(codeGenPo state, int32 pc);
FlexOp getLclSrc(codeGenPo state, int32 pc, int32 lclNo);

void stash(jitCompPo jit, int32 depth);
void stashEngineState(jitCompPo jit, int32 stackLevel, registerMap freeRegs);
void unstashEngineState(jitCompPo jit);

void loadElement(jitCompPo jit, armReg tgt, armReg base, int32 ix);
void storeElement(jitCompPo jit, armReg src, armReg base, int32 ix);

void loadVarble(jitCompPo jit, armReg tgt, int32 varNo);
void storeVarble(jitCompPo jit, armReg src, int32 varNo);

void loadConstant(jitCompPo jit, int32 key, armReg tgt);

retCode showStackSlot(ioPo f, void* data, long depth, long precision, logical alt);
void frameOverride(blockPo block, int arity);
void frameOOverride(blockPo block, int arity, armReg frReg);

registerMap criticalRegs();

#define argSpec(s,d) (ArgSpec){.src = s, .dst = d, .mark = True, .group = -1}

static inline logical isSmall(termPo x) {
  if (isInteger(x))
    return is16bit(integerVal(x));
  else if (isChar(x))
    return is16bit((integer)charVal(x));
  else
    return False;
}

void breakPt();

void installBkCall(codeGenPo state, int32 pc);

#endif //STAR_LOWERP_H
