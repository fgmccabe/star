//
// Created by Francis McCabe on 2/10/26.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "abort.h"
#include "lower.h"
#include "x86_64P.h"

#define OF(Rg, Off) BS(Rg, Off)
#define sOff Based
#define isOffsetOp(a) ((a).mode == Based)
static inline logical isRegisterOp(FlexOp a) { return a.mode == Reg; }
#include "macros.h"
#include "term.h"
#include "arith.h"
#include "char.h"
#include "jitP.h"
#include "analyseP.h"

#define CO (R12)
#define AG  (R13)
#define STK (R14)
#define PR (R15)
#define RTS (RAX)
#define RTV (RDX)
#define FP  (RBP)
#define LR  (R11)
#define X0  (RDI)
#define X1  (RSI)
#define X2  (RDX)
#define X3  (RCX)
#define X4  (R8)
#define X5  (R9)
#define X16 (R10)

#define F0 (XMM0)
#define F1 (XMM1)
#define F2 (XMM2)
#define F3 (XMM3)
#define F4 (XMM4)
#define F5 (XMM5)
#define F6 (XMM6)
#define F7 (XMM7)
#define F8 (XMM8)
#define F9 (XMM9)
#define F10 (XMM10)
#define F11 (XMM11)
#define F12 (XMM12)
#define F13 (XMM13)
#define F14 (XMM14)
#define F15 (XMM15)

#define cbz(reg, lbl) { test(RG(reg), RG(reg)); j_cc_(lbl, EQ_CC, ctx); }
#define cbnz(reg, lbl) { test(RG(reg), RG(reg)); j_cc_(lbl, NE_CC, ctx); }
#define b(lbl) jmp(LB(lbl))
#define br(tgt) jmp(RG(tgt))
#define adr(reg, lbl) lea(RG(reg), LB(lbl))
#define blr(tgt) {\
  codeLblPo rtn = newLabel(ctx);\
  lea(RG(LR), LB(rtn));\
  jmp(RG(tgt));\
  bind(rtn);\
}
#define ldrw(reg, src) mov(RG32(reg), src)
#define ldr(reg, src) mov(RG(reg), src)
#define str(reg, dst) mov(dst, RG(reg))
#define cbz_w(reg, lbl) { test(RG32(reg), RG32(reg)); j_cc_(lbl, EQ_CC, ctx); }
#define cbnz_w(reg, lbl) { test(RG32(reg), RG32(reg)); j_cc_(lbl, NE_CC, ctx); }
#define cmp_w(dst, src) cmp_(RG32(dst), src, ctx)
#define mov_w(dst, src) mov_(RG32(dst), src, ctx)
#define cqo() cqo_(ctx)
#define cdq() cdq_(ctx)

typedef struct {
  FlexOp src;
  int32 stkOff;
  logical stashed; // Is the value in the stack frame?
  logical inited;  // Has the variable ever been written to?
  logical inUse;   // Is the variable in use?
  varDescPo desc;
} LocalVar, *localVarPo;

typedef struct {
  methodPo mtd;
  analysisPo analysis;
  jitCompPo jit;
  ssaInsPo code;
  localVarPo locals;
  logical* voided;
  int32 argMark;
  int32 numLocals;
} CodeGenState, *codeGenPo;

typedef struct jitBlock_* blockPo;

typedef struct jitBlock_ {
  ssaOp blockType;
  int32 startPc;
  int32 endPc;
  int32 phiCnt;
  localVarPo* phiVars;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  blockPo parent;
} JitBlock;

void stackCheck(codeGenPo state, int32 pc, int32 arity, int32 lcls);

#define pointerSize ((int32)sizeof(integer))

void verifyState(codeGenPo state, int32 pc);
void bailOut(codeGenPo state, int32 pc, ExitCode code);

retCode getIntVal(jitCompPo jit, mcRegister rg);
retCode mkIntVal(jitCompPo jit, mcRegister rg);
void getFltVal(jitCompPo jit, mcRegister rg, fpReg tgt);

retCode jitError(jitCompPo jit, char* msg, ...);

registerMap defaultArgRegs();
registerMap systemArgRegs();
registerMap lambdaArgRegs();
extern int32 maxArgRegister;
mcRegister findMcRegister(codeGenPo state, int32 pc);
void loadRegister(codeGenPo state, mcRegister rg, FlexOp src);

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

typedef int32 (*localVarProc)(codeGenPo state, int32 pc, localVarPo var, void* cl);
int32 processLocals(codeGenPo state, int32 pc, localVarProc proc, void* cl);

logical liveVar(localVarPo var, int32 pc);
int32 flushArguments(codeGenPo state, int32 pc);
int32 stashLiveLocals(codeGenPo state, int32 pc, logical moveOwnership);
registerMap registerLocals(codeGenPo state, int32 pc);
void restoreStashedLocals(codeGenPo state, int32 pc);
localVarPo localSource(codeGenPo state, int32 pc, int32 lx);
localVarPo localTarget(codeGenPo state, int32 pc, int32 lx);
logical allLocalsStashed(codeGenPo state, int32 pc);
int32 argSaveCnt(int32 arity);

void dumpState(codeGenPo state, int32 pc);
retCode showLocalVar(ioPo out, void* data, long depth, long precision, logical alt);

void voidOutFrameLocals(codeGenPo state, int32 pc, int32 minOffset);

localVarPo findSpareLocal(codeGenPo state, int32 pc);
int32 nextStkOff(codeGenPo state, int32 pc);
FlexOp getLclSrc(codeGenPo state, int32 pc, int32 lclNo);
mcRegister nxtAvailArgReg(registerMap from);

void stash(jitCompPo jit, int32 depth);
void stashEngineState(jitCompPo jit, int32 stackLevel, registerMap freeRegs);
void unstashEngineState(jitCompPo jit);

void loadElement(jitCompPo jit, mcRegister tgt, mcRegister base, int32 ix);
void storeElement(jitCompPo jit, mcRegister src, mcRegister base, int32 ix);

void loadVarble(jitCompPo jit, mcRegister tgt, int32 varNo);
void storeVarble(jitCompPo jit, mcRegister src, int32 varNo);

void writeBarrier(codeGenPo state, int32 pc, FlexOp src);

void loadConstant(jitCompPo jit, int32 key, mcRegister tgt);

retCode showStackSlot(ioPo f, void* data, long depth, long precision, logical alt);
void frameOverride(blockPo block, int arity);
void frameOOverride(blockPo block, int arity, mcRegister frReg);

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

void conditionalSelect(assemCtxPo ctx, FlexOp dst, FlexOp trueVal, FlexOp falseVal, uint8 cond);
void shiftRegister(codeGenPo state, int32 pc, ssaOp op, mcRegister dst, mcRegister src1, mcRegister src2);

#define csel(Rd, Rn, Rm, Cond) conditionalSelect(ctx, RG(Rd), RG(Rn), RG(Rm), Cond)
#define EQ EQ_CC
#define NE NE_CC
#define LT LT_CC
#define GE GE_CC

void breakPt();

void getFltVal(jitCompPo jit, mcRegister rg, fpReg tgt);

void installBkPt(codeGenPo state, int32 pc);

#endif //STAR_LOWERP_H
