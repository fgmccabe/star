//
// Created by Francis McCabe on 8/20/25.
//

#include <config.h>

#include "cellP.h"
#include "lowerP.h"
#include "stackP.h"
#include "singleP.h"
#include "constantsP.h"
#include "jitP.h"
#include "debug.h"
#include "engineP.h"

// We need these registers preserved at all costs

registerMap criticalRegs() {
  return 1u << CO | 1u << PR;
}

void setStackDepth(jitCompPo jit, int32 depth) {
  jit->stackDepth = depth;
  check(jitStackHasRoom(jit,0), "Invalid jit stack depth");
}

armReg popStkOp(jitCompPo jit, armReg tgt) {
  check(jitStackHasRoom(jit,1), "Insufficient stack depth for pop stack");
  loadLocal(jit, tgt, -jitTrueStackDepth(jit));
  jit->stackDepth--;
  return tgt;
}

armReg topStkOp(jitCompPo jit) {
  armReg tgt = findFreeReg(jit);
  check(jitStackHasRoom(jit,1), "Invalid jit stack depth");
  loadLocal(jit, tgt, -jitTrueStackDepth(jit));
  return tgt;
}

void pushStkOp(jitCompPo jit, armReg src) {
  jit->stack[jit->stackDepth].state = inRegister;
  jit->stack[jit->stackDepth].Rg = src;
  jit->stackDepth++;
  storeLocal(jit, src, -jitTrueStackDepth(jit));
}

void pushStk(jitCompPo jit, VarRecord var) {
  jit->stack[jit->stackDepth++] = var;
}

armReg popStk(jitCompPo jit) {
  VarRecord var = jit->stack[--jit->stackDepth];

  switch (var.state) {
    case localVar: {
      armReg tgt = findFreeReg(jit);
      loadLocal(jit, tgt, var.offset);
      return tgt;
    }
    case inRegister:
      return var.Rg;
    case emptyVar: {
      bailOut(jit, 45);
    }
    case globalConst: {
      armReg tgt = findFreeReg(jit);
      loadConstant(jit, var.id, tgt);
      return tgt;
    }
  }
}

void storeStack(jitCompPo jit, armReg src, int32 depth) {
  storeLocal(jit, src, -(lclCount(jit->mtd) + depth));
}

void loadLocal(jitCompPo jit, armReg tgt, int32 lclNo) {
  check(lclNo>=jit->minOffset && lclNo<jit->maxOffset, "Accessing out of bounds locals");
  loadOffset(jit, tgt,AG, lclNo);
}

void storeLocal(jitCompPo jit, armReg src, int32 lclNo) {
  check(lclNo>=jit->minOffset && lclNo<jit->maxOffset, "Accessing out of bounds locals");
  storeOffset(jit, src,AG, lclNo);
}

void loadConstant(jitCompPo jit, int32 key, armReg tgt) {
  assemCtxPo ctx = assemCtx(jit);
  termPo lit = getConstant(key);

  if (isSmall(lit))
    mov(tgt, IM((integer) lit));
  else
    loadOffset(jit, tgt,CO, key);
}

void loadOffset(jitCompPo jit, armReg tgt, armReg base, int32 ix) {
  assemCtxPo ctx = assemCtx(jit);
  int32 offset = ix * pointerSize;
  if (is9bit(offset))
    ldur(tgt, base, offset);
  else {
    mov(tgt, IM(ix));
    ldr(tgt, EX2(base,tgt,U_XTX,3));
  }
}

void storeOffset(jitCompPo jit, armReg src, armReg base, int32 lclNo) {
  assemCtxPo ctx = assemCtx(jit);
  int32 offset = lclNo * pointerSize;
  if (is9bit(offset))
    stur(src, base, offset);
  else {
    armReg tmp = findFreeReg(jit);
    mov(tmp, IM(lclNo));
    str(src, EX2(base,tmp,U_XTX,3));
    releaseReg(jit, tmp);
  }
}

retCode stackCheck(jitCompPo jit, methodPo mtd) {
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = newLabel(ctx);
  int32 delta = (stackDelta(mtd) + (int32) FrameCellCount) * pointerSize;

  if (is16bit(delta))
    sub(X16, AG, IM(delta));
  else {
    mov(X16, IM(delta));
    sub(X16, AG, RG(X16));
  }
  cmp(X16, RG(FP));
  bhi(okLbl);

  mov(X0, IM((integer) mtd));
  str(X0, OF(STK, OffsetOf(StackRecord, prog)));
  adr(X0, okLbl);
  str(X0, OF(STK,OffsetOf(StackRecord,pc)));

  stashRegisters(jit, 0);
  retCode ret =
    callIntrinsic(ctx, criticalRegs(), (runtimeFn) handleStackOverflow, 4, RG(PR), IM(True), IM(delta),
                  IM(mtdArity(mtd)));
  unstash(jit);

  bind(okLbl);
  return ret;
}
