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
#include "globals.h"

// We need these registers preserved at all costs

registerMap criticalRegs() {
  return 1u << CO | 1u << PR;
}

localVarPo stackSlot(jitBlockPo block, int32 slot) {
  return &block->stack.local[block->stack.stackPnt - block->stack.vTop + slot];
}

localVarPo localSlot(jitBlockPo block, int32 slot) {
  return &block->stack.local[block->stack.argPnt - slot];
}

localVarPo argSlot(jitBlockPo block, int32 slot) {
  return &block->stack.local[block->stack.argPnt + slot];
}

int32 trueStackDepth(jitBlockPo block) {
  return lclCount(block->jit->mtd) + block->stack.stackDepth;
}

void setStackDepth(jitBlockPo block, int32 depth) {
  assert(depth<=block->stack.vTop);
  jitCompPo jit = block->jit;
  for (int32 i = depth; i < block->stack.vTop; i++) {
    localVarPo var = stackSlot(block, i);
    if (var->kind == inRegister)
      releaseReg(jit, var->Rg);
    var->kind = isConstant;
    var->key = voidIndex;
  }
  block->stack.vTop = depth;
}

armReg popValue(jitBlockPo block) {
  jitCompPo jit = block->jit;
  check(block->stack.vTop>0, "Insufficient stack depth for pop stack");
  localVarPo var = stackSlot(block, 0);
  switch (var->kind) {
    case isLocal: {
      armReg tmp = findFreeReg(jit);
      loadLocal(jit, tmp, var->stkOff);
      block->stack.vTop--;
      return tmp;
    }
    case inStack: {
      armReg tmp = findFreeReg(jit);
      loadLocal(jit, tmp, var->stkOff);
      if (var->stkOff == block->stack.stackDepth)
        block->stack.stackDepth--;
      block->stack.vTop--;
      return tmp;
    }
    case inRegister: {
      armReg tmp = var->Rg;
      block->stack.vTop--;
      return tmp;
    }
    case isConstant: {
      armReg tmp = findFreeReg(jit);
      loadConstant(jit, var->key, tmp);
      block->stack.vTop--;
      return tmp;
    }
    default: {
      bailOut(jit, errorCode);
      return XZR;
    }
  }
}

armReg topValue(jitBlockPo block) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  check(block->stack.vTop>0, "Insufficient stack depth for pop stack");
  localVarPo var = stackSlot(block, 0);
  switch (var->kind) {
    case isLocal:
    case inStack: {
      armReg tmp = findFreeReg(jit);
      loadLocal(jit, tmp, var->stkOff);
      return tmp;
    }
    case inRegister: {
      armReg tmp = findFreeReg(jit);
      armReg rst = var->Rg;
      mov(rst, RG(tmp));
      return tmp;
    }
    case isConstant: {
      armReg tmp = findFreeReg(jit);
      loadConstant(jit, var->key, tmp);
      return tmp;
    }
    default: {
      bailOut(jit, errorCode);
      return XZR;
    }
  }
}

void pushValue(jitBlockPo block, LocalEntry entry) {
  block->stack.vTop++;
  localVarPo var = stackSlot(block, 0);
  *var = entry;
}

void pushBlank(jitBlockPo block) {
  block->stack.vTop++;
  block->stack.stackDepth++;
  localVarPo var = stackSlot(block, 0);
  *var = (LocalEntry){.kind = inStack, .stkOff = -(block->stack.stackDepth + lclCount(block->jit->mtd))};
}

void pushRegister(jitBlockPo block, armReg rg) {
  pushValue(block, (LocalEntry){.kind = inRegister, .Rg = rg});
}

void dropValue(jitBlockPo block) {
  jitCompPo jit = block->jit;
  localVarPo var = stackSlot(block, 0);
  switch (var->kind) {
    case inRegister: {
      releaseReg(jit, var->Rg);
      break;
    }
    default:
      if (var->stkOff == block->stack.stackDepth)
        block->stack.stackDepth--;
  }
  block->stack.vTop--;
}

void dropValues(jitBlockPo block, int32 count) {
  for (int32 i = 0; i < count; i++) {
    dropValue(block);
  }
}

static void spillVar(jitBlockPo block, localVarPo var) {
  jitCompPo jit = block->jit;

  switch (var->kind) {
    case inStack:
    case isLocal:
      return;
    case inRegister: {
      block->stack.stackDepth++;
      int32 stkOff = -trueStackDepth(block);
      storeLocal(jit, var->Rg, stkOff);
      releaseReg(jit, var->Rg);
      var->stkOff = stkOff;
      var->kind = inStack;
      return;
    }
    case isConstant: {
      block->stack.stackDepth++;
      int32 stkOff = -trueStackDepth(block);
      armReg tmp = findFreeReg(jit);
      loadConstant(jit, var->key, tmp);
      storeLocal(jit, tmp, stkOff);
      var->kind = inStack;
      var->stkOff = stkOff;
      releaseReg(jit, tmp);
      return;
    }
    default: {
      bailOut(jit, errorCode);
    }
  }
}

void spillStack(jitBlockPo block) {
  jitCompPo jit = block->jit;

  for (int32 ax = 0; ax < mtdArity(jit->mtd); ax++) {
    localVarPo var = argSlot(block, ax);
    spillVar(block, var);
  }
  for (int32 v = 0; v < lclCount(jit->mtd); v++) {
    localVarPo var = localSlot(block, v);
    spillVar(block, var);
  }

  for (int32 v = 0, stkOff = trueStackDepth(block); v < block->stack.vTop; v++, stkOff--) {
    localVarPo var = stackSlot(block, v);
    spillVar(block, var);
  }
}

void mergeBlockStacks(jitBlockPo parent, jitBlockPo block) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);

  setStackDepth(parent, parent->exitHeight);

  int32 vTop = parent->stack.vTop = min(block->stack.vTop, parent->stack.vTop);
  parent->stack.stackDepth = min(parent->stack.stackDepth, block->stack.stackDepth);
  for (int32 i = 0; i < vTop; i++) {
    localVarPo blockVar = stackSlot(block, i);
    localVarPo parentVar = stackSlot(parent, i);
    switch (blockVar->kind) {
      case isLocal:
      case inStack: {
        switch (parentVar->kind) {
          case isLocal:
          case inStack: {
            if (blockVar->stkOff != parentVar->stkOff) {
              armReg tmp = findFreeReg(jit);
              loadLocal(jit, tmp, blockVar->stkOff);
              storeLocal(jit, tmp, parentVar->stkOff);
              releaseReg(jit, tmp);
            }
            continue;
          }
          case inRegister: {
            loadLocal(jit, parentVar->Rg, blockVar->stkOff);
            continue;
          }
          default: {
            *parentVar = *blockVar;
            continue;
          }
        }
      }
      case inRegister: {
        switch (parentVar->kind) {
          case isLocal:
          case inStack: {
            storeLocal(jit, blockVar->Rg, parentVar->stkOff);
            releaseReg(jit, blockVar->Rg);
            continue;
          }
          case inRegister: {
            if (parentVar->Rg != blockVar->Rg) {
              mov(parentVar->Rg, RG(blockVar->Rg));
              releaseReg(jit, blockVar->Rg);
            }
            continue;
          }
          default: {
            *parentVar = *blockVar;
            continue;
          }
        }
      }
      default: {
        *parentVar = *blockVar;
      }
    }
  }
}

// Put the top arity elements of the stack over caller

void frameOverride(jitBlockPo block, int arity) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);

  armReg tgt = findFreeReg(jit);
  add(tgt, AG, IM(argCount(jit->mtd) * pointerSize));

  for (int32 i = 0; i < arity; i++) {
    localVarPo var = stackSlot(block, i);
    switch (var->kind) {
      case inRegister: {
        armReg vrReg = var->Rg;
        str(vrReg, PRX(tgt, -pointerSize));
        releaseReg(jit, vrReg);
        continue;
      }
      case isLocal:
      case inStack: {
        armReg vrReg = findFreeReg(jit);
        loadLocal(jit, vrReg, var->stkOff);
        str(vrReg, PRX(tgt, -pointerSize));
        releaseReg(jit, vrReg);
        continue;
      }
      case isConstant: {
        armReg vrReg = findFreeReg(jit);
        loadConstant(jit, vrReg, var->key);
        str(vrReg, PRX(tgt, -pointerSize));
        releaseReg(jit, vrReg);
        continue;
      }
      default: {
        bailOut(jit, 47);
      }
    }
  }

  // Release any tied up registers
  for (int32 i = 0; i < block->stack.vTop - arity; i++) {
    localVarPo var = stackSlot(block, i);
    switch (var->kind) {
      case inRegister: {
        releaseReg(jit, var->Rg);
        break;
      }
      default:
        break;
    }
  }

  mov(AG, RG(tgt));

  releaseReg(jit, tgt);
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
