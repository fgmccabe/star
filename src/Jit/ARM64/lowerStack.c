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
#include "shuffle.h"

// We need these registers preserved at all costs

registerMap criticalRegs() {
  return 1u << CO | 1u << PR;
}

localVarPo stackSlot(valueStackPo stack, int32 slot) {
  return &stack->local[stack->stackPnt - stack->vTop + slot];
}

localVarPo localSlot(valueStackPo stack, int32 slot) {
  return &stack->local[stack->argPnt - slot];
}

localVarPo argSlot(valueStackPo stack, int32 slot) {
  return &stack->local[stack->argPnt + slot];
}

int32 trueStackDepth(valueStackPo stack) {
  return stack->argPnt - stack->stackPnt + stack->vTop;
}

armReg popValue(valueStackPo stack, jitCompPo jit) {
  check(stack->vTop>0, "Insufficient stack depth for pop stack");
  localVarPo var = stackSlot(stack, 0);
  switch (var->kind) {
    case isLocal: {
      armReg tmp = findFreeReg(jit);
      loadLocal(jit, tmp, var->stkOff);
      stack->vTop--;
      return tmp;
    }
    case inStack: {
      armReg tmp = findFreeReg(jit);
      loadLocal(jit, tmp, var->stkOff);
      stack->vTop--;
      return tmp;
    }
    case inRegister: {
      armReg tmp = var->Rg;
      stack->vTop--;
      return tmp;
    }
    case isConstant: {
      armReg tmp = findFreeReg(jit);
      loadConstant(jit, var->key, tmp);
      stack->vTop--;
      return tmp;
    }
    default: {
      bailOut(jit, errorCode);
      return XZR;
    }
  }
}

armReg topValue(valueStackPo stack, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  check(stack->vTop>0, "Insufficient stack depth for pop stack");
  localVarPo var = stackSlot(stack, 0);
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
      mov(tmp, RG(rst));
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

void setLocal(valueStackPo stack, int32 lclNo, LocalEntry entry) {
  *localSlot(stack, lclNo) = entry;
}

void pushValue(valueStackPo stack, LocalEntry entry) {
  stack->vTop++;
  localVarPo var = stackSlot(stack, 0);
  *var = entry;
}

void pushBlank(valueStackPo stack) {
  stack->vTop++;
  localVarPo var = stackSlot(stack, 0);
  *var = (LocalEntry){.kind = inStack, .stkOff = -trueStackDepth(stack)};
}

void pushRegister(valueStackPo stack, armReg rg) {
  pushValue(stack, (LocalEntry){.kind = inRegister, .Rg = rg});
}

void setStackDepth(valueStackPo stack, jitCompPo jit, int32 depth) {
  assert(depth<=stack->vTop);
  while (stack->vTop > depth)
    dropValue(stack, jit);
}

void dropValue(valueStackPo stack, jitCompPo jit) {
  localVarPo var = stackSlot(stack, 0);
  switch (var->kind) {
    case inRegister: {
      releaseReg(jit, var->Rg);
      break;
    }
    default: ;
  }
  stack->vTop--;
}

void dropValues(valueStackPo stack, jitCompPo jit, int32 count) {
  for (int32 i = 0; i < count; i++) {
    dropValue(stack, jit);
  }
}

static void spillVar(jitCompPo jit, valueStackPo stack, localVarPo var, int32 stkOff) {
  switch (var->kind) {
    case inStack:
    case isLocal: {
      if (var->stkOff != stkOff) {
        armReg tmp = findFreeReg(jit);
        loadLocal(jit, tmp, var->stkOff);
        storeLocal(jit, stkOff, tmp);
        var->stkOff = stkOff;
      }
      return;
    }
    case inRegister: {
      storeLocal(jit, var->Rg, stkOff);
      releaseReg(jit, var->Rg);
      var->stkOff = stkOff;
      var->kind = inStack;
      return;
    }
    case isConstant: {
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

void spillLocals(valueStackPo stack, jitCompPo jit) {
  for (int32 ax = 0; ax < mtdArity(jit->mtd); ax++) {
    localVarPo var = argSlot(stack, ax);
    spillVar(jit, stack, var, ax);
  }
  for (int32 v = 1; v <= lclCount(jit->mtd); v++) {
    localVarPo var = localSlot(stack, v);
    spillVar(jit, stack, var, -v);
  }
}

retCode setupLocals(localVarPo stack, argSpecPo newArgs, int32 count, int32 tgtOff) {
  for (int32 ix = 0; ix < count; ix++) {
    localVarPo var = &stack[ix];

    switch (var->kind) {
      case inRegister: {
        newArgs[ix] = (ArgSpec){.src = RG(var->Rg), .dst = OF(AG, tgtOff*pointerSize), .mark = True, .group = -1};
        break;
      }
      case isLocal:
      case inStack: {
        newArgs[ix] = (ArgSpec){
          .src = OF(AG, var->stkOff*pointerSize), .dst = OF(AG, tgtOff*pointerSize), .mark = True, .group = -1
        };
        break;
      }
      case isConstant: {
        newArgs[ix] = (ArgSpec){
          .src = OF(CO, var->key*pointerSize), .dst = OF(AG, tgtOff*pointerSize), .mark = True, .group = -1
        };
        break;
      }
      default: {
        return Error;
      }
    }
    var->kind = isLocal;
    var->stkOff = tgtOff;
    tgtOff++;
  }
  return Ok;
}

void spillStack(valueStackPo stack, jitCompPo jit) {
  int32 size = stack->vTop + jit->arity + jit->lclCnt;

  ArgSpec newArgs[size];
  setupLocals(&stack->local[stack->stackPnt - stack->vTop], &newArgs[0], size, -(jit->lclCnt + stack->vTop));

  shuffleVars(jit->assemCtx, newArgs, size, jit->freeRegs);
}

// Put the top arity elements of the stack over caller

void frameOverride(jitBlockPo block, int arity) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  valueStackPo stack = &block->stack;

  int32 tgtOff = argCount(jit->mtd);

  ArgSpec newArgs[arity];
  for (int32 ax = arity - 1; ax >= 0; ax--) {
    localVarPo var = stackSlot(stack, ax);
    switch (var->kind) {
      case inRegister: {
        tgtOff--;
        newArgs[ax] = (ArgSpec){.src = RG(var->Rg), .dst = OF(AG, tgtOff*pointerSize), .mark = True, .group = -1};
        continue;
      }
      case isLocal:
      case inStack: {
        tgtOff--;
        newArgs[ax] = (ArgSpec){
          .src = OF(AG, var->stkOff*pointerSize), .dst = OF(AG, tgtOff*pointerSize), .mark = True, .group = -1
        };
        continue;
      }
      case isConstant: {
        tgtOff--;
        newArgs[ax] = (ArgSpec){
          .src = OF(CO, var->key*pointerSize), .dst = OF(AG, tgtOff*pointerSize), .mark = True, .group = -1
        };
        continue;
      }
      default: {
        bailOut(jit, 47);
      }
    }
  }

  shuffleVars(ctx, newArgs, arity, jit->freeRegs);

  if (tgtOff > 0) {
    int32 delta = tgtOff * pointerSize;
    if (is12bit(delta))
      add(AG, AG, IM(delta));
    else {
      armReg tmp = findFreeReg(jit);
      mov(tmp, IM(delta));
      add(AG, AG, RG(tmp));
      releaseReg(jit, tmp);
    }
  } else {
    int32 delta = -tgtOff * pointerSize;
    if (is12bit(delta))
      sub(AG, AG, IM(delta));
    else {
      armReg tmp = findFreeReg(jit);
      mov(tmp, IM(delta));
      sub(AG, AG, RG(tmp));
      releaseReg(jit, tmp);
    }
  }

  // Release any tied up registers
  for (int32 i = 0; i < stack->vTop - arity; i++) {
    localVarPo var = stackSlot(stack, i);
    switch (var->kind) {
      case inRegister: {
        releaseReg(jit, var->Rg);
        break;
      }
      default:
        break;
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

static void dumpSlot(ioPo out, localVarPo var) {
  switch (var->kind) {
    case inRegister:
      outMsg(out, "register X%d", var->Rg);
      return;
    case inStack:
      outMsg(out, "on stack %d", var->stkOff);
      return;
    case isLocal:
      outMsg(out, "local vr %d", var->stkOff);
      return;
    case isConstant: {
      outMsg(out, "constant %T", getConstant(var->key));
      return;
    }
    default:
      outMsg(out, "unknown type of slot");
  }
}

void dumpStack(valueStackPo stack) {
  outMsg(logFile, "Stack: top=%d, arity=%d, locals=%d\n", stack->vTop,
         NumberOf(stack->local) - stack->argPnt, stack->argPnt - stack->stackPnt);

  // for (int ax = 0; ax + stack->argPnt < NumberOf(stack->local); ax++) {
  //   localVarPo var = argSlot(stack, ax);
  //   outMsg(logFile, "arg %d ",ax);
  //   dumpSlot(logFile, var);
  //   outStr(logFile, "\n");
  // }
  //
  // for (int lx=1;lx<=stack->argPnt-stack->stackPnt;lx++) {
  //   localVarPo var = localSlot(stack, lx);
  //   outMsg(logFile, "lcl %d ",lx);
  //   dumpSlot(logFile, var);
  //   outStr(logFile, "\n");
  // }

  for (int sx = 0; sx < stack->vTop; sx++) {
    localVarPo var = stackSlot(stack, sx);
    outMsg(logFile, "stk %d ", sx);
    dumpSlot(logFile, var);
    outStr(logFile, "\n");
  }
  flushOut();
}

#define combineKind(S,K) ((S)<<3|(K))

void propagateVar(jitCompPo jit, localVarPo src, localVarPo dst) {
  assemCtxPo ctx = assemCtx(jit);

  switch (combineKind(src->kind, dst->kind)) {
    case combineKind(inStack, inStack):
    case combineKind(inStack, isLocal):
    case combineKind(isLocal, inStack):
    case combineKind(isLocal, isLocal): {
      if (src->stkOff != dst->stkOff) {
        armReg tmp = findFreeReg(jit);
        loadLocal(jit, tmp, src->stkOff);
        storeLocal(jit, dst->stkOff, tmp);
        releaseReg(jit, tmp);
      }
      return;
    }
    case combineKind(inRegister, isLocal):
    case combineKind(inRegister, inStack): {
      storeLocal(jit, src->Rg, dst->stkOff);
      releaseReg(jit, src->Rg);
      return;
    }
    case combineKind(inRegister, inRegister): {
      if (src->Rg != dst->Rg) {
        mov(dst->Rg, RG(src->Rg));
        releaseReg(jit, src->Rg);
      }
      return;
    }
    case combineKind(isConstant, inRegister): {
      loadConstant(jit, src->key, dst->Rg);
      return;
    }

    case combineKind(isConstant, inStack):
    case combineKind(isConstant, isLocal): {
      armReg tmp = findFreeReg(jit);
      loadConstant(jit, src->key, tmp);
      storeLocal(jit, tmp, dst->stkOff);
      releaseReg(jit, tmp);
      return;
    }

    case combineKind(isLocal, isConstant): {
      armReg tmp = findFreeReg(jit);
      loadLocal(jit, tmp, src->stkOff);
      storeLocal(jit, dst->stkOff, tmp);
      releaseReg(jit, tmp);
      dst->kind = isLocal;
      return;
    }

    default: {
      bailOut(jit, errorCode);
    }
  }
}

retCode propagateStack(jitCompPo jit, valueStackPo srcStack, valueStackPo tgtStack, int32 tgtHeight) {
  if (!tgtStack->propagated) {
    tgtStack->propagated = True;
    tgtStack->vTop = tgtHeight;

    // Should be a nop at the moment.
    for (int32 ax = 0; ax < mtdArity(jit->mtd); ax++) {
      localVarPo src = argSlot(srcStack, ax);
      localVarPo dst = argSlot(tgtStack, ax);
      *dst = *src;
    }

    for (int32 v = 1; v <= srcStack->argPnt - srcStack->stackPnt; v++) {
      localVarPo src = localSlot(srcStack, v);
      localVarPo dst = localSlot(tgtStack, v);
      *dst = *src;
    }

    for (int32 v = tgtHeight; v > 0; v--) {
      localVarPo src = stackSlot(srcStack, v);
      localVarPo dst = stackSlot(tgtStack, v);
      *dst = *src;
    }

    for (int32 v = tgtHeight; v > tgtHeight; v--) {
      localVarPo src = stackSlot(srcStack, v - 1);
      pushValue(tgtStack, *src);
    }
  } else {
    check(srcStack->vTop >= tgtStack->vTop, "Stack depth mismatch");
    for (int32 v = 0; v < tgtStack->vTop; v++) {
      localVarPo src = localSlot(srcStack, v);
      localVarPo dst = localSlot(tgtStack, v);
      *dst = *src;
    }
  }
  return Ok;
}
