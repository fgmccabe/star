//
// Created by Francis McCabe on 3/20/26.
//

#include "arithP.h"
#include "arm64.h"
#include "constants.h"
#include "formioP.h"
#include "jit.h"
#include "jitP.h"
#include "lowerP.h"
#include "ooio.h"
#include "stackP.h"
#include "codeP.h"
#include "engineP.h"
#include "globals.h"
#include "shuffle.h"
#include "sort.h"

static int32 stashVar(codeGenPo state, int32 pc, localVarPo var, logical moveOwnership);
static void unstashVar(codeGenPo state, int32 pc, localVarPo var);

blockPo targetBlock(blockPo block, int32 tgt, ssaOp blockType) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(block->blockType == blockType);
      return block;
    }
    block = block->parent;
  }
  return Null;
}

codeLblPo breakLabel(blockPo block) {
  return block->breakLbl;
}

codeLblPo loopLabel(blockPo block) {
  return block->loopLbl;
}

void breakOut(codeGenPo state, int32 nextPc, blockPo tgt) {
  if (tgt->endPc != nextPc) {
    assemCtxPo ctx = assemCtx(state->jit);
    b(breakLabel(tgt));
  }
}

retCode getIntVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  asr(rg, rg, IM(2));
  return Ok;
}

retCode mkIntVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  lsl(rg, rg, IM(2));
  orr(rg, rg, IM(intTg));
  return Ok;
}

void getFltVal(jitCompPo jit, armReg rg, fpReg tgt) {
  assemCtxPo ctx = assemCtx(jit);

  fldr(tgt, OF(rg, OffsetOf(FloatRecord, dx)));
}

armReg findARegister(codeGenPo state, int32 pc) {
  armReg tmp = findFreeReg(state->jit);
  if (tmp == XZR)
    syserr("Not enough free registers");

  return tmp;
}

registerMap criticalRegs() { return 1u << CO | 1u << PR; }

registerMap defaultArgRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7;
}

registerMap lambdaArgRegs() {
  return dropReg(defaultArgRegs(), X0);
}

registerMap systemArgRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7;
}

int32 maxArgRegister = 7;

void loadRegister(codeGenPo state, armReg rg, FlexOp src) {
  assemCtxPo ctx = state->jit->assemCtx;
  if (isRegisterOp(src))
    mov(rg, src);
  else
    ldr(rg, src);
}

logical liveVar(localVarPo var, int32 pc) {
  return var->live && var->desc->end > pc;
}

int32 stashLiveLocals(codeGenPo state, int32 pc, logical moveOwnership) {
  int32 minOffset = 0;
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc)) {
      minOffset = min(minOffset, stashVar(state, pc, var, moveOwnership));
    }
  }
  return minOffset;
}

registerMap registerLocals(codeGenPo state, int32 pc) {
  registerMap map = emptyRegSet();

  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && var->inited && isRegisterOp(var->src)) {
      map = addReg(map, var->src.reg);
    }
  }
  return map;
}

logical allLocalsStashed(codeGenPo state, int32 pc) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && var->inited && !var->stashed) {
      return False;
    }
  }
  return True;
}

void showLiveLocals(ioPo out, codeGenPo state) {
  outMsg(out, "Live locals:\n");
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->live) {
      outMsg(out, "%V\n%_", lcl);
    }
  }
}

static void voidOutSlot(codeGenPo state, int32 pc, int32 offset, armReg vdCon, logical *loaded) {
  for (int32 vx = 0; vx < state->numLocals; vx++) {
    localVarPo var = &state->locals[vx];
    if (liveVar(var, pc) && var->stashed && var->stkOff == offset)
      return; // Because C does not have labeled breaks
  }
  if (!*loaded) {
    loadConstant(state->jit, voidIndex, vdCon);
    *loaded = True;
  }
  storeFlex(state, pc, RG(vdCon),OF(AG, offset*pointerSize));
}

void storeFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt) {
  if (!sameFlexOp(src, tgt)) {
    assemCtxPo ctx = assemCtx(state->jit);
    if (isRegisterOp(tgt)) {
      loadRegister(state, tgt.reg, src);
    } else if (isRegisterOp(src)) {
      move(ctx, tgt, src, state->jit->freeRegs);
    } else {
      armReg tmp = findFreeReg(state->jit);
      loadRegister(state, tmp, src);
      move(ctx, tgt,RG(tmp), state->jit->freeRegs);
      releaseReg(state->jit, tmp);
    }
  }
}

void loadFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt) {
  assemCtxPo ctx = assemCtx(state->jit);
  if (isRegisterOp(tgt))
    loadRegister(state, tgt.reg, src);
  else if (isRegisterOp(src))
    move(ctx, tgt, src, state->jit->freeRegs);
  else {
    armReg tmp = findFreeReg(state->jit);
    loadRegister(state, tmp, src);
    move(ctx, tgt,RG(tmp), state->jit->freeRegs);
    releaseReg(state->jit, tmp);
  }
}

FlexOp constantFlex(int32 index) {
  return OF(CO, index*pointerSize);
}

FlexOp varFlex(int32 index) {
  return OF(AG, index*pointerSize);
}

void voidOutFrameLocals(codeGenPo state, int32 pc, int32 minOffset, armReg vdRg) {
  logical loaded = False;
  for (int32 ix = -1; ix > minOffset; ix--) {
    voidOutSlot(state, pc, ix, vdRg, &loaded);
  }
}

void argMove(assemCtxPo ctx, FlexOp dst, FlexOp src, registerMap *freeRegs) {
  move(ctx, dst, src, *freeRegs);
}

void invokeIntrinsic(codeGenPo state, int32 pc, int32 livePc, runtimeFn fn, int32 arity, FlexOp args[],
                     logical moveOwnership,
                     int32 rsCnt, FlexOp results[]) {
  assemCtxPo ctx = assemCtx(state->jit);

  int32 lastSlot = stashLiveLocals(state, livePc, moveOwnership);
  voidOutFrameLocals(state, pc, lastSlot, X16); // void out gaps in the locals map

  ArgSpec operands[arity];

  registerMap argRegs = systemArgRegs();
  registerMap saveMap = criticalRegs();

  for (int32 ix = 0; ix < arity; ix++) {
    armReg ax = nxtAvailReg(argRegs);
    argRegs = dropReg(argRegs, ax);
    assert(ax!=XZR);
    operands[ix] = (ArgSpec){
      .src = args[ix], .dst = RG(ax), .mark = True, .group = -1
    };
  }
  registerMap tmpMap = fixedRegSet(X16);

  shuffleVars(ctx, operands, arity, &tmpMap, argMove);

  stashEngineState(state->jit, -lastSlot, argRegs);
  saveRegisters(ctx, saveMap);
  mov(X16, IM((integer) fn));
  blr(X16);
  restoreRegisters(ctx, saveMap);
  unstashEngineState(state->jit);

  assert(rsCnt >=0 && rsCnt<=2);

  argRegs = systemArgRegs();
  for (int32 ix = 0; ix < rsCnt; ix++) {
    armReg ax = nxtAvailReg(argRegs);
    argRegs = dropReg(argRegs, ax);
    assert(ax!=XZR);
    operands[ix] = argSpec(RG(ax), results[ax]);
  }
  shuffleVars(ctx, operands, rsCnt, &tmpMap, argMove);
  restoreStashedLocals(state, livePc);
}

retCode jitError(jitCompPo jit, char *msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args; /* access the generic arguments */
  va_start(args, msg); /* start the variable argument sequence */

  __voutMsg(O_IO(f), msg, args); /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0); /* Terminate the string */

  closeIo(O_IO(f));

  strMsg(jit->errMsg, NumberOf(jit->errMsg), RED_ESC_ON "%s"RED_ESC_OFF, buff);
  return Error;
}

void bailOut(codeGenPo state, int32 pc, ExitCode code) {
  assemCtxPo ctx = assemCtx(state->jit);
  mov(X17, IM((integer)star_exit));
  mov(X0, RG(PR));
  mov(X1, IM(code));
  br(X17);

  /* char buff[MAXLINE]; */
  /* strBufferPo f = fixedStringBuffer(buff, NumberOf(buff)); */

  /* va_list args;      /\* access the generic arguments *\/ */
  /* va_start(args, msg);    /\* start the variable argument sequence *\/ */

  /* __voutMsg(O_IO(f), msg, args);  /\* Display into the string buffer *\/ */

  /* va_end(args); */
  /* outByte(O_IO(f), 0);                /\* Terminate the string *\/ */

  /* closeIo(O_IO(f)); */

  /* assemCtxPo ctx = assemCtx(jit);   */
  /* integer conString = stringConstant(ctx,buff,uniStrlen(buff)); */
  /* ldr(X0,IM(conString)); */

  /* return callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(conString)); */
}

void loadElement(jitCompPo jit, armReg tgt, armReg base, int32 ix) {
  assemCtxPo ctx = assemCtx(jit);
  int32 offset = ix * pointerSize;
  if (is9bit(offset))
    ldur(tgt, base, offset);
  else {
    mov(tgt, IM(ix));
    ldr(tgt, EX2(base, tgt, U_XTX, 3));
  }
}

void storeElement(jitCompPo jit, armReg src, armReg base, int32 ix) {
  assemCtxPo ctx = assemCtx(jit);
  int32 offset = ix * pointerSize;
  if (is9bit(offset))
    stur(src, base, offset);
  else {
    armReg tmp = findFreeReg(jit);
    mov(tmp, IM(ix));
    str(src, EX2(base, tmp, U_XTX, 3));
    releaseReg(jit, tmp);
  }
}

void loadConstant(jitCompPo jit, int32 key, armReg tgt) {
  assemCtxPo ctx = assemCtx(jit);
  termPo lit = getConstant(key);

  if (isSmall(lit))
    mov(tgt, IM((integer) lit));
  else
    loadElement(jit, tgt, CO, key);
}

void stackCheck(codeGenPo state, int32 pc, int32 argCnt, int32 lclCnt) {
  methodPo mtd = state->mtd;
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = newLabel(ctx);
  int32 delta = (argCnt + lclCnt + (int32) (FrameCellCount + FrameCellCount)) * pointerSize;
  armReg tmp = findFreeReg(jit);

  if (is16bit(delta))
    sub(tmp, AG, IM(delta));
  else {
    mov(tmp, IM(delta));
    sub(tmp, AG, RG(tmp));
  }
  cmp(tmp, RG(FP));
  bhi(okLbl);

  mov(tmp, IM((integer) mtd));
  str(tmp, OF(STK, OffsetOf(StackRecord, prog)));
  adr(tmp, okLbl);
  str(tmp, OF(STK, OffsetOf(StackRecord, pc)));

  invokeIntrinsic(state, pc, pc + 1, (runtimeFn) handleStackOverflow,
                  4, (FlexOp[]){RG(PR), IM(True), IM(delta), IM(mtdArity(mtd))}, False, 0, (FlexOp[]){});
  bind(okLbl);
  releaseReg(jit, tmp);
}

retCode showStackSlot(ioPo out, void *data, long depth, long precision, logical alt) {
  localVarPo var = (localVarPo) data;

  return outMsg(out, "%s[%d]%s", (var->stkOff < 0 ? "L" : "A"), var->stkOff, (var->inited ? "*" : ""));
}

void stashEngineState(jitCompPo jit, int32 stackLevel, registerMap freeRegs) {
  assert(stackLevel>=0);
  assemCtxPo ctx = assemCtx(jit);
  str(AG, OF(STK, OffsetOf(StackRecord, args)));
  armReg currSP = nxtAvailReg(freeRegs);
  if (stackLevel != 0) {
    sub(currSP, AG, IM(stackLevel*pointerSize));
    str(currSP, OF(STK, OffsetOf(StackRecord, sp)));
  } else
    str(AG, OF(STK, OffsetOf(StackRecord, sp)));
  str(FP, OF(STK, OffsetOf(StackRecord, fp)));
}

void unstashEngineState(jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  ldr(STK, OF(PR, OffsetOf(EngineRecord, stk)));
  ldr(AG, OF(STK, OffsetOf(StackRecord, args)));
  ldr(FP, OF(STK, OffsetOf(StackRecord, fp)));
}

int32 stashVar(codeGenPo state, int32 pc, localVarPo var, logical moveOwnership) {
  if (var->inited) {
    if (!var->stashed) {
      if (isRegisterOp(var->src)) {
        var->stkOff = (var->desc->kind == argument ? var->desc->varNo : nextStkOff(state, pc));
        FlexOp lclFlex = varFlex(var->stkOff);
        storeFlex(state, pc, var->src, lclFlex);
        if (moveOwnership || !var->desc->registerCandidate) {
          var->src = lclFlex;
          releaseReg(state->jit, var->src.reg);
        }
        var->stashed = True;
        return var->stkOff;
      }
    }
    return var->stkOff;
  }
  return 0;
}

void unstashVar(codeGenPo state, int32 pc, localVarPo var) {
  assemCtxPo ctx = assemCtx(state->jit);
  if (liveVar(var, pc) && var->inited && var->stashed) {
    if (isRegisterOp(var->src)) {
      reserveReg(state->jit, var->src.reg);
      ldr(var->src.reg, varFlex(var->stkOff));
      var->stashed = False;
    }
  }
}

void restoreStashedLocals(codeGenPo state, int32 pc) {
  assemCtxPo ctx = assemCtx(state->jit);
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && var->inited && var->stashed) {
      if (isRegisterOp(var->src)) {
        ldr(var->src.reg, varFlex(var->stkOff));
        var->stashed = False;
      }
    }
  }
}

retCode showLocalVar(ioPo out, void *data, long depth, long precision, logical alt) {
  localVarPo var = (localVarPo) data;
  varDescPo desc = var->desc;
  outMsg(out, "%s[%d]%s [%d,%d)", varKindName(desc->kind), var->desc->varNo,
         (var->inited ? (var->stashed ? "S" : "") : "u"),
         desc->start, desc->end);
  if (var->inited) {
    outMsg(out, " @ %F",&var->src);
  }
  return Ok;
}

int32 nextStkOff(codeGenPo state, int32 pc) {
  int32 lastSlot = 0;

  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (liveVar(lcl, pc)) {
      if ((lcl->stashed || !isRegisterOp(lcl->src)) && lcl->inited && lcl->stkOff < lastSlot)
        lastSlot = lcl->stkOff;
    }
  }

  return lastSlot - 1;
}

localVarPo findSpareLocal(codeGenPo state, int32 pc) {
  for (int32 lx = 0; lx < state->numLocals; lx++) {
    localVarPo slot = &state->locals[lx];
    if (!liveVar(slot, pc)) {
      return slot;
    }
  }
  return Null;
}

FlexOp getLclSrc(codeGenPo state, int32 pc, int32 lclNo) {
  localVarPo lcl = localSource(state, pc, lclNo);
  assert(lcl!=Null && liveVar(lcl,pc));
  return lcl->src;
}
