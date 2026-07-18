//
// Created by Francis McCabe on 3/20/26.
//

#include "arithP.h"
#include "constants.h"
#include "formioP.h"
#include "jit.h"
#include "jitP.h"
#include "lowerP.h"
#include "ooio.h"
#include "stackP.h"
#include "codeP.h"
#include "debugP.h"
#include "engineP.h"
#include "shuffle.h"
#include "sort.h"

static int32 stashVar(codeGenPo state, int32 pc, localVarPo var, logical moveOwnership);
static localVarPo findLclByOffset(codeGenPo state, int32 pc, int32 offset);

blockPo targetBlock(blockPo block, int32 tgt, ssaOp blockType) {
  while (block != Null) {
    if (block->startPc == tgt) {
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

retCode getIntVal(jitCompPo jit, mcRegister rg) {
  assemCtxPo ctx = assemCtx(jit);
  sar(RG(rg), IM(2));
  return Ok;
}

retCode mkIntVal(jitCompPo jit, mcRegister rg) {
  assemCtxPo ctx = assemCtx(jit);
  shl(RG(rg), IM(2));
  or(RG(rg), IM(intTg));
  return Ok;
}

void getFltVal(jitCompPo jit, mcRegister rg, fpReg tgt) {
  assemCtxPo ctx = assemCtx(jit);
  movsd(FLT(tgt), BS(rg, OffsetOf(FloatRecord, dx)));
}

mcRegister findMcRegister(codeGenPo state, int32 pc) {
  mcRegister tmp = findFreeReg(state->jit);
  if (tmp == XZR)
    syserr("Not enough free registers");

  return tmp;
}

registerMap criticalRegs() { return 1u << CO | 1u << PR; }

registerMap defaultArgRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5;
}

registerMap lambdaArgRegs() {
  return dropReg(defaultArgRegs(), X0);
}

registerMap systemArgRegs() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5;
}

static mcRegister argRegs[] = {X0, X1, X2, X3, X4, X5};

mcRegister nxtAvailArgReg(registerMap from) {
  for (uint32 ix = 0; ix < NumberOf(argRegs); ix++) {
    mcRegister rg = argRegs[ix];
    if (isRegInMap(from, rg)) {
      return rg;
    }
  }
  return XZR;
}

int32 maxArgRegister = 5;

void loadRegister(codeGenPo state, mcRegister rg, FlexOp src) {
  assemCtxPo ctx = state->jit->assemCtx;
  if (isRegisterOp(src))
    mov(RG(rg), src);
  else if (src.mode == sOff) {
    load(ctx, rg, src.op.based.base, src.op.based.disp);
  }
  else
    mov(RG(rg), src);
}

logical liveVar(localVarPo var, int32 pc) {
  return var->inUse && var->desc->start <= pc && var->desc->end > pc;
}

static int32 varOffset(localVarPo var, codeGenPo state, int32 pc) {
  assert(liveVar(var, pc) && var->inited);

  if (isRegisterOp(var->src)) {
    varDescPo desc = var->desc;
    return var->stkOff = desc->varNo >= 0 ? desc->varNo : nextStkOff(state, pc);
  }
  else
    return var->stkOff;
}

int32 stashVar(codeGenPo state, int32 pc, localVarPo var, logical moveOwnership) {
  if (liveVar(var, pc) && var->inited) {
    if (!var->stashed) {
      if (isRegisterOp(var->src)) {
        FlexOp lclFlex = varFlex(varOffset(var, state, pc));
        storeFlex(state, pc, var->src, lclFlex);
        releaseReg(state->jit, var->src.op.reg);
        if (moveOwnership) {
          var->src = lclFlex;
        }
        var->stashed = True;
        return var->stkOff;
      }
    }
    return var->stkOff;
  }
  return 0;
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
      map = addReg(map, var->src.op.reg);
    }
  }
  return map;
}

logical allStashed(codeGenPo state, int32 pc) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && var->inited) {
      if (isRegisterOp(var->src) && !var->stashed)
        return False;
    }
  }
  return True;
}

void showLocalSlotMap(ioPo out, codeGenPo state, int32 pc) {
  int32 minSlot = -lclCount(state->mtd);
  for (int32 lx = -1; lx >= minSlot; lx--) {
    localVarPo var = findLclByOffset(state, pc, lx);
    if (var == Null)
      outChar(out, '*');
    else if (var->inited)
      outMsg(out, "(%d)I", var->desc->varNo);
    else
      outMsg(out, "(%d)U", var->desc->varNo);
  }
  outMsg(out, "\n%_");
}

static void showLiveLocals(ioPo out, codeGenPo state, int32 pc) {
  outMsg(out, "Live locals:\n");
  // showLocalSlotMap(out, state, pc);
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo lcl = &state->locals[ix];
    if (lcl->inUse) {
      outMsg(out, "%V\n%_", lcl);
    }
  }
}

void dumpState(codeGenPo state, int32 pc) {
  showLiveLocals(logFile, state, pc);
  dRegisterMap(state->jit->freeRegs);
}

void storeFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt) {
  if (!sameFlexOp(src, tgt)) {
    assemCtxPo ctx = assemCtx(state->jit);
    if (isRegisterOp(tgt)) {
      loadRegister(state, tgt.op.reg, src);
    }
    else if (isRegisterOp(src)) {
      move(ctx, tgt, src, state->jit->freeRegs);
    }
    else {
      mcRegister tmp = findFreeReg(state->jit);
      loadRegister(state, tmp, src);
      move(ctx, tgt,RG(tmp), state->jit->freeRegs);
      releaseReg(state->jit, tmp);
    }
  }
}

void loadFlex(codeGenPo state, int32 pc, FlexOp src, FlexOp tgt) {
  assemCtxPo ctx = assemCtx(state->jit);
  if (isRegisterOp(tgt))
    loadRegister(state, tgt.op.reg, src);
  else if (isRegisterOp(src))
    move(ctx, tgt, src, state->jit->freeRegs);
  else {
    mcRegister tmp = findFreeReg(state->jit);
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

static void voidOutSlot(codeGenPo state, int32 pc, int32 offset) {
  for (int32 vx = 0; vx < state->numLocals; vx++) {
    localVarPo var = &state->locals[vx];
    if (liveVar(var, pc) && var->stashed && var->stkOff == offset)
      return; // Because C does not have labeled breaks
  }
  assert(state->argMark+offset>=0 && state->argMark+offset<state->numLocals);
  if (!state->voided[state->argMark + offset]) {
    storeFlex(state, pc, IM(0),OF(AG, offset*pointerSize));
    state->voided[state->argMark + offset] = True;
  }
}

void voidOutFrameLocals(codeGenPo state, int32 pc, int32 minOffset) {
  for (int32 ix = -1; ix > minOffset; ix--) {
    voidOutSlot(state, pc, ix);
  }
  for (int32 ix = 0; ix < mtdArity(state->mtd); ix++)
    voidOutSlot(state, pc, ix);
  for (int32 ix = 0; ix < state->argMark + minOffset; ix++) {
    state->voided[ix] = False;
  }
}

int32 flushArg(codeGenPo state, int32 pc, localVarPo var, void* cl) {
  if (liveVar(var, pc)) {
    if (var->inited) {
      int32 vOffset = var->stkOff = var->desc->varNo;
      if (isRegisterOp(var->src) && !var->desc->registerCandidate) {
        FlexOp lclFlex = varFlex(vOffset);
        storeFlex(state, pc, var->src, lclFlex);
        releaseReg(state->jit, var->src.op.reg);
        var->src = lclFlex;
        var->stashed = True;
        return vOffset;
      }
    }
  }
  return 0;
}

int32 flushArguments(codeGenPo state, int32 pc) {
  int32 minOffset = processLocals(state, pc, flushArg, Null);
  voidOutFrameLocals(state, pc, minOffset);
  return minOffset;
}

int32 processLocals(codeGenPo state, int32 pc, localVarProc vProc, void* cl) {
  int32 minOffset = 0;

  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc)) {
      minOffset = min(minOffset, vProc(state, pc, var, cl));
    }
  }
  return minOffset;
}

static localVarPo findLclByOffset(codeGenPo state, int32 pc, int32 offset) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && !isRegisterOp(var->src) && var->stkOff == offset) {
      return var;
    }
  }
  return Null;
}

void invokeIntrinsic(codeGenPo state, int32 pc, int32 livePc, runtimeFn fn, int32 arity, FlexOp args[],
                     logical moveOwnership,
                     int32 rsCnt, FlexOp results[]) {
  assemCtxPo ctx = assemCtx(state->jit);

  int32 lastSlot = stashLiveLocals(state, livePc, moveOwnership);

  ArgSpec operands[arity];

  registerMap argRegs = systemArgRegs();

  for (int32 ix = 0; ix < arity; ix++) {
    mcRegister ax = nxtAvailArgReg(argRegs);
    argRegs = dropReg(argRegs, ax);
    assert(ax!=XZR);
    operands[ix] = (ArgSpec){
      .src = args[ix], .dst = RG(ax), .mark = True, .group = -1
    };
  }
  registerMap tmpMap = fixedRegSet(X16);

  shuffleVars(state->jit, operands, arity, &tmpMap);
  voidOutFrameLocals(state, pc, lastSlot); // void out gaps in the locals map

  assert(allStashed(state,livePc));

  stashEngineState(state->jit, lastSlot, argRegs);
  registerMap saveMap = criticalRegs();
  saveRegisters(ctx, saveMap);
  mov(RG(X16), IM((integer) fn));
  call(RG(X16));
  restoreRegisters(ctx, saveMap);
  unstashEngineState(state->jit);

  assert(rsCnt >= 0 && rsCnt <= 2);

  ArgSpec rsltOps[rsCnt];
  if (rsCnt > 0) {
    rsltOps[0] = argSpec(RG(RTS), results[0]);
    if (rsCnt > 1) {
      rsltOps[1] = argSpec(RG(RTV), results[1]);
    }
  }
  shuffleVars(state->jit, rsltOps, rsCnt, &tmpMap);
  restoreStashedLocals(state, livePc);
}

retCode jitError(jitCompPo jit, char* msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args;        /* access the generic arguments */
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
  mov(RG(X16), IM((integer)star_exit));
  mov(RG(X0), IM(code));
  br(X16);

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

void loadElement(jitCompPo jit, mcRegister tgt, mcRegister base, int32 ix) {
  assemCtxPo ctx = assemCtx(jit);
  mov(RG(tgt), OF(base, ix * pointerSize));
}

void storeElement(jitCompPo jit, mcRegister src, mcRegister base, int32 ix) {
  assemCtxPo ctx = assemCtx(jit);
  mov(OF(base, ix * pointerSize), RG(src));
}

void loadConstant(jitCompPo jit, int32 key, mcRegister tgt) {
  assemCtxPo ctx = assemCtx(jit);
  termPo lit = getConstant(key);

  if (isSmall(lit))
    mov(RG(tgt), IM((integer) lit));
  else
    loadElement(jit, tgt, CO, key);
}

int32 stashArg(codeGenPo state, int32 pc, localVarPo var) {
  var->stkOff = nextStkOff(state, pc);
  var->stashed = True;
  storeFlex(state, pc, var->src, varFlex(var->stkOff));
  return var->stkOff;
}

int32 stashArgs(codeGenPo state, int32 pc) {
  int32 minOffset = 0;
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc)) {
      if (isRegisterOp(var->src))
        minOffset = min(minOffset, stashArg(state, pc, var));
      else
        minOffset = min(minOffset, var->stkOff);
    }
  }
  return minOffset;
}

void restoreArgs(codeGenPo state, int32 pc) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && var->stashed && isRegisterOp(var->src)) {
      loadFlex(state, pc, varFlex(var->stkOff), var->src);
      var->stashed = False;
    }
  }
}

void stackCheck(codeGenPo state, int32 pc, int32 arity, int32 lcls) {
  jitCompPo jit = state->jit;
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = newLabel(ctx);
  int32 delta = (arity + lcls + (int32)(FrameCellCount + FrameCellCount)) * pointerSize;
  mcRegister tmp = findFreeReg(jit);

  // if (mtdHasName(state->mtd, "star.multi@star.core$sequence!star.multi*multi@Γ%283@_cons")) {
  //   installBkPt(state, pc);
  // }

  mov(RG(tmp), RG(AG));
  sub(RG(tmp), IM(delta));
  cmp(RG(tmp), RG(FP));
  ja(okLbl);

  adr(tmp, okLbl);
  str(tmp, OF(STK, OffsetOf(StackRecord, pc)));

  stashLiveLocals(state, pc, False);

  invokeIntrinsic(state, pc, pc + 1, (runtimeFn)handleStackOverflow,
                  3, (FlexOp[]){RG(PR), IM(delta), IM(arity)}, False, 0, Null);
  bind(okLbl);
  releaseReg(jit, tmp);
}

retCode showStackSlot(ioPo out, void* data, long depth, long precision, logical alt) {
  localVarPo var = (localVarPo)data;

  return outMsg(out, "%s[%d]%s", (var->stkOff < 0 ? "L" : "A"), var->stkOff, (var->inited ? "*" : ""));
}

void stashEngineState(jitCompPo jit, int32 stackLevel, registerMap freeRegs) {
  assert(stackLevel<=0);
  assemCtxPo ctx = assemCtx(jit);
  mov(OF(STK, OffsetOf(StackRecord, args)), RG(AG));
  mcRegister currSP = nxtAvailReg(freeRegs);
  if (stackLevel != 0) {
    mov(RG(currSP), RG(AG));
    add(RG(currSP), IM(-stackLevel*pointerSize));
    mov(OF(STK, OffsetOf(StackRecord, sp)), RG(currSP));
  }
  else {
    mov(OF(STK, OffsetOf(StackRecord, sp)), RG(AG));
  }
  mov(OF(STK, OffsetOf(StackRecord, fp)), RG(FP));
}

void unstashEngineState(jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);
  mov(RG(STK), OF(PR, OffsetOf(EngineRecord, stk)));
  mov(RG(AG), OF(STK, OffsetOf(StackRecord, args)));
  mov(RG(FP), OF(STK, OffsetOf(StackRecord, fp)));
}

void restoreStashedLocals(codeGenPo state, int32 pc) {
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo var = &state->locals[ix];
    if (liveVar(var, pc) && var->inited && var->stashed) {
      if (isRegisterOp(var->src)) {
        loadRegister(state, var->src.op.reg, varFlex(var->stkOff));
        reserveReg(state->jit, var->src.op.reg);
        var->stashed = False;
      }
    }
  }
}

retCode showLocalVar(ioPo out, void* data, long depth, long precision, logical alt) {
  localVarPo var = (localVarPo)data;
  varDescPo desc = var->desc;
  outMsg(out, "%s[%d]%s [%d,%d)", varKindName(desc->kind), var->desc->varNo,
         (var->inited ? (var->stashed ? "S" : "") : "u"),
         desc->start, desc->end);
  if (var->inited) {
    outMsg(out, " @ %F", &var->src);
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

  state->voided[state->argMark + lastSlot - 1] = False;
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

int32 argSaveCnt(int32 arity) {
  return (int32)(clamp(0, arity - countBits(defaultArgRegs()), arity));
}

void verifyState(codeGenPo state, int32 pc) {
  registerMap freeRegs = state->jit->freeRegs;
  for (int32 ix = 0; ix < state->numLocals; ix++) {
    localVarPo v = &state->locals[ix];
    if (v->inUse && v->inited) {
      assert(v->desc->end >= pc);
      assert(isRegisterOp(v->src) || isOffsetOp(v->src));
      assert(isRegisterOp(v->src) ? !isRegInMap(freeRegs,v->src.op.reg):
        v->stashed ? v->src.op.based.disp==v->stkOff*pointerSize:True);
      assert(v->stashed ? (v->stkOff>=-state->numLocals && v->stkOff<mtdArity(state->mtd)):True);
    }
  }
}

// Implement write barrier
/*
if (t >= heap.old && t < heap.oldLimit) {
uint64 add = t - heap.old;

heap.cards[add >> CARDSHIFT] |= masks[add & CARDMASK];
}
*/
void writeBarrier(codeGenPo state, int32 pc, FlexOp src) {
  assemCtxPo ctx = assemCtx(state->jit);
  mcRegister trm = findMcRegister(state, pc);
  mcRegister hpReg = findMcRegister(state, pc);
  mcRegister tmp = findMcRegister(state, pc);
  mcRegister tmp2 = findMcRegister(state, pc);
  codeLblPo skipLbl = newLabel(ctx);

  // installBkPt(state, pc);

  loadRegister(state, trm, src);
  mov(RG(hpReg), IM((integer)&heap));
  load(ctx, tmp, hpReg, OffsetOf(HeapRecord, old));
  load(ctx, tmp2, hpReg, OffsetOf(HeapRecord, oldLimit));
  cmp(RG(tmp2), RG(trm));
  j_cc_(skipLbl, NA_CC, ctx);
  cmp(RG(tmp), RG(trm));
  j_cc_(skipLbl, A_CC, ctx);

  sub(RG(trm), RG(tmp));
  shr(RG(trm), IM(3));

  mov(RG(tmp), IM(1));
  mov(RG(tmp2), RG(trm));
  and(RG(tmp2), IM(63));
  shiftRegister(state, pc, sBLsl, tmp2, tmp, tmp2);
  mov(RG(hpReg), OF(hpReg,OffsetOf(HeapRecord,cards)));
  sar(RG(trm), IM(6));
  mov(RG(tmp), IX(hpReg,trm,8, 0));
  or(RG(tmp), RG(tmp2));
  mov(IX(hpReg, trm, 8, 0), RG(tmp));
  bind(skipLbl);
  releaseReg(state->jit, tmp);
  releaseReg(state->jit, hpReg);
  releaseReg(state->jit, trm);
  releaseReg(state->jit, tmp2);
}

void breakPt() {}

void installBkPt(codeGenPo state, int32 pc) {
  mcRegister rg = findMcRegister(state, pc);
  assemCtxPo ctx = assemCtx(state->jit);
  mov(RG(rg), IM((uinteger)breakPt));
  call(RG(rg));
  releaseReg(state->jit, rg);
}
