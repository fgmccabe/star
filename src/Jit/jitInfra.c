//
// Created by Francis McCabe on 4/1/20.
//

#include "jit.h"
#include <lower.h>
#include "jitP.h"
#include "quick.h"
#include "assert.h"

static poolPo contextPool = Null;

void initJit() {
  if (contextPool == Null) {
    contextPool = newPool(sizeof(JitCompilerContext), 8);
  }
}

jitCompPo jitContext(methodPo mtd) {
  jitCompPo jitComp = (jitCompPo) allocPool(contextPool);

  jitComp->mtd = mtd;
  jitComp->vTop = 0;
  jitComp->assemCtx = createCtx();
  jitComp->usedRegs = 0;
  jitComp->freeRegs = defltAvailRegSet();
  jitComp->locals = allocArray(sizeof(LocalRecord), 0, True);
  jitComp->labels = allocArray(sizeof(LabelMarkerRecord), 0, True);
  return jitComp;
}

void clearJitContext(jitCompPo jit) {
  discardCtx(jit->assemCtx);
  jit->locals = eraseArray(jit->locals, Null, Null);
  jit->labels = eraseArray(jit->labels, Null, Null);
  freePool(contextPool, jit);
}

integer allocateLocal(jitCompPo jit, integer id, integer offset, localVarState state) {
  for (integer ix = 0; ix < arrayCount(jit->locals); ix++) {
    localPo lcl = (localPo) nthEntry(jit->locals, ix);
    if (lcl->state == emptyVar && id != -1) {
      lcl->state = state;
      if (offset != 0)
        lcl->offset = offset;
      lcl->id = id;
      return lcl->offset;
    }
  }
  LocalRecord lcl = {.offset=(offset >= 0 ? offset : arrayCount(jit->locals) + 1), .state = state, .id=id};
  appendEntry(jit->locals, &lcl);
  return lcl.offset;
}

integer findLocalOffset(jitCompPo jit, integer id) {
  for (integer ix = 0; ix < arrayCount(jit->locals); ix++) {
    localPo lcl = (localPo) nthEntry(jit->locals, ix);
    if (lcl->state == localVar && lcl->id == id)
      return lcl->offset;
  }
  return -1;
}

integer cancelLocal(jitCompPo jit, integer id) {
  for (integer ix = 0; ix < arrayCount(jit->locals); ix++) {
    localPo lcl = (localPo) nthEntry(jit->locals, ix);
    if (lcl->state == localVar && lcl->id == id) {
      lcl->state = emptyVar;
      return lcl->offset;
    }
  }
  return -1;
}

armReg findFreeReg(jitCompPo jit) {
  armReg rg = nxtAvailReg(jit->freeRegs);
  jit->freeRegs = dropReg(jit->freeRegs,rg);
  return rg;
}

void releaseReg(jitCompPo jit, armReg rg) {
  jit->freeRegs = freeReg(jit->freeRegs, rg);
}

void collectLblTgt(insPo pc, jitCompPo jit) {
  for (integer ix = 0; ix < arrayCount(jit->labels); ix++) {
    labelMarkerPo lbl = (labelMarkerPo) nthEntry(jit->labels, ix);
    if (lbl->pc == pc)
      return;
  }
  LabelMarkerRecord lbl = {.pc = pc, .lbl=Null};
  appendEntry(jit->labels, &lbl);
}

typedef retCode (*swap)(integer el1, integer el2, void *cl);

static comparison compareLblPC(integer el1, integer el2, void *cl) {
  jitCompPo jit = (jitCompPo) cl;
  labelMarkerPo l1 = (labelMarkerPo) nthEntry(jit->labels, el1);
  labelMarkerPo l2 = (labelMarkerPo) nthEntry(jit->labels, el2);

  if (l1->pc < l2->pc)
    return smaller;
  else if (l1->pc > l2->pc)
    return bigger;
  else
    return same;
}

static retCode swapLbl(integer el1, integer el2, void *cl) {
  jitCompPo jit = (jitCompPo) cl;
  labelMarkerPo l1 = (labelMarkerPo) nthEntry(jit->labels, el1);
  labelMarkerPo l2 = (labelMarkerPo) nthEntry(jit->labels, el2);

  LabelMarkerRecord t = *l1;
  *l1 = *l2;
  *l2 = t;
  return Ok;
}

retCode sortLabels(jitCompPo jit) {
  return quick(0, arrayCount(jit->labels), compareLblPC, swapLbl, (void *) jit);
}

static labelMarkerPo lblMarker(insPo pc, jitCompPo jit) {
  for (integer ix = 0; ix < arrayCount(jit->labels); ix++) {
    labelMarkerPo lbl = (labelMarkerPo) nthEntry(jit->labels, ix);
    if (lbl->pc == pc)
      return lbl;
  }
  return Null;
}

retCode resolvePcLbl(insPo code, integer off, jitCompPo jit, char *errMsg, integer msgLen) {
  insPo pc = &code[off];
  labelMarkerPo lblMrk = lblMarker(pc, jit);

  if (lblMrk != Null && lblMrk->lbl == Null) {
    assemCtxPo ctx = assemCtx(jit);

    codeLblPo lbl = defineLabel(ctx, currentPc(ctx));
    if (lbl != Null) {
      lblMrk->lbl = lbl;
      return Ok;
    } else {
      strMsg(errMsg, msgLen, "Could not define a label at %d", pc);
      return Error;
    }
  }
  return Ok;
}

codeLblPo getLblByPc(insPo pc, jitCompPo jit) {
  labelMarkerPo lblMrk = lblMarker(pc, jit);

  if (lblMrk != Null) {
    return lblMrk->lbl;
  }
  return Null;
}
