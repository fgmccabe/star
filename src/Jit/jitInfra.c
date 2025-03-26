//
// Created by Francis McCabe on 4/1/20.
//

#include "jit.h"
#include <lower.h>
#include "jitP.h"

static poolPo contextPool = Null;
static poolPo labelPool = Null;

void initJit() {
  if (contextPool == Null) {
    contextPool = newPool(sizeof(JitCompilerContext), 8);
    labelPool = newPool(sizeof(LabelMarkerRecord), 256);
  }
}

static integer pcHash(void *pc) {
  return (integer) (pc);
}

static comparison pcComp(void *l, void *r) {
  if (l == r)
    return same;
  else
    return different;
}

static retCode delEntry(void *n, void *r) {
  labelMarkerPo entry = (labelMarkerPo) r;

  if (entry->lbl != Null) {
    clearLbl(entry->lbl);
    entry->lbl = Null;
  }

  freePool(labelPool, entry);
  return Ok;
}

jitCompPo jitContext(methodPo mtd) {
  jitCompPo jitComp = (jitCompPo) allocPool(contextPool);

  jitComp->mtd = mtd;
  jitComp->vTop = 0;
  jitComp->assemCtx = createCtx();
  jitComp->usedRegs = 0;
  jitComp->freeRegs = defltAvailRegSet();
  jitComp->locals = allocArray(sizeof(LocalRecord), 0, True);
  jitComp->labels = newHash(1024, pcHash, pcComp, delEntry);
  return jitComp;
}

void clearJitContext(jitCompPo jit) {
  discardCtx(jit->assemCtx);
  jit->locals = eraseArray(jit->locals, Null, Null);
  eraseHash(jit->labels);
  jit->labels = Null;
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
  jit->freeRegs = dropReg(jit->freeRegs, rg);
  return rg;
}

void releaseReg(jitCompPo jit, armReg rg) {
  jit->freeRegs = freeReg(jit->freeRegs, rg);
}

codeLblPo defineJitLbl(jitCompPo jit, insPo pc) {
  labelMarkerPo entry = (labelMarkerPo) hashGet(jit->labels, (void *) pc);
  if (entry != Null) {
    assert(!isLabelDefined(entry->lbl));
    assert(entry->pc == pc);
    setLabel(jit->assemCtx,entry->lbl);
    return entry->lbl;
  } else {
    entry = (labelMarkerPo) allocPool(labelPool);
    entry->pc = pc;
    entry->lbl = currentPcLabel(jit->assemCtx);
    hashPut(jit->labels, entry->pc, entry);
    return entry->lbl;
  }
}

codeLblPo getJitLbl(jitCompPo jit, insPo pc) {
  labelMarkerPo entry = (labelMarkerPo) hashGet(jit->labels, (void *) pc);
  if (entry != Null) {
    assert(entry->pc == pc);
    return entry->lbl;
  } else
    return Null;
}

codeLblPo newJitLbl(jitCompPo jit, insPo pc) {
  labelMarkerPo entry = (labelMarkerPo) hashGet(jit->labels, (void *) pc);
  if (entry != Null) {
    assert(entry->pc == pc);
    setLabel(jit->assemCtx, entry->lbl);
    return entry->lbl;
  } else {
    entry = (labelMarkerPo) allocPool(labelPool);
    entry->lbl = defineLabel(jit->assemCtx, currentPc(jit->assemCtx));
    entry->pc = pc;
    hashPut(jit->labels, pc, entry);
    return entry->lbl;
  }
}
