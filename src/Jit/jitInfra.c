//
// Created by Francis McCabe on 4/1/20.
//

#include "jit.h"
#include <lower.h>
#include "jitP.h"

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
  return jitComp;
}

void clearJitContext(jitCompPo jit) {
  discardCtx(jit->assemCtx);
  destroyObject(O_OBJECT(jit->locals));
  freePool(contextPool, jit);
}

integer allocateLocal(jitCompPo jit, integer id, integer offset, localVarState state) {
  for (integer ix = 0; ix < arrayCount(jit->locals); ix++) {
    localPo lcl = (localPo) nthEntry(jit->locals, ix);
    if (lcl->state == emptyVar && id!=-1) {
      lcl->state = state;
      if (offset != 0)
        lcl->offset = offset;
      lcl->id = id;
      return lcl->offset;
    }
  }
  LocalRecord lcl = {.offset=(offset>=0?offset:arrayCount(jit->locals) + 1), .state = state, .id=id};
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

integer cancelLocal(jitCompPo jit, integer id){
  for (integer ix = 0; ix < arrayCount(jit->locals); ix++) {
    localPo lcl = (localPo) nthEntry(jit->locals, ix);
    if (lcl->state == localVar && lcl->id == id) {
      lcl->state = emptyVar;
      return lcl->offset;
    }
  }
  return -1;
}
