//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include "codeP.h"
#include "jitP.h"

integer jitThreshold = 1000;
logical jitOnLoad = False;

#ifdef TRACEJIT
tracingLevel traceJit = noTracing;
#endif

static void sortPcLocs(jitCompPo jit);

assemCtxPo assemCtx(jitCompPo jitCtx) {
  return jitCtx->assemCtx;
}

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space) {
  //  check(jitCtx->vTop >= amnt && jitCtx->vTop < NumberOf(jitCtx->vStack) - space, "stack out of bounds");
}

void markEntry(jitCompPo jit, codeLblPo entry) {
  assert(jit->entry == Null);
  jit->entry = entry;
}

codeLblPo jitEntry(jitCompPo jit) {
  return jit->entry;
}

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  jitCompPo jit = jitContext(mtd, errMsg, msgLen);

  retCode ret = jitInstructions(jit, mtd, errMsg, msgLen);

  if (ret == Ok) {
    sortPcLocs(jit);
    ret = setJitCode(mtd, createCode(jit->assemCtx), jit->pcLocs);
    jit->pcLocs = Null;
  }
  clearJitContext(jit);

  strMsg(errMsg, msgLen, "error in generating jit code");

  return ret;
}

retCode recordPC(jitCompPo jit, int32 pc, uint32 offset) {
  for (int32 ix = 0; ix < arrayCount(jit->pcLocs); ix++) {
    pcMapEntryPo entry = (pcMapEntryPo) nthEntry(jit->pcLocs, ix);
    if (entry->pc == pc) {
      return Ok; // should never happen
    } else if (entry->pc < pc)
      continue;
    else {
      PcMapEntry newEntry = {.pc = pc, .offset = offset};
      return insertEntry(jit->pcLocs, ix, (void *) &newEntry);
    }
  }
  PcMapEntry newEntry = {.pc = pc, .offset = offset};
  return appendEntry(jit->pcLocs, (void *) &newEntry);
}

comparison compLocs(arrayPo ar, integer ix, integer iy, void *cl) {
  pcMapEntryPo xEntry = (pcMapEntryPo) nthEntry(ar, ix);
  pcMapEntryPo yEntry = (pcMapEntryPo) nthEntry(ar, iy);
  if (yEntry->offset < xEntry->offset)
    return smaller;
  else if (yEntry->offset > xEntry->offset)
    return bigger;
  else
    return same;
}

void sortPcLocs(jitCompPo jit) {
  sortArray(jit->pcLocs, compLocs,Null);
}
