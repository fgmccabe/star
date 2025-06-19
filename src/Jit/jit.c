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
    assemCtxPo ctx = jit->assemCtx;
    ret = setJitCode(mtd, createCode(ctx), currentPc(ctx), fixedCopy(jit->pcLocs, Null, Null));
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
  sortArray(jit->pcLocs, compLocs, Null);
}

int32 jitPc(methodPo mtd, void *address) {
  if (mtd->jit.code != Null) {
    arrayPo locs = mtd->pcLocs;
    int32 start = 0;
    int32 limit = (int32) arrayCount(locs) - 1;
    uint32 offset = (uint32) (address - (void *) (mtd->jit.code));

    integer lowerPc = -1;
    integer upperPc = mtd->jit.codeSize;

    pcMapEntryPo lowerEntry = Null;
    pcMapEntryPo upperEntry = Null;

    while (limit >= start) {
      int32 mid = start + (limit - start) / 2;

      pcMapEntryPo midEntry = nthEntry(locs, mid);

      int32 testPc = (int32) midEntry->offset;

      if (testPc == offset)
        return midEntry->pc;
      else if (testPc < offset) {
        start = mid + 1;
        if (testPc > lowerPc) {
          lowerPc = testPc;
          lowerEntry = midEntry;
        }
      } else {
        limit = mid - 1;

        if (testPc < upperPc) {
          upperPc = testPc;
          upperEntry = midEntry;
        }
      }
    }
    if (lowerEntry != Null)
      return (int32) lowerPc;
    else if (upperEntry != Null)
      return (int32) upperPc;
    else
      return -1;
  } else
    return -1;
}
