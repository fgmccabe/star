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
    ret = setJitCode(mtd, createCode(jit->assemCtx), fixedCopy(jit->pcLocs,Null,Null));
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

insPo jitPc(methodPo mtd, void *address) {
  if (mtd->jit!=Null) {
    int32 start = 0;
    int32 limit = termArity(locs) - 1;

    int32 lowerPc = -1;
    int32 upperPc = codeSize(mtd);

    termPo lowerLoc = Null;
    termPo upperLoc = Null;

    while (limit >= start) {
      int32 mid = start + (limit - start) / 2;

      normalPo midEntry = C_NORMAL(nthArg(locs, mid));

      int32 testPc = (int32) integerVal(nthArg(midEntry, 0));

      if (testPc == pc)
        return nthArg(midEntry, 1);
      else if (testPc < pc) {
        start = mid + 1;
        if (testPc > lowerPc) {
          lowerPc = testPc;
          lowerLoc = nthArg(midEntry, 1);
        }
      } else {
        limit = mid - 1;

        if (testPc < upperPc) {
          upperPc = testPc;
          upperLoc = nthArg(midEntry, 1);
        }
      }
    }
    if (lowerLoc != Null)
      return lowerLoc;
    else if (upperLoc != Null)
      return upperLoc;
    else
      return Null;
  } else
    return Null;
  }
  else
    return Null;
}