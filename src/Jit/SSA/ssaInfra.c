//
// Created by Francis McCabe on 11/23/25.
//

#include "ooio.h"
#include "ssaP.h"
#include "assert.h"

static poolPo segmentPool = Null;

static int32 segNo = 0;

static void initSegs() {
  if (segmentPool == Null) {
    segmentPool = newPool(sizeof(CodeSegment), 1024);
  }
}

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg) {
  initSegs();

  codeSegPo seg = allocPool(segmentPool);

  seg->segNo = segNo++;
  seg->start = start;
  seg->end = end;
  seg->altLink = Null;
  seg->fallthrough = Null;
  seg->nextByPc = nextSeg;
  return seg;
}

void tearDownSegs(codeSegPo root) {
  while (root != Null) {
    codeSegPo nextSeg = root->nextByPc;
    freePool(segmentPool, root);
    root = nextSeg;
  }
  segNo = 0;
}

static logical pcInSeg(codeSegPo seg, int32 pc) {
  return pc >= seg->start && pc < seg->end;
}

codeSegPo findSeg(codeSegPo root, int32 pc) {
  for (codeSegPo seg = root; seg != Null; seg = seg->nextByPc) {
    if (pcInSeg(seg, pc)) {
      return seg;
    }
  }
  return Null;
}

codeSegPo splitAtPC(codeSegPo root, int32 pc) {
  codeSegPo seg = root;
  while (seg != Null && !pcInSeg(seg, pc))
    seg = seg->nextByPc;

  if (seg != Null) {
    if (seg->start == pc)
      return seg;
    codeSegPo newSeg = newCodeSeg(pc, seg->end, seg->nextByPc);
    seg->nextByPc = newSeg;
    seg->end = pc;
    return newSeg;
  }
  return Null; // Special case - tried to split end of code
}

codeSegPo splitNextPC(codeSegPo root, int32 pc, codeSegPo alt) {
  codeSegPo next = splitAtPC(root, pc + 1);
  if (next != Null) {
    codeSegPo curr = findSeg(root, pc);
    curr->fallthrough = next;
    curr->altLink = alt;
  }
  return next;
}

codeSegPo bumpSeg(codeSegPo seg, int32 pc) {
  while (seg != Null && !pcInSeg(seg, pc)) {
    seg = seg->nextByPc;
  }
  return seg;
}

retCode showSeg(ioPo out, codeSegPo seg) {
  return outMsg(out, "seg: %d [%d -> %d] alt: %d, fall: %d\n%_", seg->segNo, seg->start, seg->end,
                seg->altLink != Null ? seg->altLink->segNo : -1,
                seg->fallthrough != Null ? seg->fallthrough->segNo : -1);
}

void showSegs(ioPo out, codeSegPo segs) {
  while (segs != Null) {
    showSeg(out, segs);
    segs = segs->nextByPc;
  }
}
