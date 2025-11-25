//
// Created by Francis McCabe on 11/23/25.
//

#include "ooio.h"
#include "ssaP.h"
#include "assert.h"

static poolPo segmentPool = Null;
static poolPo linkPool = Null;

static int32 segNo = 0;

static void initSegs() {
  if (segmentPool == Null) {
    segmentPool = newPool(sizeof(CodeSegment), 1024);
    linkPool = newPool(sizeof(SegLinkRecord), 1024);
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
  seg->incoming = Null;
  return seg;
}

void tearDownSegs(codeSegPo root) {
  while (root != Null) {
    codeSegPo nextSeg = root->nextByPc;
    segLinkPo link = root->incoming;
    while (link != Null) {
      segLinkPo nextLink = link->next;
      freePool(linkPool, link);
      link = nextLink;
    }
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

void linkIncoming(codeSegPo tgt, codeSegPo incoming) {
  segLinkPo link = tgt->incoming;
  while (link != Null) {
    if (link->seg == incoming) {
      return;
    }
    link = link->next;
  }

  link = allocPool(linkPool);
  link->seg = incoming;
  link->next = tgt->incoming;
  tgt->incoming = link;
}

retCode showSeg(ioPo out, codeSegPo seg) {
  tryRet(outMsg(out, "seg: %d [%d -> %d]",seg->segNo, seg->start, seg->end));
  if (seg->altLink != Null) {
    tryRet(outMsg(out," alt: %d",seg->altLink->segNo));
  }
  if (seg->fallthrough != Null) {
    tryRet(outMsg(out," fall: %d",seg->fallthrough->segNo));
  }

  if (seg->incoming != Null) {
    tryRet(outStr(out,", incoming: ["));
    segLinkPo link = seg->incoming;
    char *sep = "";
    while (link != Null) {
      tryRet(outMsg(out,"%s%d",sep,link->seg->segNo));
      sep = ", ";
      link = link->next;
    }
    tryRet(outMsg(out,"]"));
  }

  return outMsg(out, "\n%_");
}

void showSegs(ioPo out, codeSegPo segs) {
  while (segs != Null) {
    showSeg(out, segs);
    segs = segs->nextByPc;
  }
}
