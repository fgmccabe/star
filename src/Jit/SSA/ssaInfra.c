//
// Created by Francis McCabe on 11/23/25.
//

#include "hash.h"
#include "ooio.h"
#include "pool.h"
#include "retcode.h"
#include "ssa.h"
#include "ssaP.h"
#include "assert.h"
#include "codeP.h"
#include "debugP.h"

static poolPo segmentPool = Null;
static poolPo linkPool = Null;
static poolPo varPool = Null;

static int32 segNo = 0;

static void initSegs() {
  if (segmentPool == Null) {
    segmentPool = newPool(sizeof(CodeSegment), 1024);
    linkPool = newPool(sizeof(SegLinkRecord), 1024);
    varPool = newPool(sizeof(VarSegRecord), 1024);
  }
}

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg) {
  initSegs();

  codeSegPo seg = allocPool(segmentPool);

  seg->segNo = segNo++;
  seg->start = start;
  seg->end = end;
  seg->altLinks = Null;
  seg->fallthrough = Null;
  seg->next = nextSeg;
  seg->incoming = Null;
  seg->defined = Null;
  seg->used = Null;
  return seg;
}

void tearDownSegs(codeSegPo root) {
  while (root != Null) {
    codeSegPo nextSeg = root->next;
    segLinkPo link = root->incoming;
    while (link != Null) {
      segLinkPo nextLink = link->next;
      freePool(linkPool, link);
      link = nextLink;
    }
    if (root->used != Null) {
      deleteSet(root->used);
      root->used = Null;
    }
    if (root->defined != Null) {
      deleteSet(root->defined);
      root->defined = Null;
    }
    link = root->altLinks;
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

static segLinkPo newLink(codeSegPo seg, segLinkPo rest);
static retCode showLinks(ioPo out, char *msg, segLinkPo link);

static logical pcInSeg(codeSegPo seg, int32 pc) {
  return pc >= seg->start && pc < seg->end;
}

codeSegPo findSeg(codeSegPo root, int32 pc) {
  for (codeSegPo seg = root; seg != Null; seg = seg->next) {
    if (pcInSeg(seg, pc)) {
      return seg;
    }
  }
  return Null;
}

codeSegPo splitAtPC(codeSegPo root, int32 pc) {
  codeSegPo seg = root;
  while (seg != Null && !pcInSeg(seg, pc))
    seg = seg->next;

  if (seg != Null) {
    if (seg->start == pc)
      return seg;
    codeSegPo newSeg = newCodeSeg(pc, seg->end, seg->next);
    seg->next = newSeg;
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
    curr->altLinks = newLink(alt, curr->altLinks);
  }
  return next;
}

segLinkPo newLink(codeSegPo seg, segLinkPo rest) {
  segLinkPo link = allocPool(linkPool);
  link->seg = seg;
  link->next = rest;
  return link;
}

void linkIncoming(codeSegPo tgt, codeSegPo incoming) {
  segLinkPo link = tgt->incoming;
  while (link != Null) {
    if (link->seg == incoming) {
      return;
    }
    link = link->next;
  }

  tgt->incoming = newLink(incoming, tgt->incoming);
}

void newOutgoing(codeSegPo root, int32 pc, codeSegPo alt) {
  codeSegPo curr = findSeg(root, pc);
  curr->altLinks = newLink(alt, curr->altLinks);
}

retCode showLinks(ioPo out, char *msg, segLinkPo link) {
  if (link != Null) {
    tryRet(outMsg(out,"%s: [",msg));
    char *sep = "";
    while (link != Null) {
      tryRet(outMsg(out,"%s%d",sep,link->seg->segNo));
      sep = ", ";
      link = link->next;
    }
    tryRet(outMsg(out,"]\n"));
  }
  return Ok;
}

static retCode showSeg(ioPo out, methodPo mtd, codeSegPo seg) {
  tryRet(outMsg(out, "seg: %d [%d -> %d]",seg->segNo, seg->start, seg->end));

  tryRet(showLinks(out,", incoming",seg->incoming));
  outMsg(out, "\n");

  for (int32 pc = seg->start; pc < seg->end; pc++) {
    disass(out,Null, mtd, &entryPoint(mtd)[pc]);
    outMsg(out, "\n");
  }

  if (seg->defined != Null) {
    outMsg(out, "defined vars: ");
    showSet(out, seg->defined);
    outMsg(out, "\n");
  }

  if (seg->used != Null) {
    outMsg(out, "referenced vars: ");
    showSet(out, seg->used);
    outMsg(out, "\n");
  }

  tryRet(showLinks(out,"alt exits",seg->altLinks));

  if (seg->fallthrough != Null) {
    tryRet(outMsg(out,"fall through: %d",seg->fallthrough->segNo));
  }

  return outMsg(out, "\n%_");
}

void showSegmented(ioPo out, methodPo mtd, codeSegPo root) {
  codeSegPo seg = root;
  while (seg != Null) {
    showSeg(out, mtd, seg);
    seg = seg->next;
  }
}

static integer varHash(void *c) {
  varSegPo var = (varSegPo) c;
  return var->varNo;
}

static comparison varComp(void *l, void *r) {
  varSegPo v1 = (varSegPo) l;
  varSegPo v2 = (varSegPo) r;

  return intCompare(v1->varNo, v2->varNo);
}

hashPo newVarTable() {
  return newHash(256, varHash, varComp, Null);
}

void recordVariableStart(codeSegPo root, hashPo vars, int32 varNo, VarKind kind, int32 pc) {
  codeSegPo seg = findSeg(root, pc);

  if (seg->defined == Null)
    seg->defined = createSet(0);

  addToSet(seg->defined, varNo);
  varSegmentPo var = findVar(vars, varNo);
  assert(var != Null && var->start==-1);
  var->start = pc;
}

void recordVariableUse(codeSegPo root, int32 varNo, int32 pc) {
  codeSegPo seg = findSeg(root, pc);

  if (seg->used == Null)
    seg->used = createSet(0);
  addToSet(seg->used, varNo);
}

varSegmentPo newVar(hashPo vars, int32 varNo, VarKind kind, int32 pc) {
  varSegmentPo var = (varSegmentPo) allocPool(varPool);

  var->varNo = varNo;
  var->kind = kind;
  var->start = pc;
  var->end = -1;
  var->uses = Null;

  hashPut(vars, var, var);
  return var;
}

varSegmentPo findVar(hashPo vars, int32 varNo) {
  VarSegRecord nme = {.varNo = varNo};
  return hashGet(vars, &nme);
}

varSegmentPo newArgVar(hashPo vars, int32 varNo) {
  return newVar(vars, varNo, argument, 0);
}

varSegmentPo newLocalVar(hashPo vars, int32 varNo) {
  return newVar(vars, varNo, argument, -1);
}

varSegmentPo newStackVar(scopePo scope, hashPo vars, int32 pc) {
  int stackVarNo = (int32) hashSize(vars);

  varSegmentPo var = newVar(vars, stackVarNo, stack, pc + 1);
  var->next = scope->stack;
  scope->stack = var;
  return var;
}

void retireStackVar(scopePo scope, int32 pc) {
  varSegmentPo var = scope->stack;

  assert(var!=Null);

  var->end = pc + 1;
  scope->stack = var->next;
}

static varSegmentPo rotateStack(varSegmentPo stack, varSegmentPo bottom, int32 depth) {
  if (depth == 0) {
    bottom->next = stack;
    return bottom;
  } else {
    stack->next = rotateStack(stack->next, bottom, depth - 1);
    return stack;
  }
}

void rotateStackVars(scopePo scope, int32 pc, int32 depth) {
  assert(depth<=stackDepth(scope));

  if (depth > 0) {
    varSegmentPo var = scope->stack;
    scope->stack = rotateStack(var->next, var, depth - 1);
  }
}

int32 stackDepth(scopePo scope) {
  int32 count = 0;
  while (scope != Null) {
    varSegmentPo top = scope->stack;
    while (top != Null) {
      count++;
      top = top->next;
    }
    scope = scope->parent;
  }
  return count;
}
