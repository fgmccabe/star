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

static void initSegs()
{
  if (segmentPool == Null){
    segmentPool = newPool(sizeof(CodeSegment), 1024);
    linkPool = newPool(sizeof(SegLinkRecord), 1024);
    varPool = newPool(sizeof(VarDescRecord), 1024);
  }
}

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg)
{
  initSegs();

  codeSegPo seg = allocPool(segmentPool);

  seg->segNo = segNo++;
  seg->start = start;
  seg->end = end;
  seg->altLinks = Null;
  seg->fallthrough = Null;
  seg->next = nextSeg;
  seg->incoming = Null;
  return seg;
}

void tearDownSegs(codeSegPo root)
{
  while (root != Null){
    codeSegPo nextSeg = root->next;
    segLinkPo link = root->incoming;
    while (link != Null){
      segLinkPo nextLink = link->next;
      freePool(linkPool, link);
      link = nextLink;
    }
    link = root->altLinks;
    while (link != Null){
      segLinkPo nextLink = link->next;
      freePool(linkPool, link);
      link = nextLink;
    }
    freePool(segmentPool, root);
    root = nextSeg;
  }
  segNo = -1;
}

static segLinkPo newLink(codeSegPo seg, segLinkPo rest);
static retCode showLinks(ioPo out, char* msg, segLinkPo link);
static retCode showVars(ioPo out, hashPo vars);

static logical pcInSeg(codeSegPo seg, int32 pc)
{
  return pc >= seg->start && pc < seg->end;
}

codeSegPo findSeg(codeSegPo root, int32 pc)
{
  for (codeSegPo seg = root; seg != Null; seg = seg->next){
    if (pcInSeg(seg, pc)){
      return seg;
    }
  }
  return Null;
}

codeSegPo splitAtPC(codeSegPo root, int32 pc)
{
  codeSegPo seg = root;
  while (seg != Null && !pcInSeg(seg, pc))
    seg = seg->next;

  if (seg != Null){
    if (seg->start == pc)
      return seg;
    codeSegPo newSeg = newCodeSeg(pc, seg->end, seg->next);
    seg->next = newSeg;
    seg->end = pc;
    return newSeg;
  }
  return Null; // Special case - tried to split end of code
}

codeSegPo splitNextPC(codeSegPo root, int32 pc, codeSegPo alt)
{
  codeSegPo next = splitAtPC(root, pc + 1);
  if (next != Null){
    codeSegPo curr = findSeg(root, pc);
    curr->fallthrough = next;
    if (alt != Null)
      curr->altLinks = newLink(alt, curr->altLinks);
  }
  return next;
}

segLinkPo newLink(codeSegPo seg, segLinkPo rest)
{
  segLinkPo link = allocPool(linkPool);
  link->seg = seg;
  link->next = rest;
  return link;
}

void linkIncoming(codeSegPo tgt, codeSegPo incoming)
{
  segLinkPo link = tgt->incoming;
  while (link != Null){
    if (link->seg == incoming){
      return;
    }
    link = link->next;
  }

  tgt->incoming = newLink(incoming, tgt->incoming);
}

void newOutgoing(codeSegPo root, int32 pc, codeSegPo alt)
{
  codeSegPo curr = findSeg(root, pc);
  curr->altLinks = newLink(alt, curr->altLinks);
}

retCode showLinks(ioPo out, char* msg, segLinkPo link)
{
  if (link != Null){
    tryRet(outMsg(out,"%s: [",msg));
    char* sep = "";
    while (link != Null){
      tryRet(outMsg(out,"%s%d",sep,link->seg->segNo));
      sep = ", ";
      link = link->next;
    }
    tryRet(outMsg(out,"]\n"));
  }
  return Ok;
}

static retCode showSeg(ioPo out, methodPo mtd, codeSegPo seg)
{
  tryRet(outMsg(out, "seg: %d [%d -> %d]",seg->segNo, seg->start, seg->end));

  tryRet(showLinks(out,", incoming",seg->incoming));
  outMsg(out, "\n");

  for (int32 pc = seg->start; pc < seg->end; pc++){
    disass(out,Null, mtd, &entryPoint(mtd)[pc]);
    outMsg(out, "\n");
  }

  tryRet(showLinks(out,"alt exits",seg->altLinks));

  if (seg->fallthrough != Null){
    tryRet(outMsg(out,"fall through: %d",seg->fallthrough->segNo));
  }

  return outMsg(out, "\n%_");
}

void showSegmented(ioPo out, methodPo mtd, codeSegPo root, hashPo vars)
{
  codeSegPo seg = root;
  while (seg != Null){
    showSeg(out, mtd, seg);
    seg = seg->next;
  }
  showVars(out, vars);
  flushOut();
}

static integer varHash(void* c)
{
  varSegPo var = (varSegPo)c;
  return var->varNo;
}

static comparison varComp(void* l, void* r)
{
  varSegPo v1 = (varSegPo)l;
  varSegPo v2 = (varSegPo)r;

  return intCompare(v1->varNo, v2->varNo);
}

hashPo newVarTable()
{
  return newHash(256, varHash, varComp, Null);
}

void recordVariableStart(hashPo vars, int32 varNo, VarKind kind, int32 pc)
{
  varDescPo var = findVar(vars, varNo);
  assert(var != Null && var->start==-1);
  var->start = pc;
}

void recordVariableUse(hashPo vars, int32 varNo, int32 pc)
{
  varDescPo var = findVar(vars, varNo);

  if (var->end < pc)
    var->end = pc;
}

varDescPo newVar(hashPo vars, int32 varNo, VarKind kind, int32 pc)
{
  varDescPo var = (varDescPo)allocPool(varPool);

  assert(kind==argument ? (varNo>=0):True);

  var->varNo = varNo;
  var->kind = kind;
  var->start = pc;
  var->end = -1;

  hashPut(vars, var, var);
  return var;
}

varDescPo findVar(hashPo vars, int32 varNo)
{
  VarDescRecord nme = {.varNo = varNo};
  return hashGet(vars, &nme);
}

varDescPo newArgVar(hashPo vars, int32 varNo)
{
  return newVar(vars, varNo, argument, 0);
}

varDescPo newLocalVar(hashPo vars, int32 varNo)
{
  return newVar(vars, varNo, local, -1);
}

varDescPo newStackVar(scopePo scope, hashPo vars, int32 pc)
{
  int stackVarNo = (int32)hashSize(vars);

  varDescPo var = newVar(vars, stackVarNo, stack, pc + 1);
  var->stackLink = scope->stack;
  scope->stack = var;
  return var;
}

void retireStackVar(scopePo scope, int32 pc)
{
  varDescPo var = scope->stack;

  assert(var!=Null);

  var->end = pc + 1;
  scope->stack = var->stackLink;
}

void retireScopeStack(scopePo scope, int32 pc)
{
  while (scope->stack != Null){
    retireStackVar(scope, pc);
  }
}

static varDescPo rotateStack(varDescPo stack, varDescPo bottom, int32 depth)
{
  if (depth == 0){
    bottom->stackLink = stack;
    return bottom;
  }
  else{
    stack->stackLink = rotateStack(stack->stackLink, bottom, depth - 1);
    return stack;
  }
}

void rotateStackVars(scopePo scope, int32 pc, int32 depth)
{
  assert(depth<=stackDepth(scope));

  if (depth > 0){
    varDescPo var = scope->stack;
    scope->stack = rotateStack(var->stackLink, var, depth - 1);
  }
}

static char* varType(VarKind kind)
{
  switch (kind){
  case argument:
    return "argument";
  case local:
    return "local";
  case stack:
    return "stack";
  }
}

static retCode showVar(void* n, void* r, void* c)
{
  varDescPo var = (varDescPo)r;
  ioPo out = (ioPo)c;

  outMsg(out, "%d: %s [%d .. %d]\n", var->varNo, varType(var->kind), var->start, var->end);
  return Ok;
}

retCode showVars(ioPo out, hashPo vars)
{
  return processHashTable(showVar, vars, (void*)out);
}

int32 stackDepth(scopePo scope)
{
  int32 count = 0;
  while (scope != Null){
    varDescPo top = scope->stack;
    while (top != Null){
      count++;
      top = top->stackLink;
    }
    scope = scope->parent;
  }
  return count;
}
