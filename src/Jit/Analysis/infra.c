//
// Created by Francis McCabe on 11/23/25.
//

#include "hash.h"
#include "ooio.h"
#include "pool.h"
#include "retcode.h"
#include "analyse.h"
#include "analyseP.h"
#include "assert.h"
#include "codeP.h"

static poolPo varPool = Null;

void initAnalysis() {
  if (varPool == Null) {
    varPool = newPool(sizeof(VarDescRecord), 1024);
  }
}

void tearDownAnalysis(AnalysisRecord *results) {
  deleteSet(results->safes);
  eraseHash(results->vars);
  eraseTree(results->index);
}

static char *varType(VarKind kind);

static retCode showVar(ioPo out, varDescPo var) {
  return outMsg(out, "%d: %s [%d .. %d]\n", var->varNo, varType(var->kind), var->start, var->end);
}

static retCode showVrIndex(void *n, void *r, void *c) {
  ioPo out = (ioPo) c;
  varDescPo var = (varDescPo) r;
  int32 index = (int32) (integer) n;

  outMsg(out, "%d -> ", index);
  return showVar(out, var);
}

static retCode showVr(void *n, void *r, void *c) {
  varDescPo var = (varDescPo) r;
  ioPo out = (ioPo) c;

  return showVar(out, var);
}

retCode showVarIndex(ioPo out, analysisPo analysis) {
  return processTree(showVrIndex, analysis->index, out);
}

static retCode showVars(ioPo out, analysisPo analysis) {
  return processHashTable(showVr, analysis->vars, (void *) out);
}

void showAnalysis(ioPo out, analysisPo analysis) {
  outMsg(out, "Analysis:\n");
  outMsg(out, "  vars:\n");
  showVars(out, analysis);
  outMsg(out, "  index:\n");
  showVarIndex(out, analysis);
  outMsg(out, "Safe points: ");
  showSet(out, analysis->safes);
  outMsg(out, "\n%_");
}

static retCode checkVarIndex(void *n, void *r, void *c) {
  varDescPo var = (varDescPo) r;
  int32 index = (int32) (integer) n;

  if (var->start == index)
    return Ok;
  return Error;
}

static void checkIndex(treePo index) {
  assert(processTree(checkVarIndex,index,Null)==Ok);
}

static integer varHash(void *c) {
  varDescPo var = (varDescPo) c;
  return var->varNo;
}

static comparison varComp(void *l, void *r) {
  varDescPo v1 = (varDescPo) l;
  varDescPo v2 = (varDescPo) r;

  return intCompare(v1->varNo, v2->varNo);
}

static retCode freeVarRecord(void *r, void *cl) {
  freePool(varPool, (varDescPo) r);
  return Ok;
}

hashPo newVarTable() {
  return newHash(256, varHash, varComp, freeVarRecord);
}

static comparison indexComp(void *l, void *r) {
  integer left = (integer) l;
  integer right = (integer) r;
  if (left == right)
    return same;
  else if (left > right)
    return bigger;
  else
    return smaller;
}

treePo newVarIndex() {
  return newTree(indexComp, Null);
}

void recordVariableStart(analysisPo analysis, int32 varNo, VarKind kind, int32 pc) {
  varDescPo var = findVar(analysis, analysis->vars, varNo);
  assert(var != Null && var->start==-1);
  var->start = pc;
  treePut(analysis->index, (void *) (integer) pc, var);
}

void recordVariableUse(analysisPo analysis, int32 varNo, int32 pc) {
  varDescPo var = findVar(analysis, analysis->vars, varNo);

  if (var->end < pc)
    var->end = pc;
}

varDescPo newVar(analysisPo analysis, int32 varNo, VarKind kind, int32 pc) {
  varDescPo var = (varDescPo) allocPool(varPool);

  assert(kind==argument ? (varNo>=0):True);

  var->varNo = varNo;
  var->kind = kind;
  var->start = pc;
  var->end = -1;

  hashPut(analysis->vars, var, var);
  return var;
}

varDescPo findVar(analysisPo analysis, hashPo vars, int32 varNo) {
  VarDescRecord nme = {.varNo = varNo};
  return hashGet(analysis->vars, &nme);
}

varDescPo newArgVar(hashPo vars, int32 varNo, analysisPo analysis) {
  return newVar(analysis, varNo, argument, 0);
}

varDescPo newLocalVar(analysisPo analysis, int32 varNo) {
  return newVar(analysis, varNo, local, -1);
}

varDescPo newStackVar(analysisPo analysis, scopePo scope, int32 pc) {
  int stackVarNo = (int32) hashSize(analysis->vars);

  varDescPo var = newVar(analysis, stackVarNo, stack, pc + 1);
  var->link = scope->stack;
  scope->stack = var;
  treePut(analysis->index, (void *) (integer) pc + 1, var);
  return var;
}

varDescPo newPhiVar(analysisPo analysis, scopePo scope, int32 pc) {
  int stackVarNo = (int32) hashSize(analysis->vars);

  varDescPo var = newVar(analysis, stackVarNo, stack, pc + 1);
  var->link = scope->stack;
  scope->stack = var;
  return var;
}

static varDescPo findPhi(scopePo scope, int32 tgt) {
  while (scope != Null) {
    if (tgt == scope->start) {
      return scope->phiVar;
    }
    scope = scope->parent;
  }
  return Null;
}

void retireStackVarToPhi(scopePo scope, int32 pc, int32 tgt) {
  assert(scope!=Null && scope->stack!=Null);

  varDescPo var = scope->stack;
  scope->stack = var->link;
  varDescPo phiVar = findPhi(scope, tgt);

  assert(phiVar!=Null);
  var->link = phiVar;
}

void retireStackVar(scopePo scope, int32 pc) {
  varDescPo var = scope->stack;

  assert(var!=Null);

  var->end = pc + 1;
  scope->stack = var->link;
  var->link = Null;
}

void retireScopeStack(scopePo scope, int32 pc) {
  while (scope->stack != Null) {
    retireStackVar(scope, pc);
  }
}

static varDescPo rotateStack(varDescPo stack, varDescPo bottom, int32 depth) {
  if (depth == 0) {
    bottom->link = stack;
    return bottom;
  } else {
    stack->link = rotateStack(stack->link, bottom, depth - 1);
    return stack;
  }
}

void rotateStackVars(scopePo scope, int32 pc, int32 depth) {
  assert(depth<=stackDepth(scope));

  if (depth > 0) {
    varDescPo var = scope->stack;
    scope->stack = rotateStack(var->link, var, depth - 1);
  }
}

char *varType(VarKind kind) {
  switch (kind) {
    case argument:
      return "argument";
    case local:
      return "local";
    case stack:
      return "stack";
    case phi:
      return "phi";
    default:
      return "unknown";
  }
}

int32 stackDepth(scopePo scope) {
  int32 count = 0;
  while (scope != Null) {
    varDescPo top = scope->stack;
    while (top != Null) {
      count++;
      top = top->link;
    }
    scope = scope->parent;
  }
  return count;
}

typedef struct {
  arrayPo vars;
} SortVarInfo;

static retCode popVar(void *n, void *r, void *c) {
  SortVarInfo *info = (SortVarInfo *) c;
  varDescPo var = (varDescPo) r;
  appendEntry(info->vars, &var);
  return Ok;
}

static comparison compVarStart(arrayPo vars, integer ix, integer iy, void *cl) {
  varDescPo left = (varDescPo) (*(varDescPo *) nthEntry(vars, ix));
  varDescPo right = (varDescPo) (*(varDescPo *) nthEntry(vars, iy));

  if (left->start < right->start)
    return smaller;
  else if (left->start > right->start)
    return bigger;
  else
    return same;
}

static comparison compLastOcc(arrayPo vars, integer ix, integer iy, void *cl) {
  varDescPo left = (varDescPo) (*(varDescPo *) nthEntry(vars, ix));
  varDescPo right = (varDescPo) (*(varDescPo *) nthEntry(vars, iy));

  if (left->end < right->end)
    return smaller;
  else if (left->end > right->end)
    return bigger;
  else
    return same;
}

static retCode showVarEntry(void *entry, integer ix, void *cl) {
  ioPo out = (ioPo) cl;
  varDescPo var = *(varDescPo *) entry;
  return showVar(out, var);
}

arrayPo varStarts(analysisPo analysis) {
  arrayPo starts = allocArray(sizeof(varDescPo), hashSize(analysis->vars), True);

  SortVarInfo info = {.vars = starts};

  processHashTable(popVar, analysis->vars, &info);

  sortArray(starts, compVarStart,Null);

  outMsg(logFile, "Var starts:\n");
  processArray(starts, showVarEntry, (void *) logFile);
  return starts;
}

arrayPo varExits(analysisPo analysis) {
  arrayPo exits = allocArray(sizeof(varDescPo), hashSize(analysis->vars), True);

  SortVarInfo info = {.vars = exits};

  processHashTable(popVar, analysis->vars, &info);

  sortArray(exits, compLastOcc,Null);

  outMsg(logFile, "Var exits:\n");
  processArray(exits, showVarEntry, (void *) logFile);
  return exits;
}
