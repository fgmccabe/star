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
  if (varPool == Null){
    varPool = newPool(sizeof(VarDescRecord), 1024);
  }
}

void tearDownAnalysis(AnalysisRecord* results) {
  deleteSet(results->safes);
  eraseHash(results->vars);
  eraseTree(results->index);
}

static char* stateName[] = {
  "unAllocated",
  "beingAllocated",
  "allocated"
};

retCode showVarDesc(ioPo out, varDescPo var) {
  return outMsg(out, "%d: %s [%d, %d) %s\n", var->varNo, varKindName(var->kind), var->start,
                var->end, (var->registerCandidate ? "reg " : "mem"));
}

static retCode showVrIndex(void* n, void* r, void* c) {
  ioPo out = (ioPo)c;
  varDescPo var = (varDescPo)r;
  int32 index = (int32)(integer)n;

  outMsg(out, "%d -> ", index);
  return showVarDesc(out, var);
}

static retCode showVr(void* n, void* r, void* c) {
  varDescPo var = (varDescPo)r;
  ioPo out = (ioPo)c;

  return showVarDesc(out, var);
}

retCode showVarIndex(ioPo out, analysisPo analysis) {
  return processTree(showVrIndex, analysis->index, out);
}

retCode showVars(ioPo out, analysisPo analysis) {
  return processHashTable(showVr, analysis->vars, (void*)out);
}

static retCode checkVarIndex(void* n, void* r, void* c) {
  varDescPo var = (varDescPo)r;
  int32 index = (int32)(integer)n;

  if (var->start == index)
    return Ok;
  return Error;
}

static void checkIndex(treePo index) {
  assert(processTree(checkVarIndex,index,Null)==Ok);
}

void showAnalysis(ioPo out, analysisPo analysis) {
  checkIndex(analysis->index);
  outMsg(out, "Analysis:\n");\
  showVars(out, analysis);
  outMsg(out, "Safe points: ");
  showSet(out, analysis->safes);
  outMsg(out, "\n%_");
}

static integer varHash(void* c) {
  varDescPo var = (varDescPo)c;
  return var->varNo;
}

static comparison varComp(void* l, void* r) {
  varDescPo v1 = (varDescPo)l;
  varDescPo v2 = (varDescPo)r;

  return intCompare(v1->varNo, v2->varNo);
}

static retCode freeVarRecord(void* r, void* cl) {
  freePool(varPool, (varDescPo)r);
  return Ok;
}

hashPo newVarTable() {
  return newHash(256, varHash, varComp, freeVarRecord);
}

static comparison indexComp(void* l, void* r) {
  integer left = (integer)l;
  integer right = (integer)r;
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

void recordVariableStart(analysisPo analysis, int32 varNo, varKind kind, int32 pc) {
  varDescPo var = findVar(analysis, varNo);
  assert(var != Null && (kind==phi || var->start==-1));
  var->start = pc;
  treePut(analysis->index, (void*)(integer)pc, var);
}

void recordVariableUse(analysisPo analysis, int32 varNo, int32 pc) {
  varDescPo var = findVar(analysis, varNo);

  if (var->end < pc)
    var->end = pc;
}

varDescPo varStart(analysisPo analysis, int32 pc) {
  return treeGet(analysis->index, (void*)(integer)pc);
}

static varDescPo newVar(analysisPo analysis, int32 varNo, varKind kind, int32 pc, varAllocationState state) {
  varDescPo var = (varDescPo)allocPool(varPool);

  assert(kind==argument ? (varNo>=0):True);

  var->varNo = varNo;
  var->kind = kind;
  var->start = pc;
  var->end = -1;
  var->registerCandidate = False;
  var->state = state;

  hashPut(analysis->vars, var, var);
  return var;
}

logical isSafe(analysisPo analysis, varDescPo var) {
  return !inSetRange(analysis->safes, var->start, var->end);
}

void setSafePoint(analysisPo analysis, int32 pc) {
  addToSet(analysis->safes, pc);
}

varAllocationState varState(varDescPo var) {
  return var->state;
}

void setState(varDescPo var, varAllocationState state) {
  var->state = state;
}

void markVarAsRegister(varDescPo var) {
  var->registerCandidate = True;
}

void markVarAsMemory(varDescPo var) {
  var->registerCandidate = False;
}

varDescPo findVar(analysisPo analysis, int32 varNo) {
  VarDescRecord nme = {.varNo = varNo};
  return hashGet(analysis->vars, &nme);
}

varDescPo newArgVar(analysisPo analysis, int32 varNo) {
  return newVar(analysis, varNo, argument, 0, allocated);
}

varDescPo newLocalVar(analysisPo analysis, int32 varNo) {
  return newVar(analysis, varNo, local, -1, unAllocated);
}

varDescPo newPhiVar(analysisPo analysis, int32 varNo, int32 pc){
  varDescPo var = newVar(analysis, varNo, phi, pc, unAllocated);
  treePut(analysis->index, (void*)(integer)pc, var);
  return var;
}

void retireVar(analysisPo analysis, int32 varNo, int32 pc) {
  varDescPo var = findVar(analysis, varNo);
  assert(var!=Null);
  var->end = pc;
}

char* varKindName(varKind kind) {
  switch (kind){
  case argument:
    return "arg";
  case local:
    return "lcl";
  case phi:
    return "phi";
  default:
    return "???";
  }
}

typedef struct {
  arrayPo vars;
} SortVarInfo;

static retCode popVar(void* n, void* r, void* c) {
  SortVarInfo* info = (SortVarInfo*)c;
  varDescPo var = (varDescPo)r;
  appendEntry(info->vars, &var);
  return Ok;
}

static comparison compVarStart(arrayPo vars, int32 ix, int32 iy, void* cl) {
  varDescPo left = (varDescPo)(*(varDescPo*)nthEntry(vars, ix));
  varDescPo right = (varDescPo)(*(varDescPo*)nthEntry(vars, iy));

  if (left->start < right->start)
    return smaller;
  else if (left->start > right->start)
    return bigger;
  else
    return same;
}

static comparison compLastOcc(arrayPo vars, int32 ix, int32 iy, void* cl) {
  varDescPo left = (varDescPo)(*(varDescPo*)nthEntry(vars, ix));
  varDescPo right = (varDescPo)(*(varDescPo*)nthEntry(vars, iy));

  if (left->end < right->end)
    return smaller;
  else if (left->end > right->end)
    return bigger;
  else
    return same;
}

static comparison compVarRange(arrayPo vars, int32 ix, int32 iy, void* cl) {
  varDescPo left = (varDescPo)(*(varDescPo*)nthEntry(vars, ix));
  varDescPo right = (varDescPo)(*(varDescPo*)nthEntry(vars, iy));

  if (left->start < right->start)
    return smaller;
  else if (left->start > right->start)
    return bigger;
  else if (left->end < right->end)
    return smaller;
  else if (left->end > right->end)
    return bigger;
  else
    return same;
}

static retCode showVarEntry(void* entry, int32 ix, void* cl) {
  ioPo out = (ioPo)cl;
  varDescPo var = *(varDescPo*)entry;
  return showVarDesc(out, var);
}

arrayPo varStarts(analysisPo analysis) {
  arrayPo starts = allocArray(sizeof(varDescPo), (int32)hashSize(analysis->vars), True);

  SortVarInfo info = {.vars = starts};

  processHashTable(popVar, analysis->vars, &info);

  sortArray(starts, compVarStart,Null);

  return starts;
}

arrayPo varExits(analysisPo analysis) {
  arrayPo exits = allocArray(sizeof(varDescPo), hashSize(analysis->vars), True);

  SortVarInfo info = {.vars = exits};

  processHashTable(popVar, analysis->vars, &info);

  sortArray(exits, compLastOcc,Null);

  return exits;
}

arrayPo varRanges(analysisPo analysis) {
  arrayPo starts = allocArray(sizeof(varDescPo), (int32)hashSize(analysis->vars), True);

  SortVarInfo info = {.vars = starts};

  processHashTable(popVar, analysis->vars, &info);

  sortArray(starts, compVarRange, Null);

  return starts;
}

static retCode showVarInRange(ioPo out, void* entry, int32 ix, void* cl) {
  varDescPo var = *(varDescPo*)entry;
  return showVarDesc(out, var);
}

retCode showRanges(ioPo out, arrayPo vars) {
  return showArray(out, vars, showVarInRange, Null);
}

static retCode checkSlot(void* n, void* r, void* cl) {
  analysisPo analysis = (analysisPo)cl;
  varDescPo var = (varDescPo)r;

  assert(var->state==allocated);

  return Ok;
}

int32 minSlot(analysisPo analysis) {
  analysis->minSlot = 0;
  processHashTable(checkSlot, analysis->vars, analysis);
  return analysis->minSlot;
}
