//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "analyse.h"
#include "array.h"
#include "tree.h"

typedef struct analysis_ {
  hashPo vars;
  treePo index;
  setPo safes;
  int32 minSlot;
} AnalysisRecord;

typedef struct var_description_ {
  int32 varNo; // Variable number, first numbers are locals
  int32 start; // PC where its value is established
  int32 end; // Last location where it is referenced
  int32 slot; // Which slot in the frame
  logical registerCandidate;
  varKind kind;
  varAllocationState state;
  varDescPo link;
} VarDescRecord;

typedef struct block_scope_ *scopePo;

typedef struct block_scope_ {
  int32 start;
  int32 limit;
  scopePo parent;
  varDescPo stack;
} ScopeBlock;

void initAnalysis();

hashPo newVarTable();
treePo newVarIndex();
retCode showVarIndex(ioPo out, analysisPo analysis);

arrayPo varStarts(analysisPo analysis);
arrayPo varExits(analysisPo analysis);
arrayPo varRanges(analysisPo analysis);
retCode showRanges(ioPo out,arrayPo vars);
int32 minSlot(analysisPo analysis);

logical isSafe(analysisPo analysis, varDescPo var);
void setSafePoint(analysisPo analysis, int32 pc);

int32 stackLoc(varDescPo var);
void markVarAsRegister(varDescPo var);
void setVarSlot(varDescPo var, int32 slotNo);

void recordVariableStart(analysisPo analysis, int32 varNo, varKind kind, int32 pc);
void recordVariableUse(analysisPo analysis, int32 varNo, int32 pc);
varDescPo isVarStart(analysisPo analysis, int32 pc);

varDescPo newStackVar(analysisPo analysis, scopePo scope, int32 pc);
varDescPo newPhiVar(analysisPo analysis, scopePo scope, int32 pc);
varDescPo newLocalVar(analysisPo analysis, int32 varNo);
varDescPo newArgVar(hashPo vars, int32 varNo, analysisPo analysis);
varDescPo findVar(analysisPo analysis, int32 varNo);

retCode showVars(ioPo out, analysisPo analysis);

void retireStackVar(scopePo scope, int32 pc);
void retireScopeStack(scopePo scope, int32 pc);
void rotateStackVars(scopePo scope, int32 pc, int32 depth);
int32 stackDepth(scopePo scope);

int32 slotCount(analysisPo analysis);
#endif
