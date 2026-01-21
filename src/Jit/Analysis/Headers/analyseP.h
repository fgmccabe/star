//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "analyse.h"
#include "code.h"
#include "engineOptions.h"

typedef struct var_description_ {
  int32 varNo; // Variable number, first numbers are locals
  VarKind kind;
  int32 start; // PC where its value is established
  int32 end; // Last location where it is referenced
  varDescPo link;
} VarDescRecord;

typedef struct block_scope_ *scopePo;

typedef struct block_scope_ {
  int32 start;
  int32 limit;
  scopePo parent;
  varDescPo stack;
  varDescPo phiVar;
} ScopeBlock;

void initAnalysis();

hashPo newVarTable();
hashPo newVarIndex();
retCode showVarTable(ioPo out,hashPo vars);

void recordVariableStart(analysisPo analysis, int32 varNo, VarKind kind, int32 pc);
void recordVariableUse(analysisPo analysis, int32 varNo, int32 pc);

varDescPo newStackVar(analysisPo analysis, scopePo scope, int32 pc);
varDescPo newPhiVar(analysisPo analysis, scopePo scope, int32 pc);
void retireStackVarToPhi(scopePo scope, int32 pc, int32 tgt);
varDescPo newLocalVar(analysisPo analysis, int32 varNo);
varDescPo newArgVar(hashPo vars, int32 varNo, analysisPo analysis);
varDescPo findVar(analysisPo analysis, hashPo vars, int32 varNo);

void checkIndex(hashPo index);

void retireStackVar(scopePo scope, int32 pc);
void retireScopeStack(scopePo scope, int32 pc);
void rotateStackVars(scopePo scope, int32 pc, int32 depth);
int32 stackDepth(scopePo scope);

#endif
