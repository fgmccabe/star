//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "analyse.h"
#include "code.h"
#include "engineOptions.h"

typedef struct code_seg_ {
  int32 segNo;
  int32 start; // Starting pc
  int32 end; // ending pc
  segLinkPo incoming;
  segLinkPo altLinks; // used when there is more than one outgoing
  codeSegPo fallthrough; // Fall through to
  codeSegPo next;
} CodeSegment;

typedef struct seg_link_ {
  codeSegPo seg;
  segLinkPo next;
} SegLinkRecord, *segLinkPo;

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

void newOutgoing(codeSegPo root, int32 pc, codeSegPo alt);
codeSegPo findSeg(codeSegPo root, int32 pc);
codeSegPo splitAtPC(codeSegPo root, int32 pc);
codeSegPo splitNextPC(analysisPo analysis, int32 pc, codeSegPo alt);

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg);

void linkIncoming(codeSegPo tgt, codeSegPo incoming);

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

#ifdef TRACEJIT
void checkIndex(hashPo index);
#endif

void retireStackVar(scopePo scope, int32 pc);
void retireScopeStack(scopePo scope, int32 pc);
void rotateStackVars(scopePo scope, int32 pc, int32 depth);
int32 stackDepth(scopePo scope);

#endif
