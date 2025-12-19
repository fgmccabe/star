//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "analyse.h"
#include "code.h"
#include "set.h"

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
  varDescPo stackLink;
} VarDescRecord;

typedef struct block_scope_ *scopePo;

typedef struct block_scope_ {
  int32 start;
  int32 limit;
  scopePo parent;
  varDescPo stack;
} ScopeBlock;

void newOutgoing(codeSegPo root, int32 pc, codeSegPo alt);
codeSegPo findSeg(codeSegPo root, int32 pc);
codeSegPo splitAtPC(codeSegPo root, int32 pc);
codeSegPo splitNextPC(codeSegPo root, int32 pc, codeSegPo alt);

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg);

void linkIncoming(codeSegPo tgt, codeSegPo incoming);

hashPo newVarTable();
void recordVariableStart(hashPo vars, int32 varNo, VarKind kind, int32 pc);
void recordVariableUse(hashPo vars, int32 varNo, int32 pc);

varDescPo newStackVar(scopePo scope, hashPo vars, int32 pc);
varDescPo newLocalVar(hashPo vars, int32 varNo);
varDescPo newArgVar(hashPo vars, int32 varNo);
varDescPo findVar(hashPo vars, int32 varNo);

void retireStackVar(scopePo scope, int32 pc);
void retireScopeStack(scopePo scope, int32 pc);
void rotateStackVars(scopePo scope, int32 pc, int32 depth);
int32 stackDepth(scopePo scope);

#endif
