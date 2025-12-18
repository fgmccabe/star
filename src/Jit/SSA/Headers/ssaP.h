//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "ssa.h"
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
  setPo defined;
  setPo used;
} CodeSegment;

typedef struct seg_link_ {
  codeSegPo seg;
  segLinkPo next;
} SegLinkRecord, *segLinkPo;

typedef struct var_seg_ *varSegmentPo;

typedef struct var_seg_ {
  int32 varNo; // Variable number, first numbers are locals
  VarKind kind;
  int32 start; // PC where its value is established
  int32 end; // Last location where it is referenced
  segLinkPo uses;
  varSegmentPo next;
} VarSegRecord;

typedef struct block_scope_ *scopePo;

typedef struct block_scope_ {
  int32 start;
  int32 limit;
  int32 next;
  scopePo parent;
  varSegmentPo stack;
} ScopeBlock;

void newOutgoing(codeSegPo root, int32 pc, codeSegPo alt);
codeSegPo findSeg(codeSegPo root, int32 pc);
codeSegPo splitAtPC(codeSegPo root, int32 pc);
codeSegPo splitNextPC(codeSegPo root, int32 pc, codeSegPo alt);

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg);

void linkIncoming(codeSegPo tgt, codeSegPo incoming);

hashPo newVarTable();
void recordVariableStart(codeSegPo root, hashPo vars, int32 varNo, VarKind kind, int32 pc);
void recordVariableUse(codeSegPo root, int32 varNo, int32 pc);

varSegmentPo newStackVar(scopePo scope, hashPo vars, int32 pc);
varSegmentPo newLocalVar(hashPo vars, int32 varNo);
varSegmentPo newArgVar(hashPo vars, int32 varNo);
varSegmentPo findVar(hashPo vars, int32 varNo);


void retireStackVar(scopePo scope, int32 pc);
void rotateStackVars(scopePo scope, int32 pc, int32 depth);
int32 stackDepth(scopePo scope);

#endif
