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
  codeSegPo altLink; // Out of line exit to
  codeSegPo fallthrough; // Fall through to
  codeSegPo next;
  setPo defined;
  setPo used;
} CodeSegment;

typedef struct seg_link_ {
  codeSegPo seg;
  segLinkPo next;
} SegLinkRecord, *segLinkPo;

typedef struct var_seg_ {
  int32 varNo; // Variable number, first numbers are locals
  VarKind kind;
  int32 start; // PC where its value is established
  int32 end; // Last location where it is referenced
  segLinkPo uses;
} VarSegRecord;

codeSegPo findSeg(codeSegPo root, int32 pc);
codeSegPo splitAtPC(codeSegPo root, int32 pc);
codeSegPo splitNextPC(codeSegPo root, int32 pc, codeSegPo alt);

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg);

void linkIncoming(codeSegPo tgt, codeSegPo incoming);

hashPo newVarTable();
void recordNewVariable(codeSegPo root, int32 varNo, VarKind kind, int32 pc);
void recordVariableUse(codeSegPo root, int32 varNo, int32 pc);

#endif
