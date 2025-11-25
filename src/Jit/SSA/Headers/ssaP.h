//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "ssa.h"
#include "code.h"

typedef struct code_seg_ {
  int32 segNo;
  int32 start;                  // Starting pc
  int32 end;                    // ending pc
  codeSegPo altLink;            // Out of line exit to
  codeSegPo fallthrough;        // Fall through to
  codeSegPo nextByPc;
} CodeSegment;

codeSegPo findSeg(codeSegPo root, int32 pc);
codeSegPo splitAtPC(codeSegPo root,int32 pc);
codeSegPo splitNextPC(codeSegPo root, int32 pc, codeSegPo alt);
codeSegPo bumpSeg(codeSegPo seg, int32 pc);

codeSegPo newCodeSeg(int32 start, int32 end, codeSegPo nextSeg);

#endif
