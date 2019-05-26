//
// Created by Francis McCabe on 5/25/18.
//

#ifndef STAR_BKPOINT_H
#define STAR_BKPOINT_H

#include "debug.h"

typedef enum {
  lineBreak,
  callBreak
} BreakPtType;

typedef struct _break_point_ {
  BreakPtType bkType;
  char nm[MAX_SYMB_LEN];
  integer lineNo;
  integer offset;
  logical temporary;
} BreakPoint, *breakPointPo;

retCode addBreakPoint(breakPointPo bp);
retCode createBreakPoint(BreakPtType type,char *name,integer lineNo,integer offset,logical temporary);
retCode isValidBreakPoint(breakPointPo b);
breakPointPo lineBreakPointHit(normalPo loc);
breakPointPo callBreakPointHit(labelPo lbl);
retCode clearBreakPoint(breakPointPo bp);
retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp);
logical sameBreakPoint(breakPointPo b1, breakPointPo b2);
logical breakPointInUse(breakPointPo b);
void markBpOutOfUse(breakPointPo b);

logical isTempBreakPoint(breakPointPo bp);

#endif //STAR_BKPOINT_H
