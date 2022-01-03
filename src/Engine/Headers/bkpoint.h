//
// Created by Francis McCabe on 5/25/18.
//

#ifndef STAR_BKPOINT_H
#define STAR_BKPOINT_H

#include "debug.h"


typedef struct _break_point_ {
  char nm[MAX_SYMB_LEN];
  integer arity;
} BreakPoint, *breakPointPo;

integer addBreakPoints(breakPointPo bp);
integer clearBreakPoints(breakPointPo bp);

  retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp);
retCode showAllBreakPoints(ioPo outChnnl);

#endif //STAR_BKPOINT_H
