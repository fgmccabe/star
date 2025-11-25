//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_H
#define STAR_SSA_H

#include "ooio.h"
#include "code.h"
#include "starOptions.h"

typedef struct code_seg_ *codeSegPo;

extern tracingLevel traceSSA;

void showSegs(ioPo out, codeSegPo segs);

codeSegPo segmentMethod(methodPo mtd);
void tearDownSegs(codeSegPo segs);

#endif
