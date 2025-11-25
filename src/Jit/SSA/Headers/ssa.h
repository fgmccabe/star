//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_H
#define STAR_SSA_H

#include "ooio.h"
#include "code.h"
#include "starOptions.h"

typedef struct code_seg_ *codeSegPo;
typedef struct seg_link_ *segLinkPo;

extern tracingLevel traceSSA;

extern logical enableSSA;

void showSegs(ioPo out, codeSegPo segs);

codeSegPo segmentMethod(methodPo mtd);
void tearDownSegs(codeSegPo segs);

#endif
