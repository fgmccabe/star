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
typedef struct var_description_ *varSegPo;

extern tracingLevel traceSSA;

extern logical enableSSA;

codeSegPo segmentMethod(methodPo mtd);
void tearDownSegs(codeSegPo segs);
void showSegmented(ioPo out, methodPo mtd, codeSegPo root, hashPo vars);

typedef enum {
  argument,
  local,
  stack
} VarKind;

#endif
