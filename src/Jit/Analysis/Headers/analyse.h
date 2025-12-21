//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_H
#define STAR_SSA_H

#include "ooio.h"
#include "code.h"
#include "hash.h"
#include "set.h"
#include "starOptions.h"

typedef struct code_seg_ *codeSegPo;
typedef struct seg_link_ *segLinkPo;
typedef struct var_description_ *varDescPo;

extern tracingLevel traceSSA;

extern logical enableSSA;

typedef struct analysis_ {
  codeSegPo segments;
  hashPo vars;
  hashPo index;
  setPo safes;
} AnalysisRecord, *analysisPo;

analysisPo analyseMethod(methodPo mtd, analysisPo results);
void tearDownAnalysis(analysisPo results);

void tearDownSegs(codeSegPo segs);
void showSegmented(ioPo out, methodPo mtd, codeSegPo root);

typedef enum {
  argument,
  local,
  stack,
  phi
} VarKind;

#endif
