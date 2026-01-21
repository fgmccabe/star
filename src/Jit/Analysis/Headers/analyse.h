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

typedef struct var_description_ *varDescPo;

extern tracingLevel traceSSA;

extern logical enableSSA;

typedef struct analysis_ {
  hashPo vars;
  hashPo index;
  setPo safes;
} AnalysisRecord, *analysisPo;

retCode analyseMethod(methodPo mtd, analysisPo results);
void tearDownAnalysis(analysisPo results);

typedef enum {
  argument,
  local,
  stack,
  phi
} VarKind;

#endif
