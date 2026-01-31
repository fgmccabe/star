//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_H
#define STAR_SSA_H

#include "code.h"
#include "set.h"
#include "starOptions.h"

typedef struct var_description_ *varDescPo;

extern tracingLevel traceSSA;

extern logical enableSSA;

typedef struct analysis_ *analysisPo;

retCode analyseMethod(methodPo mtd, analysisPo results);
void tearDownAnalysis(analysisPo results);
void showAnalysis(ioPo out, analysisPo analysis);

typedef enum {
  argument,
  local,
  stack,
  phi
} varKind;

typedef enum{
  onStack,
  inRegister,
  notAllocated
} varLocation;

typedef enum {
  unAllocated,
  beingAllocated,
  allocated
} varAllocationState;

varAllocationState varState(varDescPo var);
void setState(varDescPo var, varAllocationState state);

#endif
