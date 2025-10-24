//
// Created by Francis McCabe on 9/25/25.
//

#ifndef STAR_REGSORT_H
#define STAR_REGSORT_H

// #include "jitP.h"
#include "macros.h"

typedef struct argSpec_
{
  FlexOp src;
  FlexOp dst;
  logical mark;
  int32 group;
} ArgSpec, *argSpecPo;

int32 sortSpecs(ArgSpec defs[], int32 arity);
int32 groupSize(argSpecPo specs, int32 arity, int32 group);

logical affects(FlexOp dst, FlexOp src);

#endif //STAR_REGSORT_H
