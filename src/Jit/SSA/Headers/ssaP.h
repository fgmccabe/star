//
// Created by Francis McCabe on 11/4/25.
//

#ifndef STAR_SSA_P_H
#define STAR_SSA_P_H

#include "ssa.h"
#include "code.h"
#include "engine.h"

typedef struct code_block_ {
  int32 start;                  // Starting pc
  int32 end;                    // ending pc
} CodeBlock;

#endif
