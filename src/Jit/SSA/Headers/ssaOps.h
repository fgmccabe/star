//
// Created by Francis McCabe on 1/15/26.
//

#ifndef STAR_SSAOPS_H
#define STAR_SSAOPS_H

#undef instr

#define instr(Op, ...) s##Op,

typedef enum
{
#include "ssaInstructions.h"
  sInval
} ssaOp;

#endif //STAR_SSAOPS_H