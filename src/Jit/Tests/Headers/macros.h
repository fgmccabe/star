//
// Created by Francis McCabe on 10/24/25.
//

#ifndef STAR_MACROS_H
#define STAR_MACROS_H

#include "config.h"
#include "logical.h"

// Simplified for testing the shuffler
// typedef uint64 registerMap;

#define Null ((void*)0)

typedef enum
{
  R1, R2, R3, R4, R5, R6, R7, R8, XZR
} mcRegister;

typedef enum
{
  reg, offset
} AddrMode;

typedef struct
{
  AddrMode mode;
  mcRegister reg;
  int32 immediate;
} FlexOp;

static logical sameFlexOp(FlexOp a, FlexOp b)
{
  return a.mode == b.mode && a.reg == b.reg && a.immediate == b.immediate;
}

#define RG(Rg) ((FlexOp){.mode=reg, .reg=Rg, .immediate=0})
#define OF(Rg,Off) ((FlexOp){.mode=offset, .reg=Rg, .immediate=Off})

#endif //STAR_MACROS_H
