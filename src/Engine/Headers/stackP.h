//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACKP_H
#define STAR_STACKP_H

#include "stack.h"
#include "engine.h"
#include "heap.h"

extern stackPo C_STACK(termPo t);

typedef struct StackStructure {
  clssPo clss;                  // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  integer tos;                  // current top of stack
  integer fp;                   // Current frame pointer
  methodPo prog;                /* current program */
  stackPo attachment;           // Where is the stack attached
  StackState state;             // is the stack attached, root or detached
  integer prompt;               // Point in the stack where last prompt was pushed
  ptrPo stack[ZEROARRAYSIZE];
} StackRecord;

#define StackCellCount(len) CellCount(sizeof(StackRecord)+((len)*sizeof(ptrPo)))

extern void initStacks();

#endif //STAR_STACKP_H
