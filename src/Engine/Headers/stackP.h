//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACKP_H
#define STAR_STACKP_H

#include <assert.h>
#include "stack.h"
#include "engine.h"
#include "heap.h"

extern stackPo C_STACK(termPo t);

typedef struct stack_frame_ *framePo;
typedef struct stack_frame_ {
  insPo pc;           // The current program counter
  methodPo prog;      // The returnee program
  framePo fp;         // Index to the arg/locals split
} StackFrame;

#define STACKFRAME_SIZE 3

typedef struct StackStructure {
  clssPo clss;                  // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  ptrPo sp;                     // Current stack pointer
  framePo fp;                   // Current frame pointer
  stackPo attachment;           // Where is the stack attached
  StackState state;             // is the stack attached, root or detached
  termPo prompt;                // Prompt label for this stack
  ptrPo stkMem;                 // Memory block used for stack
} StackRecord;

#define StackCellCount CellCount(sizeof(StackRecord))

extern void initStacks();
extern logical traceStacks;      // stack operation tracing

extern integer minStackSize;       /* How big is a stack */
extern integer stackRegionSize;    // How much space for stacks

static inline ptrPo stackLimit(stackPo stk) {
  return stk->stkMem + stk->sze;
}

static inline logical validStkPtr(stackPo stk, ptrPo p) {
  return p >= stk->stkMem && p <= stackLimit(stk);
}

static inline logical stkHasSpace(stackPo stk, integer amount) {
  assert(amount >= 0);
  return stk->sp - amount >= stk->stkMem;
}

extern void stackSanityCheck(stackPo stk);
extern void verifyStack(stackPo stk, heapPo H);

static inline ptrPo stackArg(stackPo stk, framePo frame, integer arg) {
  return ((ptrPo) (frame + 1)) + arg;
}

static inline ptrPo stackLcl(stackPo stk, framePo frame, integer lcl) {
  return ((ptrPo) (frame)) - lcl;
}

#endif //STAR_STACKP_H
