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

typedef struct stack_frame_ {
  insPo pc;           // The current program counter
  methodPo prog;      // The returnee program
  integer fp;         // Index to the arg/locals split
} StackFrame;

#define STACKFRAME_SIZE 3

typedef struct StackStructure {
  clssPo clss;                  // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  integer sp;                   // current top of value stack
  integer fp;                   // Current frame index
  stackPo attachment;           // Where is the stack attached
  StackState state;             // is the stack attached, root or detached
  termPo prompt;                // Prompt label for this stack
  ptrPo stack[ZEROARRAYSIZE];
} StackRecord;

#define StackCellCount(len) CellCount(sizeof(StackRecord)+((len)*sizeof(ptrPo)))

extern void initStacks();
extern logical traceStacks;      // stack operation tracing

extern long initStackSize;       /* How big is a stack */
extern long initThreadStackSize; // How big is a stacklet
extern long maxStackSize;        // How big may the stack grow?

static inline integer spMin(stackPo stk){
  return CellCount(sizeof(StackFrame)) * (stk->fp + 1);
}

static inline ptrPo validStkPtr(stackPo stk, integer offset) {
  assert(offset >= spMin(stk) && offset <= stk->sze);
  return (ptrPo) &stk->stack[offset];
}

static inline integer stackOffset(stackPo stk, ptrPo ptr) {
  integer off = ptr - (ptrPo) stk->stack;
  assert(off >= spMin(stk) && off <= stk->sze);
  return off;
}

extern void stackSanityCheck(stackPo stk);
extern void verifyStack(stackPo stk, heapPo H);

static inline termPo stackArg(stackPo stk, integer frame, integer arg) {
  return *validStkPtr(stk, stackFrame(stk, frame)->fp + arg);
}

static inline termPo stackLcl(stackPo stk, integer frame, integer lcl) {
  return *validStkPtr(stk, stackFrame(stk, frame)->fp - lcl);
}

#endif //STAR_STACKP_H
