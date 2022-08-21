//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACKP_H
#define STAR_STACKP_H

#include <assert.h>
#include "stack.h"
#include "engine.h"
#include "heap.h"

typedef struct stack_frame_ *framePo;
typedef struct stack_frame_ {
  insPo pc;           // The current program counter
  methodPo prog;      // The returnee program
  ptrPo csp;          // Pointer to the arg/local split on value stack
} StackFrame;

#define STACKFRAME_SIZE 3

typedef struct StackStructure {
  clssPo clss;                  // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  integer hwm;                  // High water mark of stack sizes rooted off this stack
  ptrPo sp;                     // Current stack pointer
  framePo fp;                   // Current frame pointer
  stackPo attachment;           // Where is the stack attached
  stackPo bottom;               // What is the actual innermost stack
  TaskState state;             // is the stack attached, root, detached or moribund
  ptrPo stkMem;                 // Memory block used for stack
} StackRecord;

#define StackCellCount CellCount(sizeof(StackRecord))

extern void initFibers();
extern logical traceStack;      // stack operation tracing

#define MINMINSTACKSIZE (64)

extern integer minStackSize;       /* How big is a stack */
extern integer stackRegionSize;    // How much space for stacks

void dumpStackStats();

static inline ptrPo stackLimit(stackPo stk) {
  return stk->stkMem + stk->sze;
}

static inline framePo baseFrame(stackPo stk) {
  return (framePo) stk->stkMem;
}

static inline logical validStkValueLoc(stackPo stk, ptrPo p) {
  return p >= ((ptrPo) (stk->fp + 1)) && p <= stackLimit(stk);
}

static inline logical fiberHasSpace(stackPo stk, integer amount) {
  assert(amount >= 0);
  return stk->sp - amount >= (ptrPo) (stk->fp + 1);
}

extern void stackSanityCheck(stackPo stk);
extern void verifyStack(stackPo stk, heapPo H);

static inline ptrPo stackArg(stackPo stk, framePo frame, integer arg) {
  return &frame->csp[arg];
}

static inline ptrPo stackLcl(stackPo stk, framePo frame, integer lcl) {
  return &frame->csp[-lcl];
}

char *stackStateName(TaskState ste);

void propagateHwm(stackPo stk);

void
showStackCall(ioPo out, integer displayDepth, logical showLocals, stackPo stk, framePo fp, ptrPo sp, integer frameNo);
void stackTrace(processPo p, ioPo out, stackPo stk, logical showLocals, integer displayDepth);

#endif //STAR_STACKP_H
