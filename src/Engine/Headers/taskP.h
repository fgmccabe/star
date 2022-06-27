//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_TASKP_H
#define STAR_TASKP_H

#include <assert.h>
#include "task.h"
#include "engine.h"
#include "heap.h"

typedef struct stack_frame_ *framePo;
typedef struct stack_frame_ {
  insPo pc;           // The current program counter
  methodPo prog;      // The returnee program
  framePo fp;         // Index to the arg/locals split
} StackFrame;

#define STACKFRAME_SIZE 3

typedef struct TaskStructure {
  clssPo clss;                  // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  integer hwm;                  // High water mark of stack sizes rooted off this stack
  ptrPo sp;                     // Current stack pointer
  framePo fp;                   // Current frame pointer
  taskPo attachment;           // Where is the stack attached
  TaskState state;             // is the stack attached, root, detached or moribund
  ptrPo stkMem;                 // Memory block used for stack
} StackRecord;

#define StackCellCount CellCount(sizeof(StackRecord))

extern void initTasks();
extern logical traceTasks;      // stack operation tracing

extern integer minStackSize;       /* How big is a stack */
extern integer stackRegionSize;    // How much space for stacks

static inline ptrPo stackLimit(taskPo stk) {
  return stk->stkMem + stk->sze;
}

static inline logical validStkPtr(taskPo stk, ptrPo p) {
  return p >= stk->stkMem && p <= stackLimit(stk);
}

static inline logical stkHasSpace(taskPo stk, integer amount) {
  assert(amount >= 0);
  return stk->sp - amount >= stk->stkMem;
}

extern void taskSanityCheck(taskPo stk);
extern void verifyTask(taskPo stk, heapPo H);

static inline ptrPo stackArg(taskPo stk, framePo frame, integer arg) {
  return ((ptrPo) (frame + 1)) + arg;
}

static inline ptrPo stackLcl(taskPo stk, framePo frame, integer lcl) {
  return ((ptrPo) (frame)) - lcl;
}

char *stackStateName(TaskState ste);

void propagateHwm(taskPo stk);

#endif //STAR_TASKP_H
