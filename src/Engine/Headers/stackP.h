//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACKP_H
#define STAR_STACKP_H

#include <assert.h>
#include "stack.h"
#include "engine.h"
#include "heap.h"

#define MAX_TRY (128)

typedef struct stack_frame_ *framePo;
typedef struct stack_frame_ {
  insPo pc;           // The current program counter
  methodPo prog;      // The returnee program
  framePo fp;         // Previous frame
} StackFrame;

typedef struct try_frame_ *tryFramePo;
typedef struct try_frame_ {
  integer tryIndex;             // Special index incremented for each try
  framePo fp;
  insPo pc;
  tryFramePo try;              // Previous try frame
} TryFrame;

#define STACKFRAME_SIZE 3

typedef struct StackStructure {
  clssPo clss;                  // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  integer hwm;                  // High watermark of stack sizes rooted off this stack
  ptrPo sp;                     // Current stack pointer
  framePo fp;                   // Current frame pointer
  tryFramePo try;
  stackPo attachment;           // Where is the stack attached
  stackPo bottom;               // What is the actual innermost stack
  StackState state;              // is the stack attached, root, detached or moribund
  ptrPo stkMem;                 // Memory block used for stack
} StackRecord;

#define StackCellCount CellCount(sizeof(StackRecord))
#define TryFrameCellCount CellCount(sizeof (TryFrame))

extern void initStacks();
extern logical traceStack;      // stack operation tracing

#define MINMINSTACKSIZE (64)

extern integer minStackSize;       /* How big is a stack */
extern integer defaultStackSize;
extern integer stackRegionSize;    // How much space for stacks

void dumpStackStats(ioPo out);

integer pushTryFrame(stackPo stk, processPo P, insPo pc, ptrPo sp, framePo fp);
stackPo popTryFrame(processPo P, integer tryIndex);
retCode dropTryFrame(processPo P, integer tryIndex);
integer tryStackSize(processPo P);

static inline ptrPo stackLimit(stackPo stk) {
  return stk->stkMem + stk->sze;
}

static inline framePo baseFrame(stackPo stk) {
  return (framePo) stackLimit(stk);
}

static inline tryFramePo tryLimit(stackPo stk) {
  return (tryFramePo) (stk->stkMem + stk->sze);
}

static inline logical validFP(stackPo stk, framePo fp) {
  return fp >= stk->fp && (ptrPo) fp >= stk->sp && fp <= baseFrame(stk);
}

static inline logical validStkPtr(stackPo stk, ptrPo p) {
  return p >= stk->stkMem && p >= stk->sp && p <= stackLimit(stk) && p <= (ptrPo) stk->fp;
}

static inline logical stackHasSpace(stackPo stk, integer amount) {
  assert(amount >= 0);
  return stk->sp - amount >= (stk->stkMem);
}

framePo dropFrame(stackPo stk);
integer stackDepth(stackPo stk, methodPo mtd, ptrPo sp, framePo fp);

void stackSanityCheck(stackPo stk);
void verifyStack(stackPo stk, heapPo H);
logical isAttachedStack(stackPo base, stackPo tgt);

static inline ptrPo stackArg(stackPo stk, framePo frame, integer arg) {
  return ((ptrPo) (frame + 1)) + arg;
}

static inline ptrPo stackLcl(stackPo stk, framePo frame, integer lcl) {
  return ((ptrPo) frame) - lcl;
}

char *stackStateName(StackState ste);

void propagateHwm(stackPo stk);

typedef enum {
  showPrognames,
  showArguments,
  showLocalVars
} StackTraceLevel;

void showStackCall(ioPo out, integer depth, framePo fp, stackPo stk, integer frameNo, StackTraceLevel tracing);
void stackTrace(processPo p, ioPo out, stackPo stk, integer depth, StackTraceLevel tracing);

#endif //STAR_STACKP_H
