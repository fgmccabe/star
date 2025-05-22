//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACKP_H
#define STAR_STACKP_H

#include <assert.h>
#include "stack.h"
#include "engine.h"
#include "heap.h"
#include "labelsP.h"

/* A Stack memory has a value stack growing down from the top, and
 * activation frames growing from the base.
 *
 * /-----------/
 * /           /
 * / Arg n     /
 * / Arg n-1   /
 * /  ...      /
 * / Arg 0     /   <- ARGS
 * / Local 1   /
 * / ...       /
 * / ......... /
 * / Stack 0   /
 * /  ..       /
 * /-----------/   <- SP
 *      ..
 *      ,,
 * /-----------/
 * /  ARGS     /
 * /  PROG     /
 * /  PC       /  <- FP
 * /===========/
 *
 */
typedef struct stack_frame_ *framePo;
typedef struct stack_frame_ {
  insPo link;                   // The return program counter
  methodPo prog;                // The return program
  ptrPo args;                   // The arg/local split point
} StackFrame;

#define FrameCellCount (CellCount(sizeof(StackFrame)))

typedef struct StackStructure {
  ClassRecord clss;             // == stackClass
  integer hash;                 // Hash code of stack (== count of created stacks)
  integer sze;                  // Size of stack
  integer hwm;                  // High watermark of stack sizes rooted off this stack
  insPo pc;                     // Current program counter
  ptrPo args;                   // Current argument pointer
  methodPo prog;                // Currently running function
  ptrPo sp;                     // Current stack pointer
  framePo fp;                   // Current frame pointer
  stackPo attachment;           // Where is the stack attached
  stackPo bottom;               // What is the actual innermost stack
  StackState state;             // is the stack attached, root, detached or moribund
  ptrPo stkMem;                 // Memory block used for stack
} StackRecord;

#define StackCellCount CellCount(sizeof(StackRecord))

extern void initStacks();
extern tracingLevel traceStack;    // stack operation tracing

#define MINMINSTACKSIZE (64)

extern integer minStackSize;       /* How big is a stack */
extern integer defaultStackSize;
extern integer stackRegionSize;    // How much space for stacks

void dumpStackStats(ioPo out);

static inline ptrPo stackLimit(stackPo stk) {
  return stk->stkMem + stk->sze;
}

static inline framePo baseFrame(stackPo stk) {
  return ((framePo) (stk->stkMem)) - 1;
}

static inline logical validFP(stackPo stk, framePo fp) {
  return fp <= stk->fp && fp >= baseFrame(stk) && ((ptrPo) (fp + 1)) < stk->sp;
}

static inline logical validStkPtr(stackPo stk, ptrPo p) {
  return p >= stk->stkMem && p >= stk->sp && p <= stackLimit(stk);
}

static inline logical stackHasSpace(stackPo stk, integer amount) {
  assert(amount >= 0);
  return stk->sp - amount > ((ptrPo)(stk->fp+1));
}

framePo dropFrame(stackPo stk);
int32 stackDepth(stackPo stk, methodPo mtd, ptrPo sp, framePo fp);

void stackSanityCheck(stackPo stk);
void verifyStack(stackPo stk, heapPo H);
logical isAttachedStack(stackPo base, stackPo tgt);

static inline ptrPo stackArg(framePo frame, integer arg) {
  return &frame->args[arg];
}

static inline ptrPo stackLcl(framePo frame, int32 lcl) {
  return &frame->args[-lcl];
}

char *stackStateName(StackState ste);

void propagateHwm(stackPo stk);

typedef enum {
  showPrognames,
  showArguments,
  showLocalVars
} StackTraceLevel;

void showStackCall(ioPo out, integer depth, framePo fp, stackPo stk, integer frameNo, StackTraceLevel tracing);
void stackTrace(processPo p, ioPo out, stackPo stk, integer depth, StackTraceLevel tracing, integer maxDepth);

static inline methodPo frameMtd(framePo fp) {
  return fp->prog;
}

#endif //STAR_STACKP_H
