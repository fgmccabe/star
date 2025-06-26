//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACK_H
#define STAR_STACK_H

#include "heap.h"
#include "term.h"
#include "code.h"
#include "closure.h"
#include "engine.h"

typedef struct StackStructure *stackPo;

extern clssPo stackClass;

static inline logical isStack(termPo p) {
  return hasClass(p, stackClass);
}

typedef enum {
  suspended,
  active,
  moribund
} StackState;

typedef struct stack_frame_ *framePo;

stackPo allocateStack(heapPo H, integer sze, labelPo underFlow, StackState state, stackPo attachment);

StackState stackState(stackPo tsk);

stackPo newStack(heapPo H, logical execJit, termPo lam);
void attachStack(enginePo P, stackPo top, termPo evt);
void detachStack(enginePo P, stackPo top, termPo event);
stackPo dropStack(stackPo tsk);
void detachDropStack(enginePo P, stackPo top, termPo event);

framePo currFrame(stackPo stk);
framePo previousFrame(stackPo stk, framePo fp);
framePo pushFrame(stackPo stk, logical execJit, methodPo mtd);

termPo popStack(stackPo stk);
termPo peekStack(stackPo stk, integer delta);
termPo topStack(stackPo stk);

void handleStackOverflow(enginePo P, integer delta, int32 arity);

void pushStack(stackPo stk, termPo ptr);
void moveStack2Stack(stackPo toStk, stackPo fromStk, integer count);

void glueOnStack(enginePo P, integer size, integer saveArity);
stackPo spinupStack(heapPo H, integer size);

integer stackHwm(stackPo stk);
integer stackNo(stackPo stk);

extern stackPo C_STACK(termPo t);

#endif //STAR_STACK_H
