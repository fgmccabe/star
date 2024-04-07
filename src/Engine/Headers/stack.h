//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACK_H
#define STAR_STACK_H

#include "term.h"
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

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, StackState state, stackPo attachment);

StackState stackState(stackPo tsk);
retCode setTaskState(stackPo stk, StackState state);

stackPo newFiber(heapPo H, termPo lam);
stackPo newStack(heapPo H, termPo lam);
stackPo splitStack(processPo P, termPo lam);
stackPo attachStack(stackPo tsk, stackPo top);
stackPo detachStack(stackPo base, stackPo top);
stackPo dropStack(stackPo tsk);
stackPo dropUntil(stackPo base, stackPo tgt);

framePo currFrame(stackPo stk);
framePo previousFrame(stackPo stk, framePo fp);
framePo pushFrame(stackPo stk, methodPo mtd, framePo fp);
integer frameNo(stackPo stk, framePo fp);

ptrPo stackSP(stackPo stk);

termPo popStack(stackPo stk);
termPo peekStack(stackPo stk, integer delta);
termPo topStack(stackPo stk);

void pushStack(stackPo stk, termPo ptr);
void moveStack2Stack(stackPo toStk, stackPo fromStk, integer count);

stackPo glueOnStack(heapPo H, stackPo stk, integer size, integer saveArity);
stackPo spinupStack(heapPo H, integer size);

integer stackHwm(stackPo stk);;

integer stackNo(stackPo stk);

extern stackPo C_STACK(termPo t);

#endif //STAR_STACK_H
