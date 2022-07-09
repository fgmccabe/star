//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_STACK_H
#define STAR_STACK_H

#include "term.h"
#include "engine.h"

typedef struct StackStructure *stackPo;

extern clssPo stackClass;

static inline logical isTask(termPo p) {
  return hasClass(p, stackClass);
}

typedef enum {
  suspended,
  active,
  moribund
} TaskState;

typedef struct stack_frame_ *framePo;

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, TaskState state, stackPo attachment);

TaskState stackState(stackPo tsk);
retCode setTaskState(stackPo stk, TaskState state);

stackPo attachTask(stackPo stk, stackPo top);
stackPo detachTask(stackPo base, stackPo top);
stackPo dropStack(stackPo tsk);

framePo currFrame(stackPo stk);
framePo previousFrame(stackPo stk, framePo fp);
framePo pushFrame(stackPo stk, methodPo mtd, framePo fp);
integer frameNo(stackPo stk,framePo fp);

termPo popStack(stackPo stk);
termPo peekStack(stackPo stk, integer delta);
termPo topStack(stackPo stk);

void pushStack(stackPo stk, termPo ptr);
void moveStack2Stack(stackPo toStk, stackPo fromStk, integer count);

stackPo glueOnStack(heapPo H, stackPo stk, integer size, integer saveArity);
stackPo spinupStack(heapPo H, integer size);

integer stackHwm(stackPo stk);;

integer stackNo(stackPo stk);

extern stackPo C_TASK(termPo t);

#endif //STAR_STACK_H
