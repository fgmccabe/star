//
// Created by Francis McCabe on 2/19/21.
//

#ifndef STAR_TASK_H
#define STAR_TASK_H

#include "term.h"
#include "engine.h"

typedef struct TaskStructure *taskPo;

extern clssPo taskClass;

static inline logical isTask(termPo p) {
  return hasClass(p, taskClass);
}

typedef enum {
  suspended,
  active,
  moribund
} TaskState;

typedef struct stack_frame_ *framePo;

taskPo allocateTask(heapPo H, integer sze, methodPo underFlow, TaskState state, taskPo attachment);

TaskState taskState(taskPo stk);
retCode setTaskState(taskPo stk, TaskState state);

taskPo attachTask(taskPo stk, taskPo top);
taskPo detachTask(taskPo base, taskPo top);
taskPo dropTask(taskPo stk);

framePo stackFrame(taskPo stk, integer off);
framePo currFrame(taskPo stk);
framePo pushFrame(taskPo stk, methodPo mtd, framePo fp, ptrPo sp);

termPo popStack(taskPo stk);
termPo peekStack(taskPo stk, integer delta);
termPo topStack(taskPo stk);

void pushStack(taskPo stk, termPo ptr);
void moveStack2Stack(taskPo toStk, taskPo fromStk, integer count);

taskPo glueOnStack(heapPo H, taskPo stk, integer size, integer saveArity);
taskPo spinupStack(heapPo H, integer size);

integer stackHwm(taskPo stk);;

integer stackNo(taskPo stk);

extern taskPo C_TASK(termPo t);

#endif //STAR_TASK_H
