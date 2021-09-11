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

extern stackPo stackVal(termPo o);

typedef enum {
  root,
  detached,
  suspended,
  attached
} StackState;

typedef struct stack_frame_ *framePo;

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, StackState state, stackPo attachment, termPo prompt);

StackState stackState(stackPo stk);
retCode setStackState(stackPo stk, StackState state);
stackPo attachedStack(stackPo stk);

stackPo attachStack(stackPo stk, stackPo seg);
stackPo detachStack(stackPo stk,termPo prompt);
stackPo dropStack(stackPo stk);

framePo stackFrame(stackPo stk, integer off);
framePo currFrame(stackPo stk);
framePo pushFrame(stackPo stk, methodPo mtd, framePo fp, ptrPo sp);

termPo popStack(stackPo stk);
void pushStack(stackPo stk, termPo ptr);
void moveStack2Stack(stackPo toStk, stackPo fromStk, integer count);

stackPo glueOnStack(heapPo H, stackPo stk, integer size);
stackPo spinupStack(heapPo H, stackPo stk, integer size, termPo prompt);

stackPo findStack(stackPo current, termPo prompt);
termPo stackPrompt(stackPo stk);
void setPrompt(stackPo stk,termPo prompt);

integer stackNo(stackPo stk);

#endif //STAR_STACK_H
