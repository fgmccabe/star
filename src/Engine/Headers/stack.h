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
  attached
} StackState;


typedef struct stack_frame_ *framePo;

extern stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, termPo prompt);

extern StackState stackState(stackPo stk);
extern retCode setStackState(stackPo stk, StackState state);
extern stackPo attachedStack(stackPo stk);

extern framePo stackFrame(stackPo stk, integer off);
extern framePo currFrame(stackPo stk);
extern framePo pushFrame(stackPo stk, methodPo mtd, integer fp);

extern termPo popStack(stackPo stk);
extern void pushStack(stackPo stk, termPo ptr);

extern stackPo glueOnStack(heapPo H, stackPo stk, integer size);
extern stackPo spinupStack(heapPo H, stackPo stk, integer size,termPo prompt);

#endif //STAR_STACK_H
