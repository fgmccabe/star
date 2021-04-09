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

extern stackPo allocateStack(heapPo H, integer sze);

extern StackState stackState(stackPo stk);
extern retCode setStackState(stackPo stk, StackState state);
extern stackPo attachedStack(stackPo stk);

extern integer stackTos(stackPo stk);
extern integer stackFp(stackPo stk);
extern ptrPo stackTerm(stackPo stk, integer off);
extern framePo stackFrame(stackPo stk, integer off);

extern stackPo glueOnStack(heapPo H, stackPo stk, integer amt);

#endif //STAR_STACK_H
