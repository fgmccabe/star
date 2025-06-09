//
// Created by Francis McCabe on 7/5/22.
//
#include "stack.h"
#include "engineP.h"
#include "escape.h"

ReturnStatus g__fiber_eq(processPo P) {
  termPo Rs = (C_STACK(popVal(P)) == C_STACK(popVal(P)) ? trueEnum : falseEnum);
  pshVal(P,Rs);
  return Normal;
}

ReturnStatus g__fiber(processPo P) {
  termPo fiberLambda = popVal(P);
  stackPo child = newStack(currentHeap, fiberLambda);
  pshVal(P,(termPo)child);
  return Normal;
}
