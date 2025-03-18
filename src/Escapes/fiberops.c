//
// Created by Francis McCabe on 7/5/22.
//
#include "stack.h"
#include "engineP.h"

ReturnStatus g__fiber_eq(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (C_STACK(a1) == C_STACK(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Normal, .result = Rs};
}

ReturnStatus g__fiber(heapPo h, termPo a1) {
  termPo fiberLambda = a1;
  stackPo child = newStack(h, fiberLambda);
  return (ReturnStatus) {.ret=Normal, .result = (termPo) child};
}
