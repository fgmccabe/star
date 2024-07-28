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
  stackPo child = newFiber(h, fiberLambda);
  return (ReturnStatus) {.ret=Normal, .result = (termPo) child};
}

ReturnStatus g__resume(heapPo h, termPo a1, termPo a2) {
  stackPo fiber = C_STACK(a1);

  assert(stackState(fiber) == suspended);

  processPo P = currentProcess;
  P->stk = attachStack(P->stk, fiber);
  return (ReturnStatus) {.ret=Normal, .result = a2};
}

ReturnStatus g__suspend(heapPo h, termPo a1, termPo a2) {
  stackPo fiber = C_STACK(a1);
  termPo event = a2;

  assert(stackState(fiber) == active);

  processPo P = currentProcess;
  P->stk = detachStack(P->stk, fiber);
  return (ReturnStatus) {.ret=Normal, .result = a2};
}

ReturnStatus g__retire(heapPo h, termPo a1, termPo a2) {
  stackPo fiber = C_STACK(a1);

  assert(stackState(fiber) == active);

  processPo P = currentProcess;
  P->stk = detachStack(P->stk, fiber);
  dropStack(fiber);
  return (ReturnStatus) {.ret=Normal, .result = a2};
}
