//
// Created by Francis McCabe on 7/5/22.
//
#include "stack.h"
#include "engineP.h"

ReturnStatus g__fiber_eq(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (C_FIBER(a1) == C_FIBER(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result = Rs};
}

ReturnStatus g__new_fiber(heapPo h, termPo fiberLambda) {
  stackPo child = newFiber(currentProcess, fiberLambda);

  return (ReturnStatus) {.ret=Ok, .result = (termPo) child};
}

ReturnStatus g__suspend_fiber(heapPo h, termPo f, termPo event) {
  stackPo fiber = C_FIBER(f);
  if (stackState(fiber) != active) {
    logMsg(logFile, "tried to suspend non-active fiber %T", fiber);
    return (ReturnStatus) {.ret=Fail, .result = Null};
  } else {
    currentProcess->stk = detachFiber(currentProcess->stk, fiber);
    return (ReturnStatus) {.ret=Ok, .result = event};
  }
}

ReturnStatus g__resume_fiber(heapPo h, termPo f, termPo event) {
  stackPo fiber = C_FIBER(f);
  if (stackState(fiber) != suspended) {
    logMsg(logFile, "tried to resume non-suspended fiber %T", fiber);
    return (ReturnStatus) {.ret=Fail, .result = Null};
  } else {
    currentProcess->stk = attachFiber(currentProcess->stk, fiber);
    return (ReturnStatus) {.ret=Ok, .result = event};
  }
}

ReturnStatus g__retire_fiber(heapPo h, termPo f, termPo event) {
  stackPo fiber = C_FIBER(f);
  if (stackState(fiber) != active) {
    logMsg(logFile, "tried to retire non-active fiber %T", fiber);
    return (ReturnStatus) {.ret=Fail, .result = Null};
  } else {
    currentProcess->stk = detachFiber(currentProcess->stk, fiber);
    dropFiber(fiber);
    return (ReturnStatus) {.ret=Ok, .result = event};
  }
}
