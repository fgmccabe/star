//
// Created by Francis McCabe on 7/5/22.
//
#include "stack.h"

ReturnStatus g__fiber_eq(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (C_FIBER(a1) == C_FIBER(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok,
    .result = Rs};
}
