//
// Created by Francis McCabe on 7/5/22.
//
#include "task.h"

ReturnStatus g__task_eq(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (C_TASK(a1) == C_TASK(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok,
    .result = Rs};
}
