//
// Created by Francis McCabe on 8/19/21.
//

#include <codeP.h>
#include "stkops.h"
#include "taskP.h"
#include "labels.h"

static MethodRec contMethod = {
  .clss = Null,
  .codeSize = 6,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Swap, OCall, 0, 2, Underflow, 0}
};

ReturnStatus g__fun2cont(heapPo h, termPo prompt,termPo a2) {
  contMethod.clss = methodClass;

  taskPo newStack = allocateTask(h, minStackSize, &contMethod, suspended, Null);

  pushStack(newStack, a2);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) newStack};
}
