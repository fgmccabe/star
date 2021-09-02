//
// Created by Francis McCabe on 8/19/21.
//

#include <codeP.h>
#include "stkops.h"
#include "stackP.h"
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

ReturnStatus g__fun2cont(processPo p, ptrPo tos) {
  heapPo H = processHeap(p);
  termPo prompt = tos[0];

  contMethod.clss = methodClass;

  stackPo newStack = allocateStack(H, minStackSize, &contMethod, suspended, Null, prompt);

  pushStack(newStack, tos[1]);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) newStack};
}
