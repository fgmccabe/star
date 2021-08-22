//
// Created by Francis McCabe on 8/19/21.
//

#include <codeP.h>
#include "stkops.h"
#include "stackP.h"

static MethodRec contMethod = {
  .clss = Null,
  .codeSize = 3,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Swap, TOCall, 1}
};

ReturnStatus g__fun2cont(processPo p, ptrPo tos) {
  heapPo H = processHeap(p);


  stackPo newStack = allocateStack(H, initStackSize, &contMethod, suspended, Null, Null);

  pushStack(newStack,tos[0]);

  gcReleaseRoot(H, root);

  return (ReturnStatus){.ret=Ok, .result=(termPo) newStack};
}
