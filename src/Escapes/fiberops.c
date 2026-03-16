//
// Created by Francis McCabe on 7/5/22.
//
#include "stack.h"
#include "engineP.h"
#include "escape.h"

ValueReturn s__fiber_eq(enginePo P, termPo l, termPo r){
  termPo Rs = (C_STACK(l) == C_STACK(r) ? trueEnum : falseEnum);
  return normalReturn(Rs);
}

ValueReturn s__fiber(enginePo P, termPo l){
  stackPo child = newStack(P, False, l);
  return normalReturn((termPo)child);
}
