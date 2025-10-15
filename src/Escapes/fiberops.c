//
// Created by Francis McCabe on 7/5/22.
//
#include "stack.h"
#include "engineP.h"
#include "escape.h"

ReturnStatus g__fiber_eq(enginePo P) {
  termPo Rs = (C_STACK(popVal(P)) == C_STACK(popVal(P)) ? trueEnum : falseEnum);
  pshVal(P,Rs);
  return Normal;
}

ReturnStatus g__fiber(enginePo P) {
  termPo fiberLambda = popVal(P);
  newStack(P, False, fiberLambda);
  return Normal;
}
