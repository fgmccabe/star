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

ReturnStatus g__fiber_eq(enginePo P) {
  termPo l = popVal(P);
  termPo r = popVal(P);

  ValueReturn ret = s__fiber_eq(P,l,r);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__fiber(enginePo P, termPo l){
  stackPo child = newStack(P, False, l);
  return normalReturn((termPo)child);
}

ReturnStatus g__fiber(enginePo P) {
  termPo l = popVal(P);
  ValueReturn ret = s__fiber(P,l);
  pshVal(P,ret.value);
  return ret.status;
}
