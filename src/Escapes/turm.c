//
// Created by Francis McCabe on 3/11/18.
//


#include <arith.h>
#include <assert.h>
#include "turm.h"

ReturnStatus g__tuple_nth(processPo P, heapPo h, termPo a1, termPo a2) {
  normalPo tpl = C_NORMAL(a1);
  integer ix = integerVal(a2);

  termPo el = nthArg(tpl, ix);

  return (ReturnStatus) {.ret=Ok, .result=el};
}

ReturnStatus g__tuple_set_nth(processPo P, heapPo h, termPo a1, termPo a2, termPo a3) {
  normalPo tpl = C_NORMAL(a1);
  integer ix = integerVal(a2);

  assert(ix >= 0 && ix < termArity(tpl));

  setArg(tpl, ix, a3);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) tpl};
}
