//
// Created by Francis McCabe on 3/11/18.
//


#include <arith.h>
#include <assert.h>
#include "turm.h"

ReturnStatus g__tuple_nth(processPo P, ptrPo tos) {
  normalPo tpl = C_TERM(tos[0]);
  integer ix = integerVal(tos[1]);

  termPo el = nthArg(tpl, ix);

  return (ReturnStatus) {.ret=Ok, .result=el};
}

ReturnStatus g__tuple_set_nth(processPo P, ptrPo tos) {
  normalPo tpl = C_TERM(tos[0]);
  integer ix = integerVal(tos[1]);

  assert(ix >= 0 && ix < termArity(tpl));

  setArg(tpl, ix, tos[2]);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) tpl};
}
