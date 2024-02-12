//
// Created by Francis McCabe on 3/11/18.
//


#include <globals.h>
#include <cons.h>
#include "lblops.h"
#include "engineP.h"

ReturnStatus g__definedLbl(heapPo h, termPo a1, termPo a2) {
  char label[MAX_SYMB_LEN];
  copyChars2Buff(C_STR(a1), label, NumberOf(label));
  integer arity = integerVal(a2);

  return (ReturnStatus) {.ret=Normal,
    .result = findLbl(label, arity) != Null ? trueEnum : falseEnum};
}

static void pushArgs(stackPo stk, termPo args) {
  if (isCons(args)) {
    normalPo p = C_NORMAL(args);
    pushArgs(stk, consTail(p));
    pushStack(stk, consHead(p));
  }
}

