//
// Created by Francis McCabe on 3/11/18.
//


#include <assert.h>
#include <globals.h>
#include <cons.h>
#include "lblops.h"
#include "engineP.h"

ReturnStatus g__definedLbl(heapPo h, termPo a1, termPo a2) {
  char label[MAX_SYMB_LEN];
  copyChars2Buff(C_STR(a1), label, NumberOf(label));
  integer arity = integerVal(a2);

  return (ReturnStatus) {.ret=Ok,
    .result = findLbl(label, arity) != Null ? trueEnum : falseEnum};
}

static void pushArgs(stackPo stk, termPo args) {
  if (isCons(args)) {
    normalPo p = C_NORMAL(args);
    pushArgs(stk, consTail(p));
    pushStack(stk, consHead(p));
  }
}

ReturnStatus g__callLbl(processPo p, termPo a1, termPo a2, termPo a3) {
  integer arity = integerVal(a2);

  char label[MAX_SYMB_LEN];
  copyChars2Buff(C_STR(a1), label, NumberOf(label));

  ReturnStatus ret = {.ret=Error, .result = voidEnum};

  labelPo lbl = findLbl(label, arity);
  if (lbl != Null || consLength(a2) != arity) {
    methodPo prog = labelCode(lbl); // Which program do we want?

    if (prog == Null) {
      return ret;
    } else {
      stackPo stk = p->stk;

      pushArgs(stk, a2);
      stk->fp = pushFrame(stk, prog, stk->fp);

      ret.ret = Switch;               // Special flag for dynamic call
      return ret;
    }
  } else {
    return ret;
  }
}
