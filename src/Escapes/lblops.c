//
// Created by Francis McCabe on 3/11/18.
//


#include <assert.h>
#include <globals.h>
#include <cons.h>
#include "lblops.h"
#include "engineP.h"

ReturnStatus g__definedLbl(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char label[MAX_SYMB_LEN];
  copyChars2Buff(C_STR(Arg1), label, NumberOf(label));
  integer arity = integerVal(Arg2);

  return (ReturnStatus) {.ret=Ok,
    .result = findLbl(label, arity) != Null ? trueEnum : falseEnum};
}

static inline void push(processPo P, termPo t) {
  stackPo stack = P->stk;
  stackSanityCheck(P->stk);
  *--stack->sp = t;
}

static void pushArgs(processPo P, termPo args) {
  if (isCons(args)) {
    normalPo p = C_NORMAL(args);
    pushArgs(P, consTail(p));
    push(P, consHead(p));
  }
}

ReturnStatus g__callLbl(processPo P, ptrPo tos) {
  integer arity = integerVal(tos[1]);
  termPo args = tos[2];

  char label[MAX_SYMB_LEN];
  copyChars2Buff(C_STR(tos[0]), label, NumberOf(label));

  ReturnStatus ret = {.ret=Error, .result = voidEnum};

  labelPo lbl = findLbl(label, arity);
  if (lbl != Null || consLength(args) != arity) {
    methodPo prog = labelCode(lbl); // Which program do we want?

    if (prog == Null) {
      return ret;
    } else {
      pushArgs(P, args);

      stackPo stk = P->stk;
      pushFrame(stk,prog,stk->fp,stk->sp);

      integer lclCnt = lclCount(prog);  /* How many locals do we have */
      ptrPo sp = stk->sp -= lclCnt;
#ifdef TRACEEXEC
      for (integer ix = 0; ix < lclCnt; ix++)
        sp[ix] = voidEnum;
#endif
      assert(validStkPtr(stk, stk->sp));

      ret.ret = Switch;               // Special flag for dynamic call
      return ret;
    }
  } else {
    return ret;
  }
}
