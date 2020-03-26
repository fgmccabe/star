//
// Created by Francis McCabe on 3/11/18.
//


#include <array.h>
#include <assert.h>
#include <globals.h>
#include <cons.h>
#include "lblops.h"
#include "engineP.h"

ReturnStatus g__definedLbl(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char label[MAX_SYMB_LEN];
  copyString2Buff(C_STR(Arg1), label, NumberOf(label));
  integer arity = integerVal(Arg2);

  labelPo lbl = findLbl(label, arity);
  return (ReturnStatus) {.ret=Ok,
    .result = findLbl(label, arity) != Null ? trueEnum : falseEnum};
}

static inline ptrPo checkStack(processPo P, ptrPo SP) {
  assert(SP > (ptrPo) P->stackBase);
  return SP;
}

static inline void push(processPo P, termPo t) {
  *checkStack(P, --P->sp) = t;
}

static void pushArgs(processPo P, termPo args) {
  if (isCons(args)) {
    normalPo const p = C_TERM(args);
    pushArgs(P, consTail(p));
    push(P, consHead(p));
  }
}

ReturnStatus g__callLbl(processPo P, ptrPo tos) {
  integer arity = integerVal(tos[1]);
  termPo args = tos[2];

  char label[MAX_SYMB_LEN];
  copyString2Buff(C_STR(tos[0]), label, NumberOf(label));

  ReturnStatus ret = {.ret=Error, .result = voidEnum};

  labelPo lbl = findLbl(label, arity);
  if (lbl != Null || consLength(args) != arity) {
    methodPo prog = labelCode(lbl); // Which program do we want?

    if (prog == Null) {
      return ret;
    } else {
      pushArgs(P, args);

      push(P, (termPo) P->prog);
      push(P, (termPo) P->pc);       // Set up for a return
      P->pc = entryPoint(prog);
      push(P, (termPo) P->fp);
      P->fp = (framePo) P->sp;     /* set the new frame pointer */
      integer lclCnt = lclCount(prog);  /* How many locals do we have */
      P->sp -= lclCnt;
#ifdef TRACEEXEC
      for (integer ix = 0; ix < lclCnt; ix++)
        P->sp[ix] = voidEnum;
#endif
      assert(P->sp > (ptrPo) P->stackBase);
      P->prog = prog;

      ret.ret = Switch;               // Special flag for dynamic call
      return ret;
    }
  } else {
    return ret;
  }
}
