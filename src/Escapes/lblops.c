//
// Created by Francis McCabe on 3/11/18.
//


#include <array.h>
#include <assert.h>
#include <globals.h>
#include "lblops.h"
#include "engineP.h"

ReturnStatus g__definedLbl(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char label[MAX_SYMB_LEN];
  copyString2Buff(C_STR(Arg1), label, NumberOf(label));
  integer arity = integerVal(Arg2);

  labelPo lbl = findLbl(label, arity);
  ReturnStatus ret = {.ret=Ok, .rslt = findLbl(label, arity) != Null ? trueEnum : falseEnum};

  return ret;
}

static inline ptrPo checkStack(processPo P, ptrPo SP) {
  assert(SP > (ptrPo) P->stackBase);
  return SP;
}

static inline void push(processPo P, termPo t) {
  *checkStack(P, --P->sp) = t;
}

ReturnStatus g__callLbl(processPo P, ptrPo tos) {
  integer nmLen;
  const char *nme = stringVal(tos[0], &nmLen);
  integer arity = integerVal(tos[1]);
  listPo args = C_LIST(tos[2]);

  ReturnStatus ret = {.ret=Error, .rslt = voidEnum};

  labelPo lbl = findLbl(nme, arity);
  if (lbl != Null || listSize(args) != arity) {
    methodPo prog = labelCode(lbl); // Which program do we want?

    if (prog == Null) {
      return ret;
    } else {
      P->sp += 3;                   // Remove existing arguments

      for (int ax = 0; ax < arity; ax++)
        push(P, nthEl(args, arity-ax));

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
      ret.ret = Fail;               // Special flag for dynamic call
      return ret;
    }
  } else {
    return ret;
  }
}
