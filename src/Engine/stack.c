//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <string.h>
#include "stackP.h"
#include "termP.h"
#include "engineP.h"

static long stkSize(specialClassPo cl, termPo o);
static termPo stkCopy(specialClassPo cl, termPo dst, termPo src);
static termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical stkCmp(specialClassPo cl, termPo o1, termPo o2);
static integer stkHash(specialClassPo cl, termPo o);
static retCode stkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass StackClass = {
  .clss = Null,
  .sizeFun = stkSize,
  .copyFun = stkCopy,
  .scanFun = stkScan,
  .compFun = stkCmp,
  .hashFun = stkHash,
  .dispFun = stkDisp
};

clssPo stackClass = (clssPo) &StackClass;

static integer stackCount = 0;

void initStacks() {
  StackClass.clss = specialClass;
}

stackPo C_STACK(termPo t) {
  assert(hasClass(t, stackClass));
  return (stackPo) t;
}

stackPo allocateStack(heapPo H, integer sze) {
  stackPo stk = (stackPo) allocateObject(H, stackClass, StackCellCount(sze));
  stk->sze = sze;
  stk->tos = sze;
  stk->fp = sze;
  stk->attachment = Null;
  stk->state = detached;
  stk->hash = stackCount++;
  return stk;
}

StackState stackState(stackPo stk) {
  return stk->state;
}

retCode setStackState(stackPo stk, StackState state) {
  switch (stk->state) {
    case detached:
      stk->state = state;
      return Ok;
    case attached:
      if (state != detached)
        return Error;
      else {
        stk->state = state;
        return Ok;
      }
    case root:
      return Error;
  }
}

long stkSize(specialClassPo cl, termPo o) {
  return StackCellCount(C_STACK(o)->sze);
}

stackPo attachedStack(stackPo stk) {
  return stk->attachment;
}

integer stackTos(stackPo stk) {
  assert(stk != Null && stk->tos >= 0 && stk->tos <= stk->sze);
  return stk->tos;
}

integer stackFp(stackPo stk) {
  assert(stk != Null && stk->fp >= 0 && stk->fp <= stk->tos);
  return stk->fp;
}

ptrPo stackTerm(stackPo stk, integer off) {
  assert(stk != Null && off >= 0 && off <= stk->tos);
  return (ptrPo) &stk->stack[off];
}

framePo stackFrame(stackPo stk, integer off) {
  assert(stk != Null && off >= 0 && off <= stk->tos);
  return (framePo) &stk->stack[off];
}

termPo stkCopy(specialClassPo cl, termPo dst, termPo src) {
  stackPo ss = C_STACK(src);
  stackPo ds = (stackPo) dst; // Dest not yet a valid stack structure
  *ss = *ss;                  // Copy the structural part

  memcpy(ds->stack, ss->stack, ss->sze * sizeof(ptrPo)); // Could make this smaller

  return ((termPo) ds) + StackCellCount(ss->sze);
}

logical stkCmp(specialClassPo cl, termPo o1, termPo o2) {
  return o1 == o2;
}

integer stkHash(specialClassPo cl, termPo o) {
  return C_STACK(o)->hash;
}

termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  stackPo stk = C_STACK(o);

  assert(stk != Null && stk->tos <= stk->sze);

#ifdef TRACEMEM
  if (traceMemory) {
    outMsg(logFile, "scan stack %d\n%_", stk->hash);
  }
#endif

  integer limit = stk->sze;
  integer fp = stackFp(stk);
  integer sp = stackTos(stk);

  while (fp < limit) {
    while (sp < fp) {
      ptrPo t = stackTerm(stk, sp);
      helper(t, c);
      sp++;
    }
    framePo f = stackFrame(stk, fp);
    integer off = insOffset(f->prog, f->rtn);
    helper((ptrPo) &f->prog, c);
    f->rtn = pcAddr(f->prog, off);

    sp = fp + STACKFRAME_SIZE;
    fp = f->fp;
  }
  while (sp < limit) {
    helper((ptrPo) stackTerm(stk, sp), c);
    sp++;
  }

  if (attachedStack(stk) != Null)
    helper((ptrPo) &stk->attachment, c);

  helper((ptrPo)&stk->prog,c);

  return o + StackCellCount(stk->sze);
}

static char *stackStateName(StackState ste) {
  switch (ste) {
    case root:
      return "root";
    case attached:
      return "attached";
    case detached:
      return "detaached";
    default:
      return "unknown state";
  }
}

retCode stkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  stackPo stk = C_STACK(t);
  return outMsg(out, "stack %d:%s %d cells (%f2.2 used)",
                stk->hash, stackStateName(stk->state),
                stk->sze, ((double) stk->tos) / stk->sze * 100.0);
}

stackPo glueOnStack(heapPo H, stackPo stk, integer amt) {
  int root = gcAddRoot(H, (ptrPo) &stk);

  stackPo newStack = allocateStack(H,amt);

  newStack->attachment = stk;
  newStack->state = attached;



  gcReleaseRoot(H, root);
  return newStack;
}
