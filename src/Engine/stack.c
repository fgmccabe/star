//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <string.h>
#include <globals.h>
#include "stackP.h"
#include "termP.h"
#include "engineP.h"
#include "heapP.h"

logical traceStacks = False;      // stack operation tracing
long initStackSize = 1024;        /* How big is the stack */
long initThreadStackSize;         // How big is a stacklet stack
long maxStackSize = 100 * 1024 * 1024;     /* 100M cells is default max stack size */

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

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, StackState state, stackPo attachment, termPo prompt) {
  int root = gcAddRoot(H, (ptrPo) &attachment);
  gcAddRoot(H, &prompt);

  stackPo stk = (stackPo) allocateObject(H, stackClass, StackCellCount(sze));
  stk->sze = sze;
  stk->sp = sze;
  stk->fp = 0;
  stk->attachment = attachment;
  stk->state = state;
  stk->hash = stackCount++;
  stk->prompt = prompt;

#ifdef TRACESTACK
  if (traceStacks)
    outMsg(logFile, "establish stack of %d words\n", sze);
#endif

  framePo f = currFrame(stk);
  f->prog = underFlow;
  f->pc = entryPoint(underFlow);
  f->fp = stk->sp;

  gcReleaseRoot(H, root);

  return stk;
}

StackState stackState(stackPo stk) {
  return stk->state;
}

retCode setStackState(stackPo stk, StackState state) {
  switch (stk->state) {
    case detached:
    case suspended:
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

framePo stackFrame(stackPo stk, integer index) {
  assert(index >= 0 && index <= stk->fp);
  return &((framePo) (stk->stack))[index];
}

framePo currFrame(stackPo stk) {
  return stackFrame(stk, stk->fp);
}

framePo pushFrame(stackPo stk, methodPo mtd, integer fp) {
  framePo f = stackFrame(stk, ++stk->fp);
  f->prog = mtd;
  f->pc = entryPoint(mtd);
  f->fp = fp;
  return f;
}

long stkSize(specialClassPo cl, termPo o) {
  return StackCellCount(C_STACK(o)->sze);
}

stackPo attachedStack(stackPo stk) {
  return stk->attachment;
}

void stackSanityCheck(stackPo stk) {
  assert(stk != Null && stk->sp > 0 && stk->sp <= stk->sze && stk->fp > 0 && stk->fp <= stk->sze);
}

void verifyStack(stackPo stk, heapPo H) {
  stackSanityCheck(stk);
  integer ssp = stk->sp;

  for (integer fx = stk->fp; fx >= 0; fx--) {
    framePo frme = stackFrame(stk, fx);
    assert(frme->fp >= ssp && frme->fp < stk->sze);
    ssp = frme->fp;
  }

  for (integer pt = stk->sp; pt < stk->sze; pt++) {
    termPo t = *validStkPtr(stk, pt);
    validPtr(H, t);
  }

  switch (stackState(stk)) {
    case attached:
    case suspended:
      verifyStack(stk->attachment, H);
      break;
    case root:
    case detached:
      return;
  }
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

termPo popStack(stackPo stk) {
  assert(stk->sp < stk->sze);
  return (termPo) stk->stack[stk->sp++];
}

void pushStack(stackPo stk, termPo ptr) {
  assert(stk->sp >= spMin(stk));
  stk->stack[--stk->sp] = (ptrPo) ptr;
}

termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  stackPo stk = C_STACK(o);

  assert(stk != Null);

#ifdef TRACEMEM
  if (traceStacks)
    stackSanityCheck(stk);

  if (traceMemory) {
    outMsg(logFile, "scan stack %d\n%_", stk->hash);
  }
#endif

  for (integer fx = 0; fx <= stk->fp; fx++) {
    framePo f = stackFrame(stk, fx);
    integer off = insOffset(f->prog, f->pc);
    helper((ptrPo) &f->prog, c);
    f->pc = pcAddr(f->prog, off);
  }

  for (integer sx = stk->sp; sx < stk->sze; sx++)
    helper(validStkPtr(stk, sx), c);

  if (stackState(stk) == attached)
    helper((ptrPo) &stk->attachment, c);

  helper((ptrPo) &stk->prompt, c);

  return o + StackCellCount(stk->sze);
}

static char *stackStateName(StackState ste) {
  switch (ste) {
    case root:
      return "root";
    case attached:
      return "attached";
    case detached:
      return "detached";
    default:
      return "unknown state";
  }
}

retCode stkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  stackPo stk = C_STACK(t);
  return outMsg(out, "stack %d:%s %d cells",
                stk->hash, stackStateName(stk->state),
                stk->sze);
}

static MethodRec underFlowMethod = {
  .clss = Null,
  .codeSize = 2,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Underflow, 0}
};

stackPo glueOnStack(heapPo H, stackPo stk, integer size) {
  int root = gcAddRoot(H, (ptrPo) &stk);

  assert(size >= initThreadStackSize);

  stackPo newStack = allocateStack(H, size, &underFlowMethod, attached, stk, Null);

  gcReleaseRoot(H, root);
  return newStack;
}

stackPo spinupStack(heapPo H, stackPo stk, integer size, termPo prompt) {
  assert(size >= initThreadStackSize);

  return allocateStack(H, size, &underFlowMethod, attached, stk, prompt);

}

stackPo attachStack(stackPo stk, stackPo seg) {
#ifdef TRACESTACK
  if (traceStacks)
    outMsg(logFile, "attach stack %T to %T\n", seg, stk);
#endif

  stackPo s = seg;
  stackPo f = s;
  while (s != Null && stackState(s) == suspended) {
    setStackState(s, attached);
    f = s;
    s = s->attachment;
  }
  assert(stackState(f) == attached && f->attachment == Null);
  f->attachment = stk;
  return seg;
}

// Get the stack immediately below the identified prompt
stackPo detachStack(stackPo stk, termPo prompt) {
  #ifdef TRACESTACK
  if (traceStacks)
    outMsg(logFile, "detach stack %T at %T\n", stk, prompt);
#endif
  stackPo s = stk;
  while (s != Null && s->prompt != prompt) {
    s->state = suspended;
    s = stk->attachment;
  }
  if (s == Null || s->state == root)
    return Null;
  else {
    stackPo ss = s->attachment;
    s->attachment = Null;
    s->state = suspended;
    return ss;
  }
}

termPo stackPrompt(stackPo stk) {
  return stk->prompt;
}

void setPrompt(stackPo stk, termPo prompt) {
  stk->prompt = prompt;
}
