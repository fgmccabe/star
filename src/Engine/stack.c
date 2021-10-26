//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <globals.h>
#include "stackP.h"
#include "termP.h"
#include "engineP.h"
#include "heapP.h"
#include "buddy.h"

logical traceStacks = False;          // stack operation tracing
integer minStackSize = 128;           /* What is the smallest stack size */
integer stackRegionSize = (1 << 23);     /* 64M cells is default max stack size */

static long stkSize(specialClassPo cl, termPo o);
static termPo stkCopy(specialClassPo cl, termPo dst, termPo src);
static termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical stkCmp(specialClassPo cl, termPo o1, termPo o2);
static integer stkHash(specialClassPo cl, termPo o);
static retCode stkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo stkFinalizer(specialClassPo class, termPo o);

SpecialClass StackClass = {
  .clss = Null,
  .sizeFun = stkSize,
  .copyFun = stkCopy,
  .scanFun = stkScan,
  .finalizer = stkFinalizer,
  .compFun = stkCmp,
  .hashFun = stkHash,
  .dispFun = stkDisp
};

static MethodRec underFlowMethod = {
  .clss = Null,
  .codeSize = 2,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Underflow, 0}
};

clssPo stackClass = (clssPo) &StackClass;

static integer stackCount = 0;
static buddyRegionPo stackRegion;

static framePo firstFrame(stackPo stk);

void initStacks() {
  StackClass.clss = specialClass;
  underFlowMethod.clss = methodClass;
  integer regionSize = (1 << lg2(stackRegionSize));

#ifdef TRACESTACK
  if (traceStacks)
    outMsg(logFile, "setting stack region to %d words\n", regionSize);
#endif

  stackRegion = createRegion(regionSize, minStackSize);
}

stackPo C_STACK(termPo t) {
  assert(hasClass(t, stackClass));
  return (stackPo) t;
}

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, StackState state, stackPo attachment, termPo prompt) {
  if (sze > stackRegionSize)
    syserr("tried to allocate too large a stack");

  sze = (1 << lg2(2 * sze - 1)) - 1; // Adjust stack size to be just under a power of two

  int root = gcAddRoot(H, (ptrPo) &attachment);
  gcAddRoot(H, &prompt);

  stackPo stk = (stackPo) allocateObject(H, stackClass, StackCellCount);
  stk->stkMem = (ptrPo) allocateBuddy(stackRegion, sze);

  if(stk->stkMem==Null){
    syserr("Ran out of stack space");
  }

  stk->sze = sze;
  stk->hwm = sze;
  stk->sp = &stk->stkMem[sze];
  stk->fp = (framePo) &stk->stkMem[sze];
  stk->attachment = attachment;
  stk->state = state;
  stk->hash = stackCount++;
  stk->prompt = prompt;

#ifdef TRACESTACK
  if (traceStacks)
    outMsg(logFile, "establish stack of %d words\n", sze);
#endif

  pushFrame(stk, underFlow, stk->fp, stk->sp);
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

framePo currFrame(stackPo stk) {
  return stk->fp;
}

integer stackHwm(stackPo stk) {
  return stk->hwm;
}

void propagateHwm(stackPo stk) {
  integer hwm = stk->hwm;

  while (stk->attachment != Null) {
    stk = stk->attachment;
    if (stk->hwm < hwm)
      stk->hwm = hwm;
  }
}

framePo pushFrame(stackPo stk, methodPo mtd, framePo fp, ptrPo sp) {
  framePo f = (framePo) (sp) - 1;
  assert(f >= (framePo) (stk->stkMem));
  f->prog = mtd;
  f->pc = entryPoint(mtd);
  f->fp = fp;
  stk->fp = f;
  stk->sp = (ptrPo) f;
  return f;
}

long stkSize(specialClassPo cl, termPo o) {
  return StackCellCount;
}

stackPo attachedStack(stackPo stk) {
  return stk->attachment;
}

void stackSanityCheck(stackPo stk) {
  assert(
    stk != Null && stk->sp >= stk->stkMem && stk->sp <= &stk->stkMem[stk->sze] && stk->fp >= (framePo) stk->stkMem &&
    stk->fp <= (framePo) &stk->stkMem[stk->sze]);
  if (stk->stkMem != Null)
    assert(!inFreeBlock(stackRegion, stk->stkMem));
}

void verifyStack(stackPo stk, heapPo H) {
  if (stk->stkMem != Null) {
    stackSanityCheck(stk);
    ptrPo sp = stk->sp;
    framePo fp = stk->fp;
    ptrPo limit = stk->stkMem + stk->sze;

    while (sp < limit) {
      assert(sp <= (ptrPo) fp);
      while (sp < (ptrPo) fp)
        validPtr(H, *sp++);
      sp = (ptrPo) (fp + 1);
      fp = fp->fp;
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
}

termPo stkCopy(specialClassPo cl, termPo dst, termPo src) {
  stackPo ss = C_STACK(src);
  stackPo ds = (stackPo) dst; // Dest not yet a valid stack structure
  *ds = *ss;                  // Copy the structural part

  return ((termPo) ds) + StackCellCount;
}

logical stkCmp(specialClassPo cl, termPo o1, termPo o2) {
  return o1 == o2;
}

integer stkHash(specialClassPo cl, termPo o) {
  return C_STACK(o)->hash;
}

integer stackNo(stackPo stk) {
  return stk->hash;
}

termPo popStack(stackPo stk) {
  assert(stk->sp < stackLimit(stk));
  return *stk->sp++;
}

termPo topStack(stackPo stk) {
  assert(stk->sp < (ptrPo) stk->fp);
  return stk->sp[0];
}

termPo peekStack(stackPo stk, integer delta) {
  assert(stk->sp + delta < (ptrPo) stk->fp);
  return stk->sp[delta];
}

void pushStack(stackPo stk, termPo ptr) {
  assert(stk->sp > stk->stkMem);
  *--stk->sp = ptr;
}

void moveStack2Stack(stackPo toStk, stackPo fromStk, integer count) {
  assert(validStkPtr(fromStk, fromStk->sp + count));
  assert(stkHasSpace(toStk, count));

  ptrPo src = fromStk->sp + count;
  ptrPo dst = toStk->sp;
  for (integer ix = count; ix > 0; ix--) {
    *--dst = *--src;
  }
  toStk->sp = dst;
  fromStk->sp += count;
}

termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  stackPo stk = C_STACK(o);

  assert(stk != Null);

  if (stk->stkMem != Null) {
#ifdef TRACEMEM
    if (traceStacks)
      stackSanityCheck(stk);

    if (traceMemory) {
      outMsg(logFile, "scan stack %d\n%_", stk->hash);
    }
#endif

    ptrPo sp = stk->sp;
    framePo fp = stk->fp;
    ptrPo limit = stk->stkMem + stk->sze;

    while (sp < limit) {
      assert(sp <= (ptrPo) fp);
      while (sp < (ptrPo) fp)
        helper(sp++, c);

      integer off = insOffset(fp->prog, fp->pc);
      helper((ptrPo) &fp->prog, c);
      fp->pc = pcAddr(fp->prog, off);

      sp = (ptrPo) (fp + 1);
      fp = fp->fp;
    }
  }

  if (stackState(stk) == attached)
    helper((ptrPo) &stk->attachment, c);

  helper((ptrPo) &stk->prompt, c);

  return o + StackCellCount;
}

termPo stkFinalizer(specialClassPo class, termPo o) {
  stackPo stk = C_STACK(o);
  if (stk->stkMem != Null) {
    release(stackRegion, (voidPtr) stk->stkMem);
    stk->stkMem = Null;
  }
  return o + StackCellCount;
}

static char *stackStateName(StackState ste) {
  switch (ste) {
    case root:
      return "root";
    case attached:
      return "attached";
    case detached:
      return "detached";
    case suspended:
      return "suspended";
    default:
      return "unknown state";
  }
}

retCode stkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  stackPo stk = C_STACK(t);
  framePo first = firstFrame(stk);

  if (first != Null && first != stk->fp)
    return outMsg(out, "(.stack %d:[%s] %M .. %M.)",
                  stk->hash,
                  stackStateName(stk->state),
                  stk->fp->prog, first->prog);
  else
    return outMsg(out, "(.stack %d:[%s] %M.)",
                  stk->hash,
                  stackStateName(stk->state),
                  stk->fp->prog);
}

stackPo glueOnStack(heapPo H, stackPo stk, integer size, integer saveArity) {
  int root = gcAddRoot(H, (ptrPo) &stk);

  assert(size >= minStackSize);

  stackPo newStack = allocateStack(H, size, &underFlowMethod, attached, stk, Null);
  moveStack2Stack(newStack, stk, saveArity);
  propagateHwm(newStack);
  gcReleaseRoot(H, root);
  return newStack;
}

stackPo spinupStack(heapPo H, stackPo stk, integer size, termPo prompt) {
  assert(size >= minStackSize);

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
    outMsg(logFile, "detach %T at prompt %T\n", stk, prompt);
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

stackPo dropStack(stackPo stk) {
#ifdef TRACESTACK
  if (traceStacks)
    outMsg(logFile, "drop stack %T\n%_", stk);
#endif
  stackPo previous = stk->attachment;
  stk->state = detached;
  release(stackRegion, (voidPtr) stk->stkMem);
  stk->stkMem = Null;
  stk->sze = -1;
  return previous;
}

termPo stackPrompt(stackPo stk) {
  return stk->prompt;
}

void setPrompt(stackPo stk, termPo prompt) {
  stk->prompt = prompt;
}

framePo firstFrame(stackPo stk) {
  ptrPo sp = stk->sp;
  framePo fp = stk->fp;
  framePo f = fp;
  framePo limit = (framePo) (stk->stkMem + stk->sze);

  while (fp < limit) {
    f = fp;
    fp = fp->fp;
  }

  if (f < limit)
    return f;
  else
    return Null;
}
