//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <globals.h>
#include "stackP.h"
#include "termP.h"
#include "engineP.h"

logical traceStack = False;             // stack operation tracing
integer minStackSize = 256;             /* What is the smallest stack size */
integer defaultStackSize = 4096;        // What is the initial default stack size when running
integer stackRegionSize = (1 << 26);    /* 64M cells is default stack region */

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

static insWord underflowCode[] = {Underflow, 0};
static MethodRec underFlowMethod = {
  .clss = Null,
  .codeSize = 2,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .instructions = underflowCode
};

static insWord newFiberCode[] = {Rot, 0, 1, TOCall, 0, 3};
static MethodRec newFiberMethod = {
  .clss = Null,
  .codeSize = 6,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .instructions = newFiberCode
};
static insWord newTaskCode[] = {Rot, 0, 2, Rot, 0, 1, TOCall, 0, 3};

static MethodRec newTaskMethod = {
  .clss = Null,
  .codeSize = 9,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .instructions = newTaskCode
};

static insWord spawnCode[] = {TOCall, 0, 2};
static MethodRec spawnMethod = {
  .clss = Null,
  .codeSize = 3,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .instructions = spawnCode
};

clssPo stackClass = (clssPo) &StackClass;

static integer stackCount = 0;
#ifdef TRACESTACK
static integer stackReleases = 0;
#endif
static buddyRegionPo stackRegion;

void initStacks() {
  StackClass.clss = specialClass;
  underFlowMethod.clss = methodClass;
  newFiberMethod.clss = methodClass;
  newTaskMethod.clss = methodClass;
  spawnMethod.clss = methodClass;
  integer regionSize = (1 << lg2(stackRegionSize));

#ifdef TRACESTACK
  if (traceStack)
    outMsg(logFile, "setting stack region to %d words\n", regionSize);
#endif

  stackRegion = createRegion(regionSize, minStackSize);
}

stackPo C_STACK(termPo t) {
  assert(isStack(t));
  return (stackPo) t;
}

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, StackState state, stackPo attachment) {
  if (sze > stackRegionSize)
    syserr("tried to allocate too large a stack");

  sze = (1 << lg2Ceiling(sze)) - 1; // Adjust stack size to be just under a power of two

  int root = gcAddRoot(H, (ptrPo) &attachment);

  stackPo stk = (stackPo) allocateObject(H, stackClass, StackCellCount);
  stk->stkMem = (ptrPo) allocateBuddy(stackRegion, sze);

  if (stk->stkMem == Null) {
    syserr("Ran out of stack space");
  }

  stk->sze = sze;
  stk->hwm = sze;
  stk->sp = &stk->stkMem[sze];
  stk->fp = (framePo) stk->sp;
  stk->try = (tryFramePo) stk->sp;
  stk->attachment = attachment;
  stk->bottom = (state == active ? Null : stk);
  stk->state = state;
  stk->hash = stackCount++;
  stk->counter = 0;

#ifdef TRACESTACK
  if (traceStack)
    outMsg(logFile, "establish stack of %d words\n", sze);
#endif

  stk->fp = pushFrame(stk, underFlow, stk->fp);
  gcReleaseRoot(H, root);

  return stk;
}

StackState stackState(stackPo tsk) {
  return tsk->state;
}

retCode setTaskState(stackPo tsk, StackState state) {
  switch (tsk->state) {
    case suspended:
      tsk->state = state;
      return Ok;
    case active:
      if (state != suspended && state != moribund)
        return Error;
      else {
        tsk->state = state;
        return Ok;
      }
    case moribund:
      return Error;
  }
}

framePo currFrame(stackPo stk) {
  return stk->fp;
}

framePo previousFrame(stackPo stk, framePo fp) {
  assert(validFP(stk, fp));
  return fp->fp;
}

framePo dropFrame(stackPo stk) {
  framePo fp = stk->fp;
  assert(validFP(stk, fp));
  assert((ptrPo) stk->try > (ptrPo) stk->fp);
  stk->sp = (ptrPo) (fp + 1) + argCount(fp->prog);
  stk->fp = fp->fp;
  return stk->fp;
}

integer stackHwm(stackPo tsk) {
  return tsk->hwm;
}

void propagateHwm(stackPo tsk) {
  integer hwm = tsk->hwm;

  while (tsk->attachment != Null) {
    tsk = tsk->attachment;
    if (tsk->hwm < hwm)
      tsk->hwm = hwm;
  }
}

framePo pushFrame(stackPo stk, methodPo mtd, framePo fp) {
  framePo f = ((framePo) stk->sp) - 1;
  assert(validFP(stk, fp));

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

void stackSanityCheck(stackPo stk) {
  switch (stk->state) {
    case active:
      assert(stk->bottom == Null);
      break;
    case suspended:
      assert(stk->bottom != Null &&
             (stk->attachment == Null ? isAttachedStack(stk->bottom, stk) : True));
      break;
    case moribund:
      assert(stk->stkMem == Null);
      return;
  }
  assert(validStkPtr(stk, stk->sp) && validFP(stk, stk->fp) && stk->sp <= (ptrPo) stk->fp);
  assert(stk->try <= tryLimit(stk));
  assert((ptrPo) stk->try >= stk->sp);
  assert(!inFreeBlock(stackRegion, stk->stkMem));
}

void verifyStack(stackPo stk, heapPo H) {
  while (stk != Null) {
    assert(classOf((termPo) stk) == stackClass);

    if (stk->stkMem != Null) {
      stackSanityCheck(stk);
      ptrPo sp = stk->sp;
      ptrPo spLimit = stackLimit(stk);
      framePo fp = stk->fp;
      framePo fpLimit = baseFrame(stk);
      tryFramePo try = stk->try;
      tryFramePo trLimit = tryLimit(stk);

      while (fp < fpLimit || sp < spLimit || try < trLimit) {
        if(sp<(ptrPo)try && sp<(ptrPo)fp)
          validPtr(H,*sp++);
        else if(sp==(ptrPo)try){
          check((ptrPo)try<(ptrPo)fp,"out of balance frame");
          check(validFP(stk, try->fp), "invalid try frame pointer");
          sp = (ptrPo) (try + 1);
          try = try->try;
        } else{
          check(sp==(ptrPo)fp,"expecting a frame here");
          check(isMethod((termPo)fp->prog),"expecting a code pointer in the frame");
          check(validFP(stk, fp->fp), "invalid fp in frame");
          sp = (ptrPo) (fp + 1);
          fp = fp->fp;
        }
      }

      stk = stk->attachment;
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
  return hash61(C_STACK(o)->hash);
}

integer stackNo(stackPo tsk) {
  return tsk->hash;
}

termPo popStack(stackPo stk) {
  assert(validStkPtr(stk, stk->sp));
  return *stk->sp++;
}

termPo topStack(stackPo stk) {
  assert(validStkPtr(stk, stk->sp));
  return stk->sp[0];
}

termPo peekStack(stackPo stk, integer delta) {
  assert(validStkPtr(stk, stk->sp + delta));
  return stk->sp[delta];
}

void pushStack(stackPo stk, termPo ptr) {
  *--stk->sp = ptr;
  assert(validStkPtr(stk, stk->sp));
}

integer pushTryFrame(stackPo stk, processPo P, insPo pc, ptrPo sp, framePo fp) {
  tryFramePo try = ((tryFramePo) stk->sp) - 1;

  try->fp = fp;
  try->pc = pc;
  stk->sp = (ptrPo) try;
  try->try = stk->try;
  stk->try = try;
  return try->tryIndex = P->tryCounter++;
}

stackPo popTryFrame(processPo P, integer tryIndex) {
  stackPo stk = P->stk;

  while (stk != Null) {
    tryFramePo try = stk->try;

    while (try < tryLimit(stk)) {
      if (try->tryIndex == tryIndex) {

        stk->fp = try->fp;
        stk->sp = (ptrPo) (try + 1);
        stk->fp->pc = try->pc;
        stk->try = try->try;
        validPC(stk->fp->prog, stk->fp->pc);
        return stk;
      } else
        try = stk->try = try->try;
    }
    if (stk->attachment != Null) {
      stk = P->stk = dropStack(stk);
    } else
      return Null;
  }
  return Null;
}

// Drop the try block without touching anything else
retCode dropTryFrame(processPo P, integer tryIndex) {
  stackPo stk = P->stk;

  while (stk != Null) {
    tryFramePo try = stk->try;

    while (try < tryLimit(stk)) {
      if (try->tryIndex == tryIndex) {
        stk->try = try->try;
        return Ok;
      } else
        try = stk->try = try->try;
    }
    if (stk->attachment != Null) {
      stk = stk->attachment;
    } else
      return Error;
  }
  return Error;
}

integer tryStackSize(processPo P) {
  integer size = 0;
  stackPo stk = P->stk;
  while (stk != Null) {
    tryFramePo try = stk->try;
    while (try < tryLimit(stk)) {
      size++;
      try = try->try;
    }
    stk = stk->attachment;
  }
  return size;
}

void moveStack2Stack(stackPo totsk, stackPo fromtsk, integer count) {
  assert(validStkPtr(fromtsk, fromtsk->sp + count));
  assert(stackHasSpace(totsk, count));

  ptrPo src = fromtsk->sp + count;
  ptrPo dst = totsk->sp;
  for (integer ix = count; ix > 0; ix--) {
    *--dst = *--src;
  }
  totsk->sp = dst;
  fromtsk->sp += count;
}

termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  stackPo stk = C_STACK(o);

  assert(stk != Null);

  if (stk->stkMem != Null) {
    ptrPo sp = stk->sp;
    ptrPo spLimit = stackLimit(stk);
    framePo fp = stk->fp;
    framePo fpLimit = baseFrame(stk);
    tryFramePo try = stk->try;
    tryFramePo tryLimit = (tryFramePo) spLimit;

    while (fp < fpLimit || sp < spLimit || try < tryLimit) {
      if ((ptrPo) fp <= sp) {
        if ((ptrPo) try < (ptrPo) fp) {
          assert(False);
        } else {
          helper((ptrPo) &fp->prog, c);
          sp = (ptrPo) (fp + 1);
          fp = fp->fp;
        }
      } else if (sp < (ptrPo) try) {
        helper(sp++, c);
      } else {
        assert(sp == (ptrPo) try);
        sp = (ptrPo) (try + 1);
        try = try->try;
      }
    }

    assert(fp == fpLimit && sp == spLimit && try == tryLimit);
  }

  if (stk->attachment != Null)
    helper((ptrPo) &stk->attachment, c);
  if (stk->bottom != Null)
    helper((ptrPo) &stk->bottom, c);

#ifdef TRACESTACK
  if (traceStack)
    stackSanityCheck(stk);
#endif

  return o + StackCellCount;
}

termPo stkFinalizer(specialClassPo class, termPo o) {
  stackPo tsk = C_STACK(o);
  if (tsk->stkMem != Null) {
    releaseBlock(stackRegion, (voidPtr) tsk->stkMem);
    tsk->stkMem = Null;
  }
  return o + StackCellCount;
}

char *stackStateName(StackState ste) {
  switch (ste) {
    case active:
      return "active";
    case suspended:
      return "suspended";
    case moribund:
      return "moribund";
    default:
      return "unknown state";
  }
}

retCode stkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  stackPo stk = C_STACK(t);

  return outMsg(out, "(.stack %d:[%s] %T.)",
                stk->hash,
                stackStateName(stk->state),
                stackState(stk) == moribund ? voidEnum : (termPo) (stk->fp->prog));
}

void showStackCall(ioPo out, integer depth, framePo fp, stackPo stk, integer frameNo, StackTraceLevel tracing) {
  methodPo mtd = fp->prog;
  assert(isMethod((termPo) mtd));
  if (normalCode(mtd)) {
    insPo pc = fp->pc;
    integer pcOffset = insOffset(mtd, pc);

    termPo locn = findPcLocation(mtd, pcOffset);
    if (locn != Null)
      outMsg(out, "[%d] %#L: %T", frameNo, locn, mtd);
    else
      outMsg(out, "[%d] (unknown loc): %T[%d]", frameNo, mtd, pcOffset);

    integer count = argCount(mtd);

    switch (tracing) {
      default:
      case showPrognames: {
        outMsg(out, "\n");
        break;
      }
      case showArguments: {
        if (depth > 0) {
          outMsg(out, "(");
          char *sep = "";
          for (integer ix = 0; ix < count; ix++) {
            outMsg(out, "%s%,*T", sep, depth - 1, *stackArg(stk, fp, ix));
            sep = ", ";
          }
          outMsg(out, ")\n");
        } else
          outMsg(out, "\n");
        break;
      }
      case showLocalVars: {
        outMsg(out, "(");
        char *sep = "";
        for (integer ix = 0; ix < count; ix++) {
          outMsg(out, "%s%,*T", sep, depth - 1, *stackArg(stk, fp, ix));
          sep = ", ";
        }
        outMsg(out, ")\n");
        count = lclCount(mtd);

        for (integer vx = 1; vx <= count; vx++) {
          ptrPo var = stackLcl(stk, fp, vx);
          if (*var != Null && *var != voidEnum)
            outMsg(out, "  L[%d] = %,*T\n", vx, depth - 1, *var);
        }
      }
    }
  }
}

void stackTrace(processPo p, ioPo out, stackPo stk, integer depth, StackTraceLevel tracing) {
  outMsg(out, "Stack trace for process %d\n", p->processNo);

  integer frameNo = 0;

  do {
    framePo fp = stk->fp;

    while (fp < baseFrame(stk)) {
      showStackCall(out, depth, fp, stk, frameNo++, tracing);
      fp = fp->fp;
    }

    stk = stk->attachment;
  } while (stk != Null);
}

integer stackDepth(stackPo stk, methodPo mtd, ptrPo sp, framePo fp) {
  integer count = 0;
  tryFramePo try = stk->try;
  ptrPo limit = (ptrPo) fp - lclCount(mtd);
  while (sp < limit) {
    while (sp == (ptrPo) try) {
      sp = (ptrPo) (try + 1);
      try = try->try;
    }
    if (sp < limit) {
      count++;
      sp++;
    }
  }
  return count;
}

stackPo glueOnStack(heapPo H, stackPo tsk, integer size, integer saveArity) {
  int root = gcAddRoot(H, (ptrPo) &tsk);

  assert(size >= minStackSize && stackState(tsk) != moribund);

  stackPo newStack = allocateStack(H, size, &underFlowMethod, stackState(tsk), tsk);
  moveStack2Stack(newStack, tsk, saveArity);
  propagateHwm(newStack);
  gcReleaseRoot(H, root);
  return newStack;
}

stackPo spinupStack(heapPo H, integer size) {
  assert(size >= minStackSize);

  return allocateStack(H, size, &underFlowMethod, suspended, Null);
}

stackPo newFiber(heapPo H, termPo lam) {
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, &newTaskMethod, child->fp);

  pushStack(child, lam);
  pushStack(child, (termPo) child);

  return child;                                                 // We return the new stack
}

stackPo newStack(heapPo H, termPo lam) {
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, &newTaskMethod, child->fp);

  pushStack(child, lam);
  pushStack(child, (termPo) child);

  return child;                                                 // We return the new stack
}

stackPo splitStack(processPo P, termPo lam) {
  heapPo H = P->heap;
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, &spawnMethod, child->fp);

  pushStack(child, (termPo) child);
  pushStack(child, lam);

  return child;                                                 // We return the new stack
}

stackPo attachStack(stackPo tsk, stackPo top) {
  stackPo bottom = top->bottom;
  assert(bottom != Null && isAttachedStack(bottom, top));

#ifdef TRACESTACK
  if (traceStack)
    outMsg(logFile, "attach stack %T to %T\n", top, tsk);
#endif

  assert(stackState(tsk) == active && stackState(top) == suspended && stackState(bottom) == suspended);

  stackPo f = bottom;

  while (f != top) {
    setTaskState(f, active);
    f->bottom = Null;
    f = f->attachment;
  }
  setTaskState(f, active);

  assert(f == top && top->attachment == Null);
  top->attachment = tsk;
  top->bottom = Null;
  return bottom;
}

// Get the stack immediately below the identified parent
stackPo detachStack(stackPo base, stackPo top) {
#ifdef TRACESTACK
  if (traceStack)
    outMsg(logFile, "detach %T up to %T\n", base, top);
#endif
  assert(stackState(top) == active && top->bottom == Null);
  assert(isAttachedStack(base, top));

  top->bottom = base;
  stackPo s = base;
  while (s != Null && s != top) {
    assert(s->state == active);
    s->state = suspended;
    s->bottom = base;
    s = s->attachment;
  }

  assert(s == top);
  stackPo parent = top->attachment;
  s->attachment = Null;
  s->state = suspended;
  s->bottom = base;

#ifdef TRACESTACK
  if (traceStack) {
    outMsg(logFile, "now at %T\n", parent);
    assert(hasClass((termPo) parent, stackClass));
  }
#endif
  return parent;
}

stackPo dropStack(stackPo tsk) {
#ifdef TRACESTACK
  if (traceStack)
    outMsg(logFile, "drop stack %T\n%_", tsk);
  stackReleases++;
#endif
  stackPo previous = tsk->attachment;
  tsk->attachment = Null;
  tsk->bottom = Null;
  tsk->state = moribund;
  releaseBlock(stackRegion, (voidPtr) tsk->stkMem);
  tsk->stkMem = Null;
  tsk->sze = -1;
  return previous;
}

logical isAttachedStack(stackPo base, stackPo tgt) {
  while (base != Null && base != tgt)
    base = base->attachment;
  return base == tgt;
}

void dumpStackStats(ioPo out) {
#ifdef TRACESTACK
  outMsg(out, "%d stacks allocated\n", stackCount);
  outMsg(out, "%d stacks dropped\n", stackReleases);
#endif
}
