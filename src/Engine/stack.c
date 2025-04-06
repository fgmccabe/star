//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <globals.h>
#include "stackP.h"
#include "engineP.h"
#include "debugP.h"

#ifdef TRACESTACK
tracingLevel traceStack = noTracing;    // stack operation tracing
#endif

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

static labelPo underflowProg;
static labelPo taskProg;
static labelPo spawnProg;

static Instruction underflowCode[] = {Underflow, 0};

static Instruction newFiberCode[] = {Rot, 1, 0, TOCall, 3, 0};

static Instruction newTaskCode[] = {Rot, 2, 0, Rot, 1, 0, TOCall, 3, 0};

static Instruction spawnCode[] = {TOCall, 2, 0};

clssPo stackClass = (clssPo) &StackClass;

static integer stackCount = 0;
#ifdef TRACESTACK
static integer stackReleases = 0;
#endif
static buddyRegionPo stackRegion;

void initStacks() {
  StackClass.clss.clss = specialClass;

  underflowProg = specialMethod("underflow", 0, NumberOf(underflowCode), underflowCode, NULL, 0);
  taskProg = specialMethod("newTask", 0, NumberOf(newTaskCode), newTaskCode, NULL, 0);
  spawnProg = specialMethod("spawn", 0, NumberOf(spawnCode), spawnCode, NULL, 0);

  integer regionSize = (1 << lg2(stackRegionSize));

#ifdef TRACESTACK
  if (traceStack > noTracing)
    outMsg(logFile, "setting stack region to %d words\n", regionSize);
#endif

  stackRegion = createRegion(regionSize, minStackSize);
}

stackPo C_STACK(termPo t) {
  assert(isStack(t));
  return (stackPo) t;
}

stackPo allocateStack(heapPo H, integer sze, labelPo underFlow, StackState state, stackPo attachment) {
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
  stk->fp = baseFrame(stk);
  stk->tp = baseTry(stk);
  stk->attachment = attachment;
  stk->bottom = (state == active ? Null : stk);
  stk->state = state;
  stk->hash = stackCount++;

#ifdef TRACESTACK
  if (traceStack > noTracing)
    outMsg(logFile, "new stack of %d words\n%_", sze);
#endif

  stk->fp = pushFrame(stk, labelCode(underFlow));
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
  assert((ptrPo) stk->tp < (ptrPo) stk->fp);
  stk->sp = fp->args + argCount(frameMtd(fp));
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

framePo pushFrame(stackPo stk, methodPo mtd) {
  assert(stackHasSpace(stk, FrameCellCount));
  framePo f = (framePo) maxPtr(((ptrPo) (stk->fp + 1)), ((ptrPo) (stk->tp + 1)));

  f->prog = mtd;
  f->pc = entryPoint(mtd);
  f->fp = stk->fp;
  f->args = stk->sp;

  stk->fp = f;

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
  assert(stk->fp >= baseFrame(stk) && ((ptrPo) (stk->fp + 1)) <= stk->sp);
  assert(stk->tp >= baseTry(stk));
  assert((ptrPo) (stk->tp + 1) < stk->sp);
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
      tryFramePo try = stk->tp;
      tryFramePo trLimit = baseTry(stk);

      while (fp > fpLimit || try > trLimit) {
        if ((ptrPo) try > (ptrPo) fp) {  // We have a tryFrame to check
          check(validFP(stk, try->fp), "invalid try frame pointer");
          check(try->try < try, "try frame pointer not pointing to earlier frame");
          check(try->sp >= sp, "try sp not in order");
          check(try->fp <= fp, "try fp out of order");
          sp = try->sp;
          try = try->try;
        } else {
          check((ptrPo) fp > (ptrPo) try, "overlapping try/frame");
          check(isMethod((termPo) frameMtd(fp)), "expecting a code pointer in the frame");
          check(validFP(stk, fp->fp), "invalid fp in frame");
          check(fp->args >= sp, "frame arg pointer invalid");
          sp = fp->args + argCount(frameMtd(fp));
          fp = fp->fp;
        }
      }

      // Walk the value stack...
      for (ptrPo p = stk->sp; p < spLimit; p++)
        validPtr(H, *p);
    }
    stk = stk->attachment;
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
  assert(stackHasSpace(stk, TryFrameCellCount));
  tryFramePo try = (tryFramePo) maxPtr(((ptrPo) (stk->fp + 1)), ((ptrPo) (stk->tp + 1)));

  try->fp = fp;
  try->pc = pc;
  try->sp = sp;
  try->try = stk->tp;
  stk->tp = try;
  return try->tryIndex = P->tryCounter++;
}

stackPo popTryFrame(processPo P, integer tryIndex) {
  stackPo stk = P->stk;

  while (stk != Null) {
    tryFramePo try = stk->tp;

    while (try < tryLimit(stk)) {
      if (try->tryIndex == tryIndex) {

        stk->fp = try->fp;
        stk->sp = try->sp;
        stk->fp->pc = try->pc;
        stk->tp = try->try;
        assert(validPC(frameMtd(stk->fp), stk->fp->pc));
        return stk;
      } else
        try = stk->tp = try->try;
    }
    if (stk->attachment != Null) {
      stk = P->stk = dropStack(stk);
    } else
      return Null;
  }
  return Null;
}

integer tryStackDepth(processPo P) {
  integer count = 0;
  stackPo stk = P->stk;

  while (stk != Null) {
    tryFramePo try = stk->tp;
    while (try > baseTry(stk)) {
      count++;
      try = try->try;
    }
    stk = stk->attachment;
  }
  return count;
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
    tryFramePo try = stk->tp;
    tryFramePo tpLimit = baseTry(stk);

    while (fp > fpLimit || try > tpLimit) {
      if ((ptrPo) fp > (ptrPo) try) {
        assert(fp > fpLimit);
        helper((ptrPo) &fp->prog, c);
        fp = fp->fp;
      } else if ((ptrPo) try > (ptrPo) fp) {
        assert(try > tpLimit);
        try = try->try;
      }
    }

    // Walk the value stack...
    for (sp = stk->sp; sp < spLimit; sp++)
      helper(sp, c);

    assert(fp == fpLimit && sp == spLimit && try == tpLimit);

    if (stk->attachment != Null)
      helper((ptrPo) &stk->attachment, c);
    if (stk->bottom != Null)
      helper((ptrPo) &stk->bottom, c);

#ifdef TRACESTACK
    if (traceStack > noTracing)
      stackSanityCheck(stk);
#endif
  }

  return o + StackCellCount;
}

termPo stkFinalizer(specialClassPo class, termPo o) {
  stackPo tsk = C_STACK(o);
  if (tsk->stkMem != Null) {
    tsk->state = moribund;
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
                stackState(stk) == moribund ? voidEnum : (termPo) frameMtd(stk->fp));
}

void showStackCall(ioPo out, integer depth, framePo fp, stackPo stk, integer frameNo, StackTraceLevel tracing) {
  methodPo mtd = frameMtd(fp);
  assert(isMethod((termPo) mtd));
  insPo pc = fp->pc;
  termPo loc = findPcLocation(mtd, codeOffset(mtd, pc));

  if (loc != Null)
    outMsg(out, "[%d] %L: %T", frameNo, loc, mtd);
  else
    outMsg(out, "[%d] (unknown loc): %T[%d]", frameNo, mtd, codeOffset(mtd, pc));

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
          outMsg(out, "%s%,*T", sep, depth - 1, *stackArg(fp, ix));
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
        outMsg(out, "%s%,*T", sep, depth - 1, *stackArg(fp, ix));
        sep = ", ";
      }
      outMsg(out, ")\n");
      count = lclCount(mtd);

      for (integer vx = 1; vx <= count; vx++) {
        ptrPo var = stackLcl(fp, vx);
        if (*var != Null && *var != voidEnum)
          outMsg(out, "  L[%d] = %,*T\n", vx, depth - 1, *var);
      }
    }
  }
}

void stackTrace(processPo p, ioPo out, stackPo stk, integer depth, StackTraceLevel tracing, integer maxDepth) {
  outMsg(out, "Stack trace for process %d\n", p->processNo);

  integer frameNo = 0;

  do {
    framePo fp = stk->fp;

    while (fp > baseFrame(stk) && maxDepth-- > 0) {
      showStackCall(out, depth, fp, stk, frameNo++, tracing);
      fp = fp->fp;
    }

    stk = stk->attachment;
  } while (stk != Null && maxDepth > 0);

  if (maxDepth <= 0)
    outMsg(out, "...\n");
}

int32 stackDepth(stackPo stk, methodPo mtd, ptrPo sp, framePo fp) {
  return stackLcl(fp, lclCount(frameMtd(fp))) - sp;
}

stackPo glueOnStack(heapPo H, stackPo stk, integer size, integer saveArity) {
  int root = gcAddRoot(H, (ptrPo) &stk);

  assert(size >= minStackSize && stackState(stk) != moribund);

  stackPo newStack = allocateStack(H, size, underflowProg, stackState(stk), stk);
  moveStack2Stack(newStack, stk, saveArity);
  propagateHwm(newStack);
  gcReleaseRoot(H, root);
  return newStack;
}

stackPo handleStackOverflow(stackPo stk, integer delta, methodPo mtd) {
  int root = gcAddRoot(globalHeap, (ptrPo) &stk);

  stackPo prevStack = stk;

  gcAddRoot(globalHeap, (ptrPo) &prevStack);

  stk = glueOnStack(globalHeap, stk, (stk->sze * 3) / 2 + delta, codeArity(mtd));
  pushFrame(stk, mtd);

  // drop old frame on old stack
  dropFrame(prevStack);
  gcReleaseRoot(globalHeap, root);
  return stk;
}

stackPo spinupStack(heapPo H, integer size) {
  assert(size >= minStackSize);

  return allocateStack(H, size, underflowProg, suspended, Null);
}

stackPo newFiber(heapPo H, termPo lam) {
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, labelCode(taskProg));

  pushStack(child, lam);
  pushStack(child, (termPo) child);

  return child;                                                 // We return the new stack
}

stackPo newStack(heapPo H, termPo lam) {
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, labelCode(taskProg));

  pushStack(child, lam);
  pushStack(child, (termPo) child);

  return child;                                                 // We return the new stack
}

stackPo splitStack(processPo P, closurePo lam) {
  heapPo H = P->heap;
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, labelCode(spawnProg));

  pushStack(child, (termPo) child);
  pushStack(child, (termPo) lam);

  return child;                                                 // We return the new stack
}

stackPo attachStack(stackPo tsk, stackPo top) {
  stackPo bottom = top->bottom;
  assert(bottom != Null && isAttachedStack(bottom, top));

#ifdef TRACESTACK
  if (traceStack > noTracing)
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
  if (traceStack > noTracing)
    outMsg(logFile, "detach %T up to %T\n", base, top);
#endif
  assert(stackState(top) == active && top->bottom == Null);
  assert(isAttachedStack(base, top));

  top->bottom = base;
  stackPo s = base;
  while (s != top) {
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
  if (traceStack > noTracing) {
    outMsg(logFile, "now at %T\n", parent);
    assert(hasClass((termPo) parent, stackClass));
  }
#endif
  return parent;
}

stackPo dropStack(stackPo tsk) {
#ifdef TRACESTACK
  if (traceStack > noTracing)
    outMsg(logFile, "%ld drop stack\n%_", pcCount);
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
