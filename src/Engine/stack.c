//
// Created by Francis McCabe on 2/19/21.
//

#include "debugP.h"
#include "engineP.h"
#include "stackP.h"
#include <assert.h>
#include <globals.h>

#ifdef TRACESTACK
tracingLevel traceStack = noTracing; // stack operation tracing
#endif

integer minStackSize = 256; /* Smallest stack size */
integer defaultStackSize = 1024 * 1024; // Initial default stack size when running
integer stackRegionSize = (1 << 26); /* 64M cells is default stack region */

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
  stk->attachment = attachment;
  stk->bottom = (state == active ? Null : stk);
  stk->state = state;
  stk->hash = stackCount++;

#ifdef TRACESTACK
  if (traceStack > noTracing)
    outMsg(logFile, "new stack of %d words\n%_", sze);
#endif

  stk->prog = labelCode(underFlow);
  stk->pc = entryPoint(stk->prog);
  stk->args = stk->sp;
  gcReleaseRoot(H, root);

  return stk;
}

StackState stackState(stackPo tsk) { return tsk->state; }

static retCode markActive(stackPo tsk) {
  switch (tsk->state) {
    case suspended:
      tsk->state = active;
      return Ok;
    case active:
    case moribund:
      return Error;
  }
}

framePo currFrame(stackPo stk) { return stk->fp; }

framePo previousFrame(stackPo stk, framePo fp) {
  assert(validFP(stk, fp));
  return fp - 1;
}

framePo dropFrame(stackPo stk) {
  framePo fp = stk->fp;
  assert(validFP(stk, fp));

  stk->sp = stk->args + argCount(stk->prog);

  stk->pc = fp->link;
  stk->prog = fp->prog;
  stk->args = fp->args;
  stk->fp--;
  return stk->fp;
}

integer stackHwm(stackPo tsk) { return tsk->hwm; }

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
  framePo f = stk->fp + 1;

  f->prog = stk->prog;
  f->args = stk->args;
  f->link = stk->pc;

  stk->pc = entryPoint(mtd);
  stk->prog = mtd;
  stk->args = stk->sp;

  stk->fp = f;

  return f;
}

long stkSize(specialClassPo cl, termPo o) { return StackCellCount; }

void stackSanityCheck(stackPo stk) {
  switch (stk->state) {
    case active:
      assert(stk->bottom == Null);
      break;
    case suspended:
      assert(
        stk->bottom != Null &&
        (stk->attachment == Null ? isAttachedStack(stk->bottom, stk) : True));
      break;
    case moribund:
      assert(stk->stkMem == Null);
      return;
  }
  assert(stk->fp >= baseFrame(stk) && ((ptrPo)(stk->fp + 1)) <= stk->sp);
  assert((ptrPo)(stk->fp + 1) < stk->sp);
  assert(!inFreeBlock(stackRegion, stk->stkMem));
}

void verifyStack(stackPo stk, heapPo H) {
  while (stk != Null) {
    assert(classOf((termPo)stk) == stackClass);

    if (stk->stkMem != Null) {
      stackSanityCheck(stk);
      ptrPo sp = stk->sp;
      ptrPo spLimit = stackLimit(stk);
      framePo fp = stk->fp;
      framePo fpLimit = baseFrame(stk);
      methodPo prog = stk->prog;
      ptrPo args = stk->args;

      do {
        check(isMethod((termPo)prog), "expecting a code pointer");
        check(args >= sp, "frame arg pointer invalid");
        sp = args + argCount(prog);
        prog = fp->prog;
        args = fp->args;
        fp--;
      } while (fp > fpLimit);

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
  *ds = *ss; // Copy the structural part

  return ((termPo) ds) + StackCellCount;
}

logical stkCmp(specialClassPo cl, termPo o1, termPo o2) { return o1 == o2; }

integer stkHash(specialClassPo cl, termPo o) {
  return hash61(C_STACK(o)->hash);
}

integer stackNo(stackPo tsk) { return tsk->hash; }

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
  pushFrame(totsk, fromtsk->prog);
  totsk->pc = fromtsk->pc;
}

termPo stkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  stackPo stk = C_STACK(o);

  assert(stk != Null);

  if (stk->stkMem != Null) {
    helper((ptrPo) &stk->prog, c);
    assert(isMethod((termPo)stk->prog));

    ptrPo spLimit = stackLimit(stk);
    framePo fp = stk->fp;
    framePo fpLimit = baseFrame(stk);

    while (fp > fpLimit) {
      helper((ptrPo) &fp->prog, c);
      assert(isMethod((termPo)fp->prog));
      fp--;
    }

    // Walk the value stack...
    for (ptrPo sp = stk->sp; sp < spLimit; sp++)
      helper(sp, c);

    assert(fp == fpLimit);

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

retCode stkDisp(ioPo out, termPo t, integer precision, integer depth,
                logical alt) {
  stackPo stk = C_STACK(t);

  return outMsg(out, "(.stack %d:[%s] %T.)", stk->hash,
                stackStateName(stk->state),
                stackState(stk) == moribund ? voidEnum : (termPo) stk->prog);
}

void showStackCall(ioPo out, integer depth, ptrPo args, integer frameNo,
                   StackTraceLevel level, methodPo prog, insPo pc) {
  assert(isMethod((termPo)prog));

  termPo loc = findLocation(prog, codeOffset(prog, pc));

  if (loc != Null)
    outMsg(out, "[%d] %L: %T", frameNo, loc, prog);
  else
    outMsg(out, "[%d] (unknown loc): %T[%d]", frameNo, prog,
           codeOffset(prog, pc));

  int32 count = argCount(prog);

  switch (level) {
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
          outMsg(out, "%s%,*T", sep, depth - 1, *stackArg(args, ix));
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
      for (int32 ix = 0; ix < count; ix++) {
        outMsg(out, "%s%,*T", sep, depth - 1, *stackArg(args, ix));
        sep = ", ";
      }
      outMsg(out, ")\n");
      count = lclCount(prog);

      for (int32 vx = 1; vx <= count; vx++) {
        ptrPo var = stackLcl(args, vx);
        if (*var != Null && *var != voidEnum)
          outMsg(out, "  L[%d] = %,*T\n", vx, depth - 1, *var);
      }
    }
  }
}

void stackTrace(processPo p, ioPo out, stackPo stk, integer depth,
                StackTraceLevel level, integer maxDepth) {
  outMsg(out, "Stack trace for process %d\n", p->processNo);

  integer frameNo = 0;
  framePo fp = stk->fp;
  methodPo prog = stk->prog;
  ptrPo args = stk->args;
  insPo pc = stk->pc;

  do {
    while (fp > baseFrame(stk) && maxDepth-- > 0) {
      showStackCall(out, depth, args, frameNo++, level, prog, pc);
      prog = fp->prog;
      args = fp->args;
      pc = fp->link;

      fp--;
    }

    stk = stk->attachment;
  } while (stk != Null && maxDepth > 0);

  if (maxDepth <= 0)
    outMsg(out, "...\n");
}

stackPo glueOnStack(heapPo H, stackPo stk, integer size, integer saveArity) {
#ifdef TRACESTACK
  if (traceStack > noTracing) {
    outMsg(logFile, "glue on extension to %T\n", stk);
    verifyStack(stk, globalHeap);
  }
#endif
  int root = gcAddRoot(H, (ptrPo) &stk);

  assert(size >= minStackSize && stackState(stk) != moribund);

  stackPo newStack =
      allocateStack(H, size, underflowProg, stackState(stk), stk);
  moveStack2Stack(newStack, stk, saveArity);
  dropFrame(stk);
  propagateHwm(newStack);
  gcReleaseRoot(H, root);
  return newStack;
}

stackPo handleStackOverflow(stackPo stk, integer delta, int32 arity) {
  return glueOnStack(globalHeap, stk, (stk->sze * 3) / 2 + delta, arity);
}

stackPo spinupStack(heapPo H, integer size) {
  assert(size >= minStackSize);

  return allocateStack(H, size, underflowProg, suspended, Null);
}

stackPo newStack(heapPo H, termPo lam) {
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, labelCode(taskProg));

  pushStack(child, lam);
  pushStack(child, (termPo) child);

  return child; // We return the new stack
}

stackPo attachStack(stackPo tsk, stackPo top) {
  stackPo bottom = top->bottom;
  assert(bottom != Null && isAttachedStack(bottom, top));

#ifdef TRACESTACK
  if (traceStack > noTracing)
    outMsg(logFile, "attach stack %T to %T\n", top, tsk);
#endif

  assert(stackState(tsk) == active && stackState(top) == suspended &&
    stackState(bottom) == suspended);

  stackPo f = bottom;

  while (f != top) {
    markActive(f);
    f->bottom = Null;
    f = f->attachment;
  }
  markActive(f);

  assert(f == top && top->attachment == Null);
  top->attachment = tsk;
  top->bottom = Null;
  return bottom;
}

// Get the stack immediately below the identified parent
stackPo detachStack(stackPo base, stackPo top) {
#ifdef TRACESTACK
  if (traceStack > noTracing) {
    outMsg(logFile, "detach %T\n", base);
    verifyStack(base, globalHeap);
  }
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
    assert(hasClass((termPo)parent, stackClass));
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
