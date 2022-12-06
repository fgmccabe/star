//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <globals.h>
#include "stackP.h"
#include "termP.h"
#include "engineP.h"
#include "heapP.h"

logical traceStack = False;          // stack operation tracing
integer minStackSize = 256;           /* What is the smallest stack size */
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

static MethodRec newTaskMethod = {
  .clss = Null,
  .codeSize = 9,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Rot, 0, 2, Rot, 0, 1, TOCall, 0, 3}
};

static MethodRec spawnMethod = {
  .clss = Null,
  .codeSize = 3,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = { TOCall, 0, 2}
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

stackPo allocateStack(heapPo H, integer sze, methodPo underFlow, TaskState state, stackPo attachment) {
  if (sze > stackRegionSize)
    syserr("tried to allocate too large a stack");

  sze = (1 << lg2(2 * sze - 1)) - 1; // Adjust stack size to be just under a power of two

  int root = gcAddRoot(H, (ptrPo) &attachment);

  stackPo tsk = (stackPo) allocateObject(H, stackClass, StackCellCount);
  tsk->stkMem = (ptrPo) allocateBuddy(stackRegion, sze);

  if (tsk->stkMem == Null) {
    syserr("Ran out of stack space");
  }

  tsk->sze = sze;
  tsk->hwm = sze;
  tsk->sp = &tsk->stkMem[sze];
  tsk->fp = ((framePo) tsk->stkMem) - 1;
  tsk->attachment = attachment;
  tsk->bottom = (state == active ? Null : tsk);
  tsk->state = state;
  tsk->hash = stackCount++;
  tsk->counter = 0;

#ifdef TRACESTACK
  if (traceStack)
    outMsg(logFile, "establish stack of %d words\n", sze);
#endif

  tsk->fp = pushFrame(tsk, underFlow, tsk->fp);
  gcReleaseRoot(H, root);

  return tsk;
}

TaskState stackState(stackPo tsk) {
  return tsk->state;
}

retCode setTaskState(stackPo tsk, TaskState state) {
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

framePo currFrame(stackPo tsk) {
  return tsk->fp;
}

framePo previousFrame(stackPo stk, framePo fp) {
  assert(fp >= baseFrame(stk) && ((ptrPo) fp + 1) < stk->sp);
  return fp - 1;
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
  /* How many locals do we have */

  framePo f = fp + 1;
  assert(f >= (framePo) (stk->stkMem) && ((ptrPo) (f + 1) < stk->sp - lclCount(mtd)));
  f->prog = mtd;
  f->pc = entryPoint(mtd);
  f->csp = stk->sp;

  return f;
}

integer frameNo(stackPo stk, framePo fp) {
  assert(fp >= (framePo) (stk->stkMem) && (ptrPo) fp < stk->sp);
  return fp - (framePo) (stk->stkMem);
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
  assert(
    stk->sp <= stackLimit(stk) && stk->sp >= ((ptrPo) (stk->fp + 1)) &&
    stk->fp >= (framePo) (stk->stkMem) - 1);
  assert(!inFreeBlock(stackRegion, stk->stkMem));
}

void verifyStack(stackPo stk, heapPo H) {
  while (stk != Null) {
    assert(classOf((termPo)stk)==stackClass);

    if (stk->stkMem != Null) {
      stackSanityCheck(stk);
      ptrPo sp = stk->sp;
      framePo fp = stk->fp;
      ptrPo limit = stackLimit(stk);

      while (fp >= (framePo) stk->stkMem) {
        ptrPo csp = fp->csp;
        assert(sp <= csp);
        while (sp < csp) {
          validPtr(H, *sp++);
        }
        fp--;
      }

      while (sp < limit)
        validPtr(H, *sp++);

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
  return C_STACK(o)->hash;
}

integer stackNo(stackPo tsk) {
  return tsk->hash;
}

termPo popStack(stackPo stk) {
  assert(validStkValueLoc(stk, stk->sp));
  return *stk->sp++;
}

termPo topStack(stackPo stk) {
  assert(validStkValueLoc(stk, stk->sp));
  return stk->sp[0];
}

termPo peekStack(stackPo stk, integer delta) {
  assert(validStkValueLoc(stk, stk->sp + delta));
  return stk->sp[delta];
}

void pushStack(stackPo stk, termPo ptr) {
  *--stk->sp = ptr;
}

void moveStack2Stack(stackPo totsk, stackPo fromtsk, integer count) {
  assert(validStkValueLoc(fromtsk, fromtsk->sp + count));
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
    framePo fp = stk->fp;
    ptrPo limit = stk->stkMem + stk->sze;
    framePo baseFp = (framePo) (stk->stkMem);

    while (fp >= baseFp) {
      integer off = insOffset(fp->prog, fp->pc);
      helper((ptrPo) &fp->prog, c);
      fp->pc = pcAddr(fp->prog, off);
      fp--;
    }

    while (sp < limit) {
      helper(sp++, c);
    }
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
    release(stackRegion, (voidPtr) tsk->stkMem);
    tsk->stkMem = Null;
  }
  return o + StackCellCount;
}

char *stackStateName(TaskState ste) {
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
  stackPo tsk = C_STACK(t);

  return outMsg(out, "(.stack %d:[%s] %M.)",
                tsk->hash,
                stackStateName(tsk->state),
                tsk->fp->prog);
}

void
showStackCall(ioPo out, integer displayDepth, framePo fp, stackPo stk, ptrPo sp, integer frameNo,
              StackTraceLevel tracing) {
  methodPo mtd = fp->prog;
  if (normalCode(mtd)) {
    insPo pc = fp->pc;
    integer pcOffset = (integer) (pc - mtd->code);

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
        if (displayDepth > 0) {
          outMsg(out, "(");
          char *sep = "";
          for (integer ix = 0; ix < count; ix++) {
            outMsg(out, "%s%,*T", sep, displayDepth, *stackArg(stk, fp, ix));
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
          outMsg(out, "%s%,*T", sep, displayDepth, *stackArg(stk, fp, ix));
          sep = ", ";
        }
        outMsg(out, ")\n");
        if (sp < fp->csp) {
          count = lclCount(mtd);

          for (integer vx = 1; vx <= count; vx++) {
            ptrPo var = stackLcl(stk, fp, vx);
            if (*var != Null && *var != voidEnum)
              outMsg(out, "  L[%d] = %,*T\n", vx, displayDepth, *var);
          }
        }
      }
    }
  }
}

void stackTrace(processPo p, ioPo out, stackPo stk, integer displayDepth, StackTraceLevel tracing) {
  outMsg(out, "Stack trace for process %d\n", p->processNo);

  do {
    switch (stackState(stk)) {
      case suspended:
        outMsg(out, RED_ESC_ON"Suspended"RED_ESC_OFF);
        break;
      case active:
        outMsg(out, GREEN_ESC_ON"Active"GREEN_ESC_OFF);
        break;
      case moribund:
        outMsg(out, RED_ESC_ON"Moribund"RED_ESC_OFF);
        break;
    }

    outMsg(out, "\n");

    framePo fp = stk->fp;
    ptrPo sp = stk->sp;

    while (fp >= baseFrame(stk)) {
      showStackCall(out, displayDepth, fp, stk, sp, frameNo(stk, fp), tracing);
      sp = fp->csp + argCount(fp->prog);
      fp--;
    }

    stk = stk->attachment;
  } while (stk != Null);

  flushFile(out);
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

stackPo newStack(processPo P, termPo lam) {
  heapPo H = P->heap;
  int root = gcAddRoot(H, (ptrPo) &lam);
  stackPo child = spinupStack(H, minStackSize);
  gcReleaseRoot(H, root);

  child->fp = pushFrame(child, &newTaskMethod, child->fp);

  pushStack(child, lam);
  pushStack(child, (termPo) child);

  return child;                                                 // We return the new stack
}

stackPo spawnStack(processPo P, termPo lam) {
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
    assert(hasClass((termPo)parent, stackClass));
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
  release(stackRegion, (voidPtr) tsk->stkMem);
  tsk->stkMem = Null;
  tsk->sze = -1;
  return previous;
}

stackPo dropUntil(stackPo base, stackPo tgt) {
  while (base != Null && base != tgt)
    base = dropStack(base);
  return base;
}

logical isAttachedStack(stackPo base, stackPo tgt) {
  while (base != Null && base != tgt)
    base = base->attachment;
  return base == tgt;
}

void dumpStackStats() {
#ifdef TRACESTACK
  outMsg(logFile, "%d stacks allocated\n", stackCount);
  outMsg(logFile, "%d stacks dropped\n", stackReleases);
#endif
}
