//
// Created by Francis McCabe on 2/19/21.
//

#include <assert.h>
#include <globals.h>
#include "taskP.h"
#include "termP.h"
#include "engineP.h"
#include "heapP.h"
#include "buddy.h"

logical traceTasks = False;          // stack operation tracing
integer minStackSize = 256;           /* What is the smallest stack size */
integer stackRegionSize = (1 << 23);     /* 64M cells is default max stack size */

static long tskSize(specialClassPo cl, termPo o);
static termPo tskCopy(specialClassPo cl, termPo dst, termPo src);
static termPo tskScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical tskCmp(specialClassPo cl, termPo o1, termPo o2);
static integer tskHash(specialClassPo cl, termPo o);
static retCode tskDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo tskFinalizer(specialClassPo class, termPo o);

SpecialClass TaskClass = {
  .clss = Null,
  .sizeFun = tskSize,
  .copyFun = tskCopy,
  .scanFun = tskScan,
  .finalizer = tskFinalizer,
  .compFun = tskCmp,
  .hashFun = tskHash,
  .dispFun = tskDisp
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

clssPo taskClass = (clssPo) &TaskClass;

static integer stackCount = 0;
#ifdef TRACESTACK
static integer stackReleases = 0;
#endif
static buddyRegionPo stackRegion;

static framePo firstFrame(taskPo tsk);
static logical isAttachedStar(taskPo base, taskPo tgt);

void initTasks() {
  TaskClass.clss = specialClass;
  underFlowMethod.clss = methodClass;
  integer regionSize = (1 << lg2(stackRegionSize));

#ifdef TRACESTACK
  if (traceTasks)
    outMsg(logFile, "setting stack region to %d words\n", regionSize);
#endif

  stackRegion = createRegion(regionSize, minStackSize);
}

taskPo C_TASK(termPo t) {
  assert(hasClass(t, taskClass));
  return (taskPo) t;
}

taskPo allocateTask(heapPo H, integer sze, methodPo underFlow, TaskState state, taskPo attachment) {
  if (sze > stackRegionSize)
    syserr("tried to allocate too large a stack");

  sze = (1 << lg2(2 * sze - 1)) - 1; // Adjust stack size to be just under a power of two

  int root = gcAddRoot(H, (ptrPo) &attachment);

  taskPo tsk = (taskPo) allocateObject(H, taskClass, StackCellCount);
  tsk->stkMem = (ptrPo) allocateBuddy(stackRegion, sze);

  if (tsk->stkMem == Null) {
    syserr("Ran out of stack space");
  }

  tsk->sze = sze;
  tsk->hwm = sze;
  tsk->sp = &tsk->stkMem[sze];
  tsk->fp = (framePo) &tsk->stkMem[sze];
  tsk->attachment = attachment;
  tsk->bottom = (state == active ? Null : tsk);
  tsk->state = state;
  tsk->hash = stackCount++;

#ifdef TRACESTACK
  if (traceTasks)
    outMsg(logFile, "establish stack of %d words\n", sze);
#endif

  pushFrame(tsk, underFlow, tsk->fp, tsk->sp);
  gcReleaseRoot(H, root);

  return tsk;
}

TaskState taskState(taskPo tsk) {
  return tsk->state;
}

retCode setTaskState(taskPo tsk, TaskState state) {
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

framePo currFrame(taskPo tsk) {
  return tsk->fp;
}

integer stackHwm(taskPo tsk) {
  return tsk->hwm;
}

void propagateHwm(taskPo tsk) {
  integer hwm = tsk->hwm;

  while (tsk->attachment != Null) {
    tsk = tsk->attachment;
    if (tsk->hwm < hwm)
      tsk->hwm = hwm;
  }
}

framePo pushFrame(taskPo tsk, methodPo mtd, framePo fp, ptrPo sp) {
  framePo f = (framePo) (sp) - 1;
  assert(f >= (framePo) (tsk->stkMem));
  f->prog = mtd;
  f->pc = entryPoint(mtd);
  f->fp = fp;
  tsk->fp = f;
  tsk->sp = (ptrPo) f;

  integer lclCnt = lclCount(mtd);  /* How many locals do we have */
  sp = tsk->sp = (ptrPo) tsk->fp - lclCnt;
#ifdef TRACEEXEC
  for (integer ix = 0; ix < lclCnt; ix++)
    sp[ix] = voidEnum;
#endif

  return f;
}

long tskSize(specialClassPo cl, termPo o) {
  return StackCellCount;
}

void taskSanityCheck(taskPo tsk) {
  assert(
    tsk != Null && tsk->sp >= tsk->stkMem && tsk->sp <= &tsk->stkMem[tsk->sze] && tsk->fp >= (framePo) tsk->stkMem &&
    tsk->fp <= (framePo) &tsk->stkMem[tsk->sze]);
  if (tsk->stkMem != Null)
    assert(!inFreeBlock(stackRegion, tsk->stkMem));
}

void verifyTask(taskPo tsk, heapPo H) {
  if (tsk->stkMem != Null) {
    taskSanityCheck(tsk);
    ptrPo sp = tsk->sp;
    framePo fp = tsk->fp;
    ptrPo limit = tsk->stkMem + tsk->sze;

    while (sp < limit) {
      assert(sp <= (ptrPo) fp);
      while (sp < (ptrPo) fp)
        validPtr(H, *sp++);
      sp = (ptrPo) (fp + 1);
      fp = fp->fp;
    }

    if (tsk->attachment != Null)
      verifyTask(tsk->attachment, H);
  }
}

termPo tskCopy(specialClassPo cl, termPo dst, termPo src) {
  taskPo ss = C_TASK(src);
  taskPo ds = (taskPo) dst; // Dest not yet a valid stack structure
  *ds = *ss;                  // Copy the structural part

  return ((termPo) ds) + StackCellCount;
}

logical tskCmp(specialClassPo cl, termPo o1, termPo o2) {
  return o1 == o2;
}

integer tskHash(specialClassPo cl, termPo o) {
  return C_TASK(o)->hash;
}

integer stackNo(taskPo tsk) {
  return tsk->hash;
}

termPo popStack(taskPo tsk) {
  assert(tsk->sp < stackLimit(tsk));
  return *tsk->sp++;
}

termPo topStack(taskPo tsk) {
  assert(tsk->sp < (ptrPo) tsk->fp);
  return tsk->sp[0];
}

termPo peekStack(taskPo tsk, integer delta) {
  assert(tsk->sp + delta < (ptrPo) tsk->fp);
  return tsk->sp[delta];
}

void pushStack(taskPo tsk, termPo ptr) {
  assert(tsk->sp > tsk->stkMem);
  *--tsk->sp = ptr;
}

void moveStack2Stack(taskPo totsk, taskPo fromtsk, integer count) {
  assert(validStkPtr(fromtsk, fromtsk->sp + count));
  assert(stkHasSpace(totsk, count));

  ptrPo src = fromtsk->sp + count;
  ptrPo dst = totsk->sp;
  for (integer ix = count; ix > 0; ix--) {
    *--dst = *--src;
  }
  totsk->sp = dst;
  fromtsk->sp += count;
}

termPo tskScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  taskPo tsk = C_TASK(o);

  assert(tsk != Null);

  if (tsk->stkMem != Null) {
#ifdef TRACEMEM
    if (traceTasks)
      taskSanityCheck(tsk);

    if (traceMemory) {
      outMsg(logFile, "scan stack %d\n%_", tsk->hash);
    }
#endif

    ptrPo sp = tsk->sp;
    framePo fp = tsk->fp;
    ptrPo limit = tsk->stkMem + tsk->sze;

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

  if (tsk->attachment != Null)
    helper((ptrPo) &tsk->attachment, c);
  if (tsk->bottom != Null)
    helper((ptrPo) &tsk->bottom, c);

  return o + StackCellCount;
}

termPo tskFinalizer(specialClassPo class, termPo o) {
  taskPo tsk = C_TASK(o);
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

retCode tskDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  taskPo tsk = C_TASK(t);

  return outMsg(out, "(.stack %d:[%s] %M.)",
                tsk->hash,
                stackStateName(tsk->state),
                tsk->fp->prog);
}

taskPo glueOnStack(heapPo H, taskPo tsk, integer size, integer saveArity) {
  int root = gcAddRoot(H, (ptrPo) &tsk);

  assert(size >= minStackSize && taskState(tsk) != moribund);

  taskPo newStack = allocateTask(H, size, &underFlowMethod, taskState(tsk), tsk);
  moveStack2Stack(newStack, tsk, saveArity);
  propagateHwm(newStack);
  gcReleaseRoot(H, root);
  return newStack;
}

taskPo spinupStack(heapPo H, integer size) {
  assert(size >= minStackSize);

  return allocateTask(H, size, &underFlowMethod, suspended, Null);
}

taskPo attachTask(taskPo tsk, taskPo top) {
  taskPo bottom = top->bottom;
  assert(bottom != Null && isAttachedStar(bottom, top));

#ifdef TRACESTACK
  if (traceTasks)
    outMsg(logFile, "attach stack %T to %T\n", top, tsk);
#endif

  assert(taskState(tsk) == active && taskState(top) == suspended && taskState(bottom) == suspended);

  taskPo f = bottom;

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
taskPo detachTask(taskPo base, taskPo top) {
#ifdef TRACESTACK
  if (traceTasks)
    outMsg(logFile, "detach %T up to %T\n", base, top);
#endif
  assert(taskState(top) == active && top->bottom == Null);
  top->bottom = base;
  taskPo s = base;
  while (s != Null && s != top) {
    assert(s->state == active);
    s->state = suspended;
    s->bottom = base;
    s = s->attachment;
  }

  assert(s == top);
  taskPo parent = top->attachment;
  s->attachment = Null;
  s->state = suspended;
  s->bottom = base;
  assert(isAttachedStar(base, top));
  return parent;
}

taskPo dropTask(taskPo tsk) {
#ifdef TRACESTACK
  if (traceTasks)
    outMsg(logFile, "drop stack %T\n%_", tsk);
  stackReleases++;
#endif
  taskPo previous = tsk->attachment;
  tsk->state = moribund;
  release(stackRegion, (voidPtr) tsk->stkMem);
  tsk->stkMem = Null;
  tsk->sze = -1;
  return previous;
}

framePo firstFrame(taskPo tsk) {
  framePo fp = tsk->fp;
  framePo f = fp;
  framePo limit = (framePo) (tsk->stkMem + tsk->sze);

  while (fp < limit) {
    f = fp;
    fp = fp->fp;
  }

  if (f < limit)
    return f;
  else
    return Null;
}

logical isAttachedStar(taskPo base, taskPo tgt) {
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
