/*
 * top-level run-time functions
 */
#include "star.h"
#include <stdlib.h>
#include <assert.h>
#include <str.h>
#include <lblops.h>
#include <args.h>

#include "engineP.h"
#include "decode.h"
#include "globals.h"
#include "debugP.h"

logical runStats = False;         // Count instructions etc.
static poolPo prPool;     /* pool of processes */

hashPo prTble;

static integer processHash(void *);
static comparison sameProcess(void *, void *);

static integer newProcessNumber();

static LockRecord processLock;

static MethodRec halt = {
  .clss = Null,
  .codeSize = 1,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Halt}
};

void initEngine() {
  prPool = newPool(sizeof(ProcessRec), 32);
  prTble = NewHash(16, processHash, sameProcess, Null);
  initLock(&processLock);
  halt.clss = methodClass;
}

retCode bootstrap(char *entry, char *rootWd) {
  labelPo umain = declareLbl(entry, 0);
  methodPo mainMtd = labelCode(umain);

  if (mainMtd != Null) {
    processPo p = newProcess(mainMtd, rootWd);
    retCode ret = run(p);
    ps_kill(p);
    return ret;
  } else {
    logMsg(logFile, "cannot find entry point %s\n", entry);
    return Error;
  }
}

processPo newProcess(methodPo mtd, char *rootWd) {
  processPo P = (processPo) allocPool(prPool);

  P->prog = mtd;
  P->pc = entryPoint(mtd);
  P->stackBase = (termPo) malloc(sizeof(integer) * initStackSize);
  P->stackLimit = &P->stackBase[initStackSize];
  P->heap = currHeap;
  P->state = P->savedState = quiescent;
  P->pauseRequest = False;
  if (insDebugging || lineDebugging) {
    if (interactive)
      P->waitFor = stepInto;
    else
      P->waitFor = nextBreak;
  } else
    P->waitFor = never;

  P->tracing = tracing;

  P->fp = (framePo) P->stackLimit;

  setProcessWd(P,rootWd,uniStrLen(rootWd));

  // cap the stack with a halting stop.

  ptrPo sp = (ptrPo) P->fp;
  *--sp = (termPo) &halt;
  *--sp = (termPo) entryPoint(&halt);
  *--sp = (termPo) P->fp;    /* set the new frame pointer */

  P->fp = (framePo) sp;
  P->sp = sp;

  P->processNo = newProcessNumber();

  hashPut(prTble, (void *) P->processNo, P);

  return P;
}

void setupProcess(processPo P, methodPo mtd) {
  P->prog = mtd;
  P->pc = entryPoint(mtd);
  P->heap = currHeap;
  P->state = P->savedState = quiescent;
  P->pauseRequest = False;
  if (insDebugging || lineDebugging) {
    if (interactive)
      P->waitFor = stepInto;
    else
      P->waitFor = nextBreak;
  } else
    P->waitFor = never;

  P->tracing = tracing;

  uniNCpy(P->wd, NumberOf(P->wd), CWD, NumberOf(CWD));

  P->fp = (framePo) P->stackLimit;

  // cap the stack with a halting stop.

  ptrPo sp = (ptrPo) P->fp;
  *--sp = (termPo) &halt;
  *--sp = (termPo) entryPoint(&halt);
  *--sp = (termPo) P->fp;    /* set the new frame pointer */

  P->fp = (framePo) sp;
  P->sp = sp;
}

void ps_kill(processPo p) {
  if (p != NULL) {
    pthread_t thread = p->threadID;

    pthread_cancel(thread);    /* cancel the thread */

    hashRemove(prTble, (void *) p->processNo);
    freePool(prPool, p);
  }
}

static inline ptrPo realign(ptrPo ptr, ptrPo oldStack, ptrPo oldLimit, ptrPo newLimit) {
  assert(ptr >= oldStack && ptr <= oldLimit);

  return newLimit - (oldLimit - ptr);
}

static ptrPo localVar(framePo fp, int64 off) {
  return &(((ptrPo) fp)[-off]);
}

retCode extendStack(processPo p, integer sfactor) {
  integer stackSize = (integer) ((p->stackLimit - p->stackBase) * sfactor);

#ifdef TRACEMEM
  if (traceMemory) {
    outMsg(logFile, "extending stack of process %d to %d cells\n%_", p->processNo, stackSize);

    verifyProc(p, processHeap(p));
  }
#endif
  if (stackSize > maxStackSize)
    return Error;

  termPo nStack = (termPo) malloc(stackSize * sizeof(termPo));

  if (nStack == NULL) {
    syserr("not enough memory to grow stack");
    return Error;
  } else {
    termPo nLimit = nStack + stackSize;
    framePo fp = p->fp;
    ptrPo sp = p->sp;
    methodPo mtd = p->prog;

    while (fp < (framePo) p->stackLimit) {
      ptrPo nsp = realign(sp, (ptrPo) p->stackBase, (ptrPo) p->stackLimit, (ptrPo) nLimit);
      framePo nfp = (framePo) realign((ptrPo) fp, (ptrPo) p->stackBase, (ptrPo) p->stackLimit, (ptrPo) nLimit);
      nfp->prog = fp->prog;
      nfp->rtn = fp->rtn;
      nfp->fp = (framePo) realign((ptrPo) (fp->fp), (ptrPo) p->stackBase, (ptrPo) p->stackLimit, (ptrPo) nLimit);

      integer count = argCount(mtd);

      for (integer ix = 0; ix < count; ix++) {
        nfp->args[ix] = fp->args[ix];
      }

      for (integer vx = 1; vx <= lclCount(mtd); vx++) {
        *localVar(nfp, vx) = *localVar(fp, vx);
      }

      ptrPo stackTop = ((ptrPo) fp) - mtd->lclcnt;
      for (integer ix = 0; sp < stackTop; ix++, sp++) {
        *realign(sp, (ptrPo) p->stackBase, (ptrPo) p->stackLimit, (ptrPo) nLimit) = *sp;
      }

      mtd = fp->prog;

      sp = (ptrPo) (fp + 1);
      fp = fp->fp;
    }
    p->fp = (framePo) realign((ptrPo) (p->fp), (ptrPo) p->stackBase, (ptrPo) p->stackLimit, (ptrPo) nLimit);
    p->sp = realign((ptrPo) (p->sp), (ptrPo) p->stackBase, (ptrPo) p->stackLimit, (ptrPo) nLimit);

    free(p->stackBase);
    p->stackBase = nStack;
    p->stackLimit = nLimit;

#ifdef TRACEMEM
    if (traceMemory) {
      verifyProc(p, processHeap(p));
    }
#endif

    return Ok;
  }
}

ReturnStatus liberror(processPo P, char *name, termPo code) {
  heapPo H = processHeap(P);
  int root = gcAddRoot(H, &code);
  stringPo msg = allocateCString(H, name);
  gcAddRoot(H, (ptrPo) &msg);

  normalPo err = allocateStruct(H, errorLbl);
  setArg(err, 0, (termPo) msg);
  setArg(err, 1, code);

  gcReleaseRoot(H, root);

  ReturnStatus rt = {.ret=Error, .result=(termPo) err};
  return rt;
}

heapPo processHeap(processPo P) {
  return P->heap;
}

void switchProcessState(processPo p, ProcessState state) {
  p->state = state;
}

void setProcessRunnable(processPo p) {
  switchProcessState(p, runnable);
}

ProcessState processState(processPo p) {
  return p->state;
}

integer processNo(processPo p) {
  return p->processNo;
}

typedef struct {
  procProc pr;
  void *cl;
} Helper;

static retCode prEntry(void *n, void *r, void *c) {
  Helper *h = (Helper *) c;

  return h->pr((processPo) r, h->cl);
}

retCode processProcesses(procProc p, void *cl) {
  Helper h = {.pr = p, .cl = cl};
  return ProcessTable(prEntry, prTble, &h);
}

processPo getProcessOfThread(void) {
  exit(-1);
}

char *processWd(processPo p) {
  return p->wd;
}

retCode setProcessWd(processPo p, char *wd, integer len) {
  return uniTrim(wd, len, "", "/", p->wd, NumberOf(p->wd));
}

static integer prCount = 0;

integer newProcessNumber() {
  acquireLock(&processLock, 0);
  integer nextPrNo = prCount++;
  releaseLock(&processLock);
  return nextPrNo;
}

integer processHash(void *x) {
  return (integer) (x);
}

comparison sameProcess(void *a, void *b) {
  if (a == b)
    return same;
  else
    return incomparible;
}
