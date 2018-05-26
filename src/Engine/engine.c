/*
 * top-level run-time functions
 */
#include "cafe.h"
#include <stdlib.h>
#include <assert.h>
#include <str.h>
#include <lblops.h>

#include "engineP.h"
#include "decode.h"
#include "globals.h"

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

retCode bootstrap(char *entry) {
  labelPo umain = declareLbl(entry, 0);
  methodPo mainMtd = labelCode(umain);

  if (mainMtd != Null) {
    processPo p = newProcess(mainMtd);
    return run(p);
  } else {
    logMsg(logFile, "cannot find entry point %s\n", entry);
    return Error;
  }
}

processPo newProcess(methodPo mtd) {
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

  uniNCpy(P->wd, NumberOf(P->wd), CWD, NumberOf(CWD));

  P->fp = (framePo) P->stackLimit;

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

void ps_kill(processPo p) {
  if (p != NULL) {
    pthread_t thread = p->threadID;

    pthread_cancel(thread);    /* cancel the thread */

    hashRemove(prTble, (void *) p->processNo);
    freePool(prPool, p);
  }
}

void stackTrace(processPo p) {

}

retCode extendStack(processPo p, int sfactor, int hfactor, long hmin) {
  syserr("stack grow not implemented");
  return Ok;
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

  ReturnStatus rt = {.ret=Error, .rslt=(termPo) err};
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
  return uniNCpy(p->wd, NumberOf(p->wd), wd, len);
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
  processPo p1 = (processPo) a;
  processPo p2 = (processPo) b;
  if (a == b)
    return same;
  else
    return incomparible;
}
