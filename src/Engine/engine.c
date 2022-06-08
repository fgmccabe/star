/*
 * top-level run-time functions
 */
#include "star.h"
#include <stdlib.h>
#include <strings.h>
#include <lblops.h>

#include "engineP.h"
#include "decode.h"
#include "globals.h"
#include "debugP.h"

// Count instructions etc.
static poolPo prPool;     /* pool of processes */

hashPo prTble;

static integer processHash(void *);
static comparison sameProcess(void *, void *);

static integer newProcessNumber();

static LockRecord processLock;
__thread processPo currentProcess = Null;

MethodRec haltMethod = {
  .clss = Null,
  .codeSize = 2,
  .arity = 0,
  .lclcnt = 0,
  .pool = Null,
  .locals = Null,
  .code = {Halt, 0}
};

void initEngine() {
  prPool = newPool(sizeof(ProcessRec), 32);
  prTble = newHash(16, processHash, sameProcess, Null);
  initLock(&processLock);
  haltMethod.clss = methodClass;
}

retCode bootstrap(heapPo h, char *entry, char *rootWd, capabilityPo rootCap) {
  labelPo umain = declareLbl(entry, 1, -1);
  methodPo mainMtd = labelCode(umain);

  if (mainMtd != Null) {
    termPo cmdLine = commandLine(h);
    processPo p = newProcess(h, mainMtd, rootWd, rootCap, cmdLine);
    integer ret = run(p);

    ps_kill(p);
    return ret;
  } else {
    logMsg(logFile, "cannot find entry point %s\n", entry);
    return Error;
  }
}

processPo newProcess(heapPo h, methodPo mtd, char *rootWd, capabilityPo processCap, termPo rootArg) {
  processPo P = (processPo) allocPool(prPool);
  taskPo stk = P->stk = allocateTask(h, minStackSize, &haltMethod, active, Null);

  pushStack(stk, rootArg);
  pushFrame(stk, mtd, stk->fp, stk->sp);

  P->heap = h;
  P->state = P->savedState = quiescent;
  P->pauseRequest = False;
  P->processCap = processCap;
  if (insDebugging || lineDebugging) {
    if (interactive)
      P->waitFor = stepInto;
    else
      P->waitFor = nextBreak;
  } else
    P->waitFor = never;

  P->tracing = tracing;
  P->waterMark = stk->fp;

  setProcessWd(P, rootWd, uniStrLen(rootWd));

  P->processNo = newProcessNumber();

  hashPut(prTble, (void *) P->processNo, P);
  return P;
}

void ps_kill(processPo p) {
  if (p != NULL) {
    p->stk = dropTask(p->stk);

    pthread_t thread = p->threadID;

    pthread_cancel(thread);    /* cancel the thread */

    hashRemove(prTble, (void *) p->processNo);
    freePool(prPool, p);
  }
}

#ifdef TRACEMEM
long stkGrow = 0;
#endif

ReturnStatus liberror(heapPo h, char *name, termPo code) {
  int root = gcAddRoot(h, &code);
  termPo msg = allocateCString(h, name);
  gcAddRoot(h, (ptrPo) &msg);

  normalPo err = allocateStruct(h, errorLbl);
  setArg(err, 0, (termPo) msg);
  setArg(err, 1, code);

  gcReleaseRoot(h, root);

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

capabilityPo processCap(processPo p) {
  return p->processCap;
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
  return processHashTable(prEntry, prTble, &h);
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
