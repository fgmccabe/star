/*
 * top-level run-time functions
 */
#include "star.h"
#include <stdlib.h>
#include <assert.h>
#include <str.h>
#include <lblops.h>
#include <args.h>
#include <strings.h>

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

retCode bootstrap(char *entry, char *rootWd, capabilityPo rootCap) {
  labelPo umain = declareLbl(entry, 1, -1);
  methodPo mainMtd = labelCode(umain);

  if (mainMtd != Null) {
    termPo cmdLine = commandLine(currHeap);
    processPo p = newProcess(mainMtd, rootWd, rootCap, cmdLine);
    retCode ret = run(p);
    ps_kill(p);
    return ret;
  } else {
    logMsg(logFile, "cannot find entry point %s\n", entry);
    return Error;
  }
}

processPo newProcess(methodPo mtd, char *rootWd, capabilityPo processCap, termPo rootArg) {
  processPo P = (processPo) allocPool(prPool);
  stackPo stk = P->stk = allocateStack(currHeap, initStackSize, &haltMethod, voidEnum);
  setStackState(P->stk, root);

  pushStack(stk, rootArg);
  pushFrame(stk, mtd, stk->sp);

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

  setProcessWd(P, rootWd, uniStrLen(rootWd));

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

static inline ptrPo realign(ptrPo ptr, ptrPo oldStack, ptrPo oldLimit, ptrPo newLimit) {
  assert(ptr >= oldStack && ptr <= oldLimit);

  return newLimit - (oldLimit - ptr);
}

static inline ptrPo localVar(framePo fp, int64 off) {
  return &(((ptrPo) fp)[-off]);
}

#ifdef TRACEMEM
long stkGrow = 0;
#endif

ReturnStatus liberror(processPo P, char *name, termPo code) {
  heapPo H = processHeap(P);
  int root = gcAddRoot(H, &code);
  termPo msg = allocateCString(H, name);
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
