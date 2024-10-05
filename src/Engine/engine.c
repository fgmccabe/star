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
#include "tpl.h"

// Count instructions etc.
static poolPo prPool;     /* pool of processes */

timerPo runTimer = Null;

hashPo prTble;

static integer processHash(void *);
static comparison sameProcess(void *, void *);

static integer newProcessNumber();

__thread processPo currentProcess = Null;

static InstructionBlock haltBlock = {
  .insCount = 2,
  .ins = {Halt, 0}
};


methodPo haltMethod;

void initEngine() {
  prPool = newPool(sizeof(ProcessRec), 32);
  prTble = newHash(16, processHash, sameProcess, Null);

  haltMethod = declareMethod("halt", 0, &haltBlock, NULL, 0);

  runTimer = newTimer("running");
}

retCode bootstrap(heapPo h, char *entry, char *rootWd) {
  labelPo umain = declareLbl(entry, 1, -1);
  methodPo mainMtd = labelCode(umain);

  if (mainMtd != Null) {
    termPo cmdLine = commandLine(h);
    processPo p = newProcess(h, mainMtd, rootWd, cmdLine);
    resumeTimer(runTimer);
    integer ret = run(p);
    pauseTimer(runTimer);

    ps_kill(p);
    return ret;
  } else {
    logMsg(logFile, "cannot find entry point %s\n", entry);
    return Error;
  }
}

processPo newProcess(heapPo h, methodPo mtd, char *rootWd, termPo rootArg) {
  processPo P = (processPo) allocPool(prPool);
  integer stackSize = maximum(stackDelta(mtd) * 2, defaultStackSize);
  stackPo stk = P->stk = allocateStack(h, stackSize, haltMethod, active, Null);

  pushStack(stk, rootArg);
  stk->fp = pushFrame(stk, mtd);
  P->tryCounter = 0;

  P->heap = h;
  P->state = quiescent;
  if (insDebugging || lineDebugging) {
    if (interactive)
      P->waitFor = stepInto;
    else
      P->waitFor = nextBreak;
  } else
    P->waitFor = never;

  P->tracing = tracing;
  P->waterMark = Null;

  setProcessWd(P, rootWd, uniStrLen(rootWd));

  P->processNo = newProcessNumber();

  hashPut(prTble, (void *) P->processNo, P);
  return P;
}

void ps_kill(processPo p) {
  if (p != NULL) {
    p->stk = dropStack(p->stk);

    pthread_t thread = p->threadID;

    if (thread) {
      pthread_cancel(thread);    /* cancel the thread */
    }

    hashRemove(prTble, (void *) p->processNo);
    freePool(prPool, p);
  }
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

integer nextTryCounter(processPo P) {
  return P->tryCounter++;
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
  integer nextPrNo = prCount++;
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

retCode markProcess(processPo P, gcSupportPo G) {
#ifdef TRACEMEM
  if (traceMemory)
    outMsg(logFile, "Mark process %d\n%_", P->processNo);
#endif
  P->stk = C_STACK(markPtr(G, (ptrPo) &P->stk));

  return Ok;
}

void markProcesses(processPo owner, gcSupportPo G) {
  if (owner != Null)
    markProcess(owner, G);
  else
    processProcesses((procProc) markProcess, G);
}

void verifyProc(processPo P, heapPo H) {
  verifyStack(P->stk, H);
}

void verifyProcesses(heapPo H) {
  if (H->owner != Null)
    verifyProc(H->owner, H);
  else
    processProcesses((procProc) verifyProc, H);
}
