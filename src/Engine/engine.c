/*
 * top-level run-time functions
 */
#include <constants.h>

#include "star.h"
#include <stdlib.h>
#include <strings.h>
#include <lblops.h>
#include "globals.h"
#include "engineP.h"
#include "debugP.h"

// Count instructions etc.
static poolPo prPool; /* pool of processes */
static labelPo haltProg;

timerPo runTimer = Null;

hashPo prTble;

static integer processHash(void *);

static comparison sameProcess(void *, void *);

static integer newProcessNumber();

static Instruction haltCode[] = {Halt};

void initEngine() {
  prPool = newPool(sizeof(EngineRecord), 32);
  prTble = newHash(16, processHash, sameProcess, Null);
  termPo zero = makeInteger(0);
  int32 zeroIndex = defineConstantLiteral(zero);
  haltCode[0].fst = zeroIndex;

  haltProg = specialMethod("halt", 0, NumberOf(haltCode), haltCode, 0, 1, 1);

  runTimer = newTimer("running");
}

int32 bootstrap(heapPo h, char *entry, char *rootWd) {
  labelPo umain = declareLbl(entry, 1, -1);
  methodPo mainMtd = labelMtd(umain);

  if (mainMtd != Null) {
    termPo cmdLine = commandLine(h);
#ifndef NOJIT
    enginePo p = newEngine(h, jitOnLoad, mainMtd, rootWd, cmdLine);

    resumeTimer(runTimer);
    int32 ret = (jitOnLoad ? exec(p) : run(p));
#else
    enginePo p = newEngine(h, False, mainMtd, rootWd, cmdLine);

    resumeTimer(runTimer);
    int32 ret = run(p);
#endif
    pauseTimer(runTimer);
    return ret;
  } else {
    logMsg(logFile, "cannot find entry point %s\n", entry);
    return Abnormal;
  }
}

enginePo newEngine(heapPo h, int execJit, methodPo mtd, char *rootWd, termPo rootArg) {
  enginePo P = (enginePo) allocPool(prPool);

  P->heap = h;
  P->state = quiescent;
  if (insDebugging || lineDebugging) {
    if (interactive)
      P->waitFor = stepInto;
    else
      P->waitFor = nextBreak;
  } else
    P->waitFor = never;

  P->tracing = False;
  P->waterMark = Null;

  setProcessWd(P, rootWd, uniStrLen(rootWd));

  P->processNo = newProcessNumber();

  integer stackSize = maximum(stackDelta(mtd) * 2, defaultStackSize);
  stackPo stk = P->stk = allocateStack(P, stackSize, haltProg, execJit, active, Null);

  pushStack(stk, rootArg);
  stk->fp = pushFrame(stk, execJit, mtd);

  hashPut(prTble, (void *) P->processNo, P);
  return P;
}

void ps_kill(enginePo p) {
  if (p != NULL) {
    p->stk = dropStack(p->stk);

    pthread_t thread = p->threadID;

    if (thread) {
      pthread_cancel(thread); /* cancel the thread */
    }

    hashRemove(prTble, (void *) p->processNo);
    freePool(prPool, p);
  }
}

heapPo processHeap(enginePo P) {
  return P->heap;
}

void pshVal(enginePo p, termPo v) {
  pushStack(p->stk, v);
}

termPo popVal(enginePo p) {
  return popStack(p->stk);
}

void switchProcessState(enginePo p, ProcessState state) {
  p->state = state;
}

void setProcessRunnable(enginePo p) {
  switchProcessState(p, runnable);
}

integer processNo(enginePo p) {
  return p->processNo;
}

typedef struct {
  procProc pr;
  void *cl;
} Helper;

static retCode prEntry(void *n, void *r, void *c) {
  Helper *h = (Helper *) c;

  return h->pr((enginePo) r, h->cl);
}

retCode processProcesses(procProc p, void *cl) {
  Helper h = {.pr = p, .cl = cl};
  return processHashTable(prEntry, prTble, &h);
}

enginePo getProcessOfThread(void) {
  exit(-1);
}

char *processWd(enginePo p) {
  return p->wd;
}

retCode setProcessWd(enginePo p, char *wd, integer len) {
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
    return different;
}

retCode markProcess(enginePo P, gcSupportPo G) {
#ifdef TRACEMEM
  if (traceMemory > noTracing)
    outMsg(logFile, "Mark process %d\n%_", P->processNo);
#endif
  P->stk = C_STACK(markPtr(G, (ptrPo) &P->stk));

  return Ok;
}

void markProcesses(enginePo owner, gcSupportPo G) {
  if (owner != Null)
    markProcess(owner, G);
  else
    processProcesses((procProc) markProcess, G);
}

void verifyProc(enginePo P, heapPo H) {
  verifyStack(P->stk, H);
}

void verifyProcesses(heapPo H) {
  if (H->owner != Null)
    verifyProc(H->owner, H);
  else
    processProcesses((procProc) verifyProc, H);
}
