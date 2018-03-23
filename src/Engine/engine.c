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

void initEngine() {
  prPool = newPool(sizeof(ProcessRec), 32);
}

retCode bootstrap(char *entry) {
  labelPo umain = declareLbl(entry, 0);
  methodPo mainMtd = labelCode(umain);

  if (mainMtd != Null) {
    processPo p = newProcess(mainMtd);
    return run(p);
  } else
    return Error;
}

static uint16 haltCode[] = {Halt};

processPo newProcess(methodPo mtd) {
  processPo P = (processPo) allocPool(prPool);

  P->prog = mtd;
  P->pc = entryPoint(mtd);
  P->stackBase = (termPo) malloc(sizeof(integer) * initStackSize);
  P->stackLimit = &P->stackBase[initStackSize];
  P->heap = currHeap;
  P->state = P->savedState = quiescent;
  P->pauseRequest = False;
  P->waitFor = debugging ? nextIns : never;
  P->hasEnter = False;

  uniNCpy(P->wd, NumberOf(P->wd), CWD, NumberOf(CWD));

  P->fp = (framePo) P->stackLimit;

  // cap the stack with a halting stop.

  ptrPo sp = (ptrPo) P->fp;
  *--sp = (termPo) mtd;
  *--sp = (termPo) &haltCode[0];

  P->sp = sp;

  return P;
}

void ps_kill(processPo p) {
  if (p != NULL) {
    pthread_t thread = p->threadID;

    pthread_cancel(thread);    /* cancel the thread */
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

retCode processProcesses(procProc p, void *cl) {
  return Ok;
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
