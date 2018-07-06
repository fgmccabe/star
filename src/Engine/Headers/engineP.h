//
// Created by Francis McCabe on 1/14/18.
//

#ifndef CAFE_ENGINEP_H
#define CAFE_ENGINEP_H

#include "engine.h"
#include "termP.h"
#include "pkgP.h"
#include "codeP.h"
#include "opcodes.h"
#include "signals.h"
#include "heapP.h"
#include "labelsP.h"
#include "thr.h"

typedef struct _processRec_ {
  insPo pc;           /* current program counter */
  framePo fp;         /* current frame */
  methodPo prog;      /* current program */
  ptrPo sp;           /* current top of stack */
  termPo stackBase;   /* base of execution stack */
  termPo stackLimit;  /* Limit of execution stack */
  heapPo heap;        // Local heap for this process
  pthread_t threadID;      /* What is the posix thread ID? */
  char wd[MAXFILELEN]; // Each thread may have its own working directory.
  ProcessState state;                 /* What is the status of this process? */
  logical pauseRequest;         /* Has a pause of this process been requested? */
  ProcessState savedState;    /* Saved state of this process? */
  threadPo thread;    // What is the thread associated with this process
  integer processNo;  // What number process is this
#ifdef TRACEEXEC
  DebugWaitFor waitFor;
  logical tracing;
  integer traceCount; // How many are we waiting for?
#endif
} ProcessRec;

typedef struct _stack_frame_ {
  framePo fp;
  insPo rtn;          /* The return address entry */
  methodPo prog;      /* stacked program */
  termPo args[ZEROARRAYSIZE];
} StackFrame;

extern void initEngine();
extern retCode run(processPo P);

void initPackages();

retCode bootstrap(char *entry);

extern pthread_key_t processKey;
pthread_t ps_threadID(processPo p);
processPo ps_suspend(processPo p, ProcessState reason);
processPo ps_resume(register processPo p, register logical fr); /* resume process */
void ps_kill(processPo p);      /* kill process */

void pauseAllThreads(pthread_t except);
void resumeAllThreads(pthread_t except);
logical checkForPause(processPo P);

void *ps_client(processPo p);  /* Get the process's client information */
void *ps_set_client(processPo p, void *cl);

processPo runerr(processPo); /* non-fatal error */

void displayProcesses(void);
void displayProcess(processPo p);
void stackTrace(processPo p, ioPo out, logical showStack);
void verifyProc(processPo p, struct _gc_support_ *G);

retCode extendStack(processPo p, integer sfactor);
retCode extendHeap(heapPo p, int hfactor, long hmin);

#endif //CAFE_ENGINEP_H
