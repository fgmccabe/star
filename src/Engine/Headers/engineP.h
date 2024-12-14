//
// Created by Francis McCabe on 1/14/18.
//

#ifndef STAR_ENGINEP_H
#define STAR_ENGINEP_H

#include "engine.h"
#include "stackP.h"
#include "termP.h"
#include "pkgP.h"
#include "codeP.h"
#include "opcodes.h"
#include "signals.h"
#include "heapP.h"
#include "labelsP.h"
#include "normalP.h"
#include "thr.h"
#include "timers.h"

typedef struct processRec_ {
  stackPo stk;            // Current stack
  heapPo heap;            // Local heap for this process
  integer tryCounter;     // A counter of the number of try instructions executed
  pthread_t threadID;     /* What is the posix thread ID? */
  char wd[MAXFILELEN];    // Each thread may have its own working directory.
  ProcessState state;     /* What is the status of this process? */
  threadPo thread;        // What is the thread associated with this process
  integer processNo;      // What number process is this
  DebugWaitFor waitFor;
  logical tracing;
  integer traceCount;     // How many are we waiting for?
  framePo waterMark;      // Used to decide when to start debugging again
} ProcessRec;

extern void initEngine();
extern retCode run(processPo P);

retCode bootstrap(heapPo h, char *entry, char *rootWd);

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
extern timerPo runTimer;

void displayProcesses(void);
void displayProcess(processPo p);

void verifyProc(processPo P, heapPo H);
void verifyProcesses(heapPo H);
retCode markProcess(processPo P, gcSupportPo G);
void markProcesses(processPo owner, gcSupportPo G);

extern logical collectStats;

#endif //STAR_ENGINEP_H
