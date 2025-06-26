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

typedef struct engineRecord_ {
  stackPo stk;            // Current stack
  heapPo heap;            // Local heap for this process
  pthread_t threadID;     /* What is the posix thread ID? */
  char wd[MAXFILELEN];    // Each thread may have its own working directory.
  ProcessState state;     /* What is the status of this process? */
  threadPo thread;        // What is the thread associated with this process
  integer processNo;      // What number process is this
  DebugWaitFor waitFor;
  logical tracing;
  integer traceCount;     // How many are we waiting for?
  framePo waterMark;      // Used to decide when to start debugging again
} EngineRecord;

extern void initEngine();
extern retCode run(enginePo P);

retCode bootstrap(heapPo h, char *entry, char *rootWd);

extern pthread_key_t processKey;

void ps_kill(enginePo p);      /* kill process */



extern timerPo runTimer;

void verifyProc(enginePo P, heapPo H);
void verifyProcesses(heapPo H);
retCode markProcess(enginePo P, gcSupportPo G);
void markProcesses(enginePo owner, gcSupportPo G);

void abort_star(enginePo P, termPo lc, termPo msg);

extern logical collectStats;

#endif //STAR_ENGINEP_H
