//
// Created by Francis McCabe on 1/14/18.
//

#ifndef STAR_ENGINEP_H
#define STAR_ENGINEP_H

#include "engine.h"
#include "stackP.h"
#include "pkgP.h"
#include "codeP.h"
#include "heapP.h"
#include "timers.h"

typedef struct engineRecord_ {
  stackPo stk; // Current stack
  pthread_t threadID; /* What is the posix thread ID? */
  char wd[MAXFILELEN]; // Each thread may have its own working directory.
  ProcessState state; /* What is the status of this process? */
  integer processNo; // What number process is this
  DebugWaitFor waitFor;
  logical tracing;
  integer traceCount; // How many are we waiting for?
  framePo waterMark; // Used to decide when to start debugging again
} EngineRecord;

extern void initEngine();
ValueReturn run(enginePo P);
ValueReturn exec(enginePo P);

int32 bootstrap(char *entry, char *rootWd);

extern pthread_key_t processKey;

void ps_kill(enginePo p); /* kill process */

extern timerPo runTimer;

void verifyProc(enginePo P);
void verifyProcesses(void);
retCode markProcess(enginePo P, gcSupportPo G);
void markProcesses(gcSupportPo G);
void verifyEngine(enginePo p);

retCode scanProcesses(termHelper helper, void *cl);

void abort_star(enginePo P, termPo lc, termPo msg);

extern logical collectStats;

#endif //STAR_ENGINEP_H
