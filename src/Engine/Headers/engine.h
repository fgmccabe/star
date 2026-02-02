/*
 * Header for the engine
 */
#ifndef ENGINE_H_
#define ENGINE_H_

#include "star.h"
#include "engineOptions.h"
#include "code.h"
#include "heap.h"

typedef struct engineRecord_ *enginePo;

typedef enum {
  stepInto, stepOver, stepOut, nextBreak, never, quitDbg, moreDebug
} DebugWaitFor;

typedef enum {
  quiescent,        /* process has yet to execute */
  runnable,
  wait_io,        /* process is waiting for I/O */
  wait_timer,        /* waiting for an interval times */
  wait_term,    /* process is waiting for another thread to terminate */
  wait_child,                           // Wait for a child process
  in_exclusion,
  dead
} ProcessState;

enginePo newEngine(heapPo h, int execJit, methodPo mtd, char *rootWd, termPo rootArg);
void switchProcessState(enginePo p, ProcessState state);
void setProcessRunnable(enginePo p);
integer processNo(enginePo p);

typedef retCode (*procProc)(enginePo p, void *cl);
retCode processProcesses(procProc p, void *cl);
enginePo getProcessOfThread(void);

heapPo processHeap(enginePo p);

char *processWd(enginePo p);
retCode setProcessWd(enginePo p, char *wd, integer len);

termPo commandLine(heapPo h);

// void pshVal(enginePo p, termPo v);
// termPo popVal(enginePo p);

extern __thread enginePo currentProcess;
#endif
