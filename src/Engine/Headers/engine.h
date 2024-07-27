/*
 * Header for the engine
 */
#ifndef ENGINE_H_
#define ENGINE_H_

#include "star.h"
#include "engineOptions.h"
#include "code.h"
#include "heap.h"
#include "escape.h"

typedef struct processRec_ *processPo;

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

processPo newProcess(heapPo h, methodPo mtd, char *rootWd, termPo rootArg);
void switchProcessState(processPo p, ProcessState state);
void setProcessRunnable(processPo p);
ProcessState processState(processPo p);
integer processNo(processPo p);

typedef retCode (*procProc)(processPo p, void *cl);
retCode processProcesses(procProc p, void *cl);
processPo getProcessOfThread(void);

heapPo processHeap(processPo p);

char *processWd(processPo p);
retCode setProcessWd(processPo p, char *wd, integer len);

termPo commandLine(heapPo H);

extern __thread processPo currentProcess;
#endif
