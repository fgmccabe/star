/*
 * Header for the engine
 */
#ifndef _ENGINE_H_
#define _ENGINE_H_

#include "star.h"
#include "engineOptions.h"
#include "code.h"
#include "heap.h"
#include "capability.h"

typedef struct processRec_ *processPo;
typedef struct stack_frame_ *framePo;

typedef enum {
  stepInto, stepOver, nextBreak, never, quitDbg, moreDebug
} DebugWaitFor;

typedef enum {
  quiescent,        /* process has yet to execute */
  runnable,
  wait_io,        /* process is waiting for I/O */
  wait_timer,        /* waiting for an interval times */
  wait_term,    /* process is waiting for another thread to terminate */
  wait_lock,        // Waiting for a lock to be released
  wait_child,                           // Wait for a child process
  wait_rendezvous,           /* Waiting for a rendezvous to release */
  in_exclusion,
  dead
} ProcessState;

typedef struct return_code_ {
  retCode ret;
  termPo result;
} ReturnStatus;

processPo newProcess(methodPo mtd, char *rootWd, capabilityPo processCap, termPo rootArg);
void setupProcess(processPo P, methodPo mtd);
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

ReturnStatus liberror(processPo P, char *name, termPo code);
termPo commandLine(heapPo H);
#endif
