/*
 * Header for the engine
 */
#ifndef _ENGINE_H_
#define _ENGINE_H_

#include "cafe.h"
#include "engineOptions.h"
#include "code.h"
#include "heap.h"

typedef struct _processRec_ *processPo;
typedef struct _stack_frame_ *framePo;

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


typedef struct _return_code_ {
  retCode ret;
  termPo rslt;
} ReturnStatus;

extern processPo newProcess(methodPo mtd);
extern void setupProcess(processPo P,methodPo mtd);
extern void switchProcessState(processPo p, ProcessState state);
extern void setProcessRunnable(processPo p);
extern ProcessState processState(processPo p);

typedef retCode (*procProc)(processPo p, void *cl);
retCode processProcesses(procProc p, void *cl);
processPo getProcessOfThread(void);

extern heapPo processHeap(processPo p);

extern char *processWd(processPo p);
extern retCode setProcessWd(processPo p, char *wd, integer len);

extern ReturnStatus liberror(processPo P, char *name, termPo code);

#endif
