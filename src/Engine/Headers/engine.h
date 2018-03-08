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

#define MAX_OPERANDS 1024

typedef enum {
  nextIns, nextSucc, nextBreak
} DebugWaitFor;

typedef enum {
  wait_io,
  wait_timer,
  runnable,
} ProcessState;

typedef struct _return_code_ {
  retCode ret;
  termPo rslt;
} ReturnStatus;

extern processPo newProcess(methodPo cl);
extern void switchProcessState(processPo p, ProcessState state);
extern void setProcessRunnable(processPo p);

extern heapPo processHeap(processPo p);

extern char *processWd(processPo p);
extern retCode setProcessWd(processPo p, char *wd, integer len);

extern termPo localVar(framePo fp, int64 off);

extern ReturnStatus liberror(processPo P, char *name, termPo code);

#endif
