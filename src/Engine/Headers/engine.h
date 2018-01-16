/*
 * Header for the engine
 */
#ifndef _ENGINE_H_
#define _ENGINE_H_

#include "cafe.h"
#include "engineOptions.h"
#include "code.h"

typedef struct _processRec_ *processPo;
typedef struct _stack_frame_ *framePo;

#include "heap.h"
#include "code.h"

#define MAX_OPERANDS 1024

typedef enum {
  nextIns, nextSucc, nextBreak
} DebugWaitFor;

typedef enum {
  wait_timer
} ProcessState;

extern processPo newProcess(closurePo cl);
extern void switchProcessState(processPo p, ProcessState state);
extern void setProcessRunnable(processPo p);

extern termPo localVar(framePo fp, int64 off);

extern retCode liberror(processPo P, char *name, termPo code);

#endif
