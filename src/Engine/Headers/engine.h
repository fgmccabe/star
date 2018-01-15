/*
 * Header for the engine
 */
#ifndef _ENGINE_H_
#define _ENGINE_H_

#include "cafe.h"
#include "engineOptions.h"
#include "pkgP.h"
#include "codeP.h"
#include "heap.h"
#include "opcodes.h"
#include "signals.h"

#define MAX_OPERANDS 1024

typedef struct _processRec_ *processPo;
typedef struct _stack_frame_ *framePo;

typedef enum {
  nextIns, nextSucc, nextBreak
} DebugWaitFor;

typedef struct _processRec_ {
  insPo pc;        /* current program counter */
  framePo fp;        /* current frame */
  closurePo prog;      /* current closure */
  ptrPo sp;        /* current top of stack */
  termPo stackBase;      /* base of execution stack */
  termPo stackLimit;      /* Limit of execution stack */
#ifdef TRACEEXEC
  DebugWaitFor waitFor;
#endif
} ProcessRec;

typedef struct _stack_frame_ {
  framePo fp;
  insPo rtn;        /* The return address entry */
  closurePo env;      /* stacked closure */
} StackFrame;

#define ENV_OFFSET sizeof(framePo)

extern void initHeap(long heapSize);
extern void initEngine();
extern int loadAndGo(char *prog, int argc, char **argv);
extern integer run(processPo P, heapPo heap);
extern void markRoot(void **addr);
extern closurePo allocate(heapPo heap, methodPo mtd);

extern logical compare_and_swap(closurePo cl, int64 expect, int64 repl);

extern void syserr(const char *msg);

extern processPo newProcess(closurePo cl);

extern termPo localVar(framePo fp, int64 off);

void initPackages();

packagePo loadedPackage(char *package);

char *loadedVersion(char *package);

void bootstrap(char *entry, char *bootPkg, char *version);

#endif
