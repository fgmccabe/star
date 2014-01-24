/*
 * Header for the engine
 */
#ifndef _ENGINE_H_
#define _ENGINE_H_

#include "config.h"
#include <ooio.h>
#include "engineOptions.h"
#include "codeP.h"
#include "heap.h"

typedef uint64 *ptrPo;			/* pointer to a 64bit word */

typedef struct _processRec_ *processPo;
typedef struct _stack_frame_ *framePo;

typedef struct _processRec_ {
  insPo pc;				/* current program counter */
  framePo fp;				/* current frame */
  closurePo prog;			/* current closure */
  ptrPo sp;				/* current top of stack */
  ptrPo stackBase;			/* base of execution stack */
  ptrPo stackLimit;			/* Limit of execution stack */
} ProcessRec;

typedef struct _stack_frame_ {
  framePo fp;
  insPo rtn;				/* The return address entry */
  closurePo env;			/* stacked closure */
} StackFrame;

#define ENV_OFFSET sizeof(framePo)

extern void initHeap(long heapSize);
extern void initEngine();
extern int loadAndGo(uniChar *prog,int argc,char **argv);
extern void run(processPo P,heapPo heap);
extern void markRoot(void **addr);
extern closurePo allocate(heapPo heap,methodPo mtd);

extern void debug_stop(int32 pcCount,processPo p,closurePo env,insPo pc,framePo fp,ptrPo sp);
extern insPo disass(int32 pcCount,processPo p,closurePo env,insPo pc,framePo fp,ptrPo sp);

extern logical compare_and_swap(closurePo cl,int64 expect,int64 repl);

extern void syserr(const char* msg);

extern processPo newProcess(closurePo cl);

extern ptrPo localVar(framePo fp,int off);

#endif
