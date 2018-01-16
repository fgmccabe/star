//
// Created by Francis McCabe on 1/14/18.
//

#ifndef CAFE_ENGINEP_H
#define CAFE_ENGINEP_H

#include "engine.h"
#include "termP.h"
#include "pkgP.h"
#include "codeP.h"
#include "opcodes.h"
#include "signals.h"

typedef struct _processRec_ {
  insPo pc;           /* current program counter */
  framePo fp;         /* current frame */
  methodPo prog;      /* current program */
  ptrPo sp;           /* current top of stack */
  termPo stackBase;   /* base of execution stack */
  termPo stackLimit;  /* Limit of execution stack */
#ifdef TRACEEXEC
  DebugWaitFor waitFor;
#endif
} ProcessRec;

typedef struct _stack_frame_ {
  framePo fp;
  insPo rtn;          /* The return address entry */
  methodPo prog;      /* stacked program */
} StackFrame;

#define ENV_OFFSET sizeof(framePo)

extern void initEngine();
extern int loadAndGo(char *prog, int argc, char **argv);
extern retCode run(processPo P, heapPo heap);

void initPackages();

packagePo loadedPackage(char *package);

char *loadedVersion(char *package);

void bootstrap(char *entry, char *bootPkg, char *version);

#endif //CAFE_ENGINEP_H
