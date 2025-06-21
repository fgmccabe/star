//
// Created by Francis McCabe on 2018-12-24.
//

#ifndef STAR_DEBUGP_H
#define STAR_DEBUGP_H

#include "debug.h"
#include "bkpoint.h"
#include "vector.h"
#include "stack.h"

typedef DebugWaitFor (*debugCmd)(char *line, processPo p, termPo lc, void *cl);

typedef retCode (*completeCb)(strBufferPo, integer cx);

typedef struct {
  codePoint c;
  char *usage;
  void *cl;
  debugCmd cmd;
  completeCb complete;
} DebugOption;

typedef struct {
  int count;
  completeCb deflt;
  DebugOption opts[ZEROARRAYSIZE];
} DebugOptions, *debugOptPo;

extern logical tracing;        /* tracing option */
extern logical insDebugging;
extern logical lineDebugging;
extern logical debugDebugging;
extern integer debuggerPort;
extern logical showPkgFile;       // True if we show file instead of package during debugging
extern logical showColors;
extern logical interactive;      /* interactive instruction tracing option */
extern logical stackVerify;      // Are we dynamically verifying the stack

DebugWaitFor insDebug(processPo p);
DebugWaitFor enterDebug(processPo p, termPo lc);
DebugWaitFor lineDebug(processPo p, termPo lc);

DebugWaitFor abortDebug(processPo p, termPo lc);
DebugWaitFor callDebug(processPo p, termPo lc, termPo pr);
DebugWaitFor tcallDebug(processPo p, termPo lc, termPo pr);
DebugWaitFor ocallDebug(processPo p, termPo lc, termPo pr);
DebugWaitFor entryDebug(processPo p, termPo lc);
DebugWaitFor retDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor assignDebug(processPo p, termPo lc);
DebugWaitFor fiberDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor suspendDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor resumeDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor retireDebug(processPo p, termPo lc, termPo vl);

insPo disass(ioPo out, stackPo stk, methodPo mtd, insPo pc);
#endif //STAR_DEBUGP_H
