//
// Created by Francis McCabe on 2018-12-24.
//

#ifndef STAR_DEBUGP_H
#define STAR_DEBUGP_H

#include "debug.h"
#include "bkpoint.h"
#include "vector.h"
#include "stack.h"

typedef DebugWaitFor (*debugCmd)(char *line, processPo p, termPo loc, void *cl);

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

DebugWaitFor insDebug(processPo p);
DebugWaitFor enterDebug(processPo p);


insPo disass(ioPo out, stackPo stk, methodPo mtd, insPo pc);
#endif //STAR_DEBUGP_H
