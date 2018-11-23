//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_DEBUG_H
#define STAR_DEBUG_H

#include "engine.h"

extern termPo getLbl(termPo lbl, int32 arity);

extern DebugWaitFor insDebug(processPo p, integer pcCount, insWord ins);
extern DebugWaitFor callDebug(processPo p, termPo call);
extern DebugWaitFor tailDebug(processPo p, termPo call);
extern DebugWaitFor ocallDebug(processPo p, termPo call, int32 arity);
extern DebugWaitFor retDebug(processPo p, termPo val);
extern DebugWaitFor lineDebug(processPo p, termPo line);

extern retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

extern integer pcCount;     /* How many instructions executed so far? */
extern void countIns(insWord ins);
extern void dumpInsCount();
extern void dumpInsStats();

extern void stackTrace(processPo p, ioPo out, logical showStack);
extern void dumpStackTrace(processPo p, ioPo out);

#endif //STAR_DEBUG_H
