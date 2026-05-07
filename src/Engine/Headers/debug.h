//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_DEBUG_H
#define STAR_DEBUG_H

#include "engine.h"
#include "globals.h"
#include "code.h"

retCode showLoc(ioPo f, void* data, long depth, long precision, logical alt);

extern integer pcCount; /* How many instructions executed so far? */
void dumpStats();

retCode setupDebugChannels();

logical isDebuggableOp(ssaOp op);
logical isDebugging();

DebugWaitFor lineDebug(enginePo p, termPo lc);
DebugWaitFor bindDebug(enginePo p, termPo name, termPo val);
DebugWaitFor glbDebug(enginePo p, termPo loc, globalPo glb);
DebugWaitFor abortDebug(enginePo p, termPo lc);
DebugWaitFor callDebug(enginePo p, ssaOp op, termPo lc, termPo pr, termPo args);
DebugWaitFor tcallDebug(enginePo p, termPo lc, termPo pr, termPo args);
DebugWaitFor ocallDebug(enginePo p, ssaOp op, termPo lc, termPo pr, termPo args);
DebugWaitFor tocallDebug(enginePo p, termPo lc, termPo pr, termPo args);
DebugWaitFor entryDebug(enginePo p, termPo lc, labelPo lbl);
DebugWaitFor retDebug(enginePo p, termPo lc, termPo pr, termPo vl);
DebugWaitFor xretDebug(enginePo p, termPo lc, termPo pr, termPo vl);
DebugWaitFor assignDebug(enginePo p, termPo lc, termPo dst, termPo src);
DebugWaitFor fiberDebug(enginePo p, termPo lc, termPo vl);
DebugWaitFor suspendDebug(enginePo p, termPo lc, termPo cn, termPo evt);
DebugWaitFor resumeDebug(enginePo p, termPo lc, termPo cn, termPo ev);
DebugWaitFor retireDebug(enginePo p, termPo lc, termPo cn, termPo ev);

#endif //STAR_DEBUG_H
