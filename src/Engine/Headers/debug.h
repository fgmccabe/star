//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_DEBUG_H
#define STAR_DEBUG_H

#include "engine.h"
#include "code.h"

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

extern integer pcCount;     /* How many instructions executed so far? */
void dumpStats();

void showMethodCode(ioPo out, char *msg, methodPo mtd);

retCode setupDebugChannels();

DebugWaitFor lineDebug(enginePo p, termPo lc);
DebugWaitFor bindDebug(enginePo p, termPo name, int32 offset);
DebugWaitFor abortDebug(enginePo p, termPo lc);
DebugWaitFor callDebug(enginePo p, OpCode op, termPo lc, termPo pr);
DebugWaitFor tcallDebug(enginePo p, termPo lc, termPo pr);
DebugWaitFor ocallDebug(enginePo p, OpCode op, termPo lc, termPo pr);
DebugWaitFor tocallDebug(enginePo p, termPo lc, termPo pr);
DebugWaitFor entryDebug(enginePo p, termPo lc, labelPo lbl);
DebugWaitFor retDebug(enginePo p, termPo lc, termPo vl);
DebugWaitFor xretDebug(enginePo p, termPo lc, termPo vl);
DebugWaitFor assignDebug(enginePo p, termPo lc);
DebugWaitFor fiberDebug(enginePo p, termPo lc, termPo vl);
DebugWaitFor suspendDebug(enginePo p, termPo lc, termPo vl);
DebugWaitFor resumeDebug(enginePo p, termPo lc, termPo vl);
DebugWaitFor retireDebug(enginePo p, termPo lc, termPo vl);

#endif //STAR_DEBUG_H
