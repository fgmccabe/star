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

DebugWaitFor lineDebug(processPo p, termPo lc);
DebugWaitFor abortDebug(processPo p, termPo lc);
DebugWaitFor callDebug(processPo p, termPo lc, OpCode op, termPo pr);
DebugWaitFor tcallDebug(processPo p, termPo lc, termPo pr);
DebugWaitFor ocallDebug(processPo p, OpCode op, termPo lc, termPo pr);
DebugWaitFor tocallDebug(processPo p, termPo lc, termPo pr);
DebugWaitFor entryDebug(processPo p, termPo lc, termPo pr);
DebugWaitFor retDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor xretDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor assignDebug(processPo p, termPo lc);
DebugWaitFor fiberDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor suspendDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor resumeDebug(processPo p, termPo lc, termPo vl);
DebugWaitFor retireDebug(processPo p, termPo lc, termPo vl);

#endif //STAR_DEBUG_H
