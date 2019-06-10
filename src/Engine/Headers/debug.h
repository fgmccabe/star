//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_DEBUG_H
#define STAR_DEBUG_H

#include "engine.h"

extern termPo getLbl(termPo lbl, int32 arity);

extern DebugWaitFor insDebug(processPo p, insWord ins);
extern DebugWaitFor lineDebug(processPo p, termPo line);
extern DebugWaitFor enterDebug(processPo p);

extern retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

extern integer pcCount;     /* How many instructions executed so far? */
extern void countIns(insWord ins);
extern void dumpInsCount();
extern void dumpInsStats();

extern void stackTrace(processPo p, ioPo out, logical showStack);
extern void dumpStackTrace(processPo p, ioPo out);

extern integer displayDepth;

extern retCode setupDebugChannels();

#endif //STAR_DEBUG_H
