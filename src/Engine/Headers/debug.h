//
// Created by Francis McCabe on 6/20/17.
//

#ifndef STAR_DEBUG_H
#define STAR_DEBUG_H

#include "engine.h"

termPo getLbl(termPo lbl, int32 arity);

DebugWaitFor insDebug(processPo p, insWord ins);
DebugWaitFor lineDebug(processPo p, termPo line);
DebugWaitFor enterDebug(processPo p);

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

integer pcCount;     /* How many instructions executed so far? */
void countIns(insWord ins);
void dumpInsCount();
void dumpInsStats();

void stackTrace(processPo p, ioPo out, logical showStack);
void dumpStackTrace(processPo p, ioPo out);

void showMethodCode(ioPo out, char *name, methodPo mtd);

integer displayDepth;

retCode setupDebugChannels();

#endif //STAR_DEBUG_H
