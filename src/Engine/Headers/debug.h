//
// Created by Francis McCabe on 6/20/17.
//

#ifndef CAFE_DEBUG_H
#define CAFE_DEBUG_H

#include "engine.h"

extern termPo getLbl(termPo lbl,int32 arity);

extern DebugWaitFor insDebug(integer pcCount, processPo p);
extern DebugWaitFor lineDebug(processPo p, termPo ln);
extern DebugWaitFor callDebug(processPo p, termPo call);
extern DebugWaitFor oCallDebug(processPo p, termPo call,termPo lbl);
extern DebugWaitFor tailDebug(processPo p, termPo call);
extern DebugWaitFor retDebug(processPo p, termPo val);

extern retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

extern integer pcCount;     /* How many instructions executed so far? */
extern void countIns(insWord ins);
extern void dumpInsCount();
extern void dumpInsStats();

extern void stackTrace(processPo p, ioPo out, logical showStack);
extern void dumpStackTrace(processPo p, ioPo out);

#endif //CAFE_DEBUG_H
