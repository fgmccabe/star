//
// Created by Francis McCabe on 6/20/17.
//

#ifndef CAFE_DEBUG_H
#define CAFE_DEBUG_H

#include "engine.h"

extern int32 insOperand(processPo p);
extern termPo getLbl(termPo lbl,int32 arity);

extern DebugWaitFor insDebug(integer pcCount, processPo p);
extern DebugWaitFor lineDebug(processPo p, termPo ln);
extern DebugWaitFor callDebug(processPo p, termPo call);
extern DebugWaitFor tailDebug(processPo p, termPo call);
extern DebugWaitFor retDebug(processPo p, termPo call);

extern retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt);

extern integer pcCount;     /* How many instructions executed so far? */
extern void countIns(insWord ins);
extern void dumpInsCount();

#endif //CAFE_DEBUG_H
