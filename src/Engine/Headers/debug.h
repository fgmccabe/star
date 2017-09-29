//
// Created by Francis McCabe on 6/20/17.
//

#ifndef CAFE_DEBUG_H
#define CAFE_DEBUG_H

#include "engine.h"

extern logical SymbolDebug;

#ifdef EXECTRACE
extern logical debugging;	/* Level of debugging */
#endif

extern long cmdCounter;
extern logical tracing;	        /* do we show each instruction */

extern void debug_stop(integer pcCount, processPo p, closurePo env, insPo pc, framePo fp, ptrPo sp);
extern insPo disass(unsigned long long int pcCount, processPo p, closurePo env, insPo pc, framePo fp, ptrPo sp);

#endif //CAFE_DEBUG_H
