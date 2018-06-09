#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include "config.h"
#include "asm.h"

#ifdef ALLTRACE
#define DEBUGPARSE
#define TRACEASSM
#endif

#ifdef DEBUGPARSE
extern int ssdebug;
extern int ss_flex_debug;
#endif

#ifdef TRACEASSM
extern logical debugAssem;
#endif

extern int getOptions(int argc, char **argv);

#endif
