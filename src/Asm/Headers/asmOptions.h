#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#include "config.h"
#include "asm.h"
#include "starOptions.h"

#ifdef ALLTRACE
#define DEBUGPARSE
#define TRACEASSM
#define TRACEMANIFEST
#endif

#ifdef DEBUGPARSE
extern int ssdebug;
extern int ss_flex_debug;
#endif

extern int getAsmOptions(int argc, char **argv);

#endif
