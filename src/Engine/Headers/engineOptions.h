#ifndef _OPTIONS_H_
#define _OPTIONS_H_

/*
 * Command line option flags
 */

#include "config.h"
#include "io.h"
#include "starOptions.h"
#include "buddy.h"

#ifdef ALLTRACE
#define TRACEMEM
#define TRACESTACK
#define TRACEEXEC
#define TRACEJIT
#define TRACEMANIFEST
#define TRACECAPABILITY
#define TRACEPKG
#define TRACESTATS
#define TRACEVERIFY
#define TRACELOCK
#define TRACE_DBG
#define TRACEDECODE
#endif

#define STAR_MAIN "STAR_MAIN"
#define STAR_BOOT "STAR_BOOT"
#define STAR_DBG_OPTS "STAR_DEBUG_OPTS"
#define STAR_DEBUGGER_PORT "STAR_DEBUGGER_PORT"
#define SYMBOL_DEBUG "STAR_DEBUG"
#define STAR_JIT_THRESHOLD "STAR_JIT_THRESHOLD"

extern char *copyright;

extern char bootEntry[MAX_SYMB_LEN];
extern char CWD[MAXFILELEN];
extern char repoDir[MAXFILELEN];

int getEngineOptions(int argc, char **argv);
char *defltCWD();

#endif
