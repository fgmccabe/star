#ifndef _OPTIONS_H_
#define _OPTIONS_H_

/*
 * Command line option flags
 */

#include <pkgP.h>
#include "config.h"
#include "starOptions.h"

#ifdef ALLTRACE
#define TRACEMEM
#define TRACEEXEC
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

extern char *copyright;

extern char bootEntry[MAX_SYMB_LEN];
extern char bootVer[MAX_SYMB_LEN];
extern logical useMicroBoot;
extern PackageRec bootPkge;
extern PackageRec microBootPkge;

extern char CWD[MAXFILELEN];
extern char repoDir[MAXFILELEN];

int getEngineOptions(int argc, char **argv);
char *defltCWD();

#endif
