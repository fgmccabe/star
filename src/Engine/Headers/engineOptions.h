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
#define TRACEPKG
#define TRACESTATS
#define TRACEVERIFY
#define TRACELOCK
#define TRACE_DBG
#endif

#define STAR_MAIN "STAR_MAIN"
#define STAR_BOOT "STAR_BOOT"
#define STAR_DBG_OPTS "STAR_DEBUG_OPTS"
#define STAR_DEBUGGER_PORT "STAR_DEBUGGER_PORT"
#define SYMBOL_DEBUG "STAR_DEBUG"

extern char *copyright;

extern logical enableVerify;      // Do we verify code that is loaded into engine
extern logical tracing;        /* tracing option */
extern logical insDebugging;
extern logical lineDebugging;
extern logical debugDebugging;
extern int debuggerPort;
extern logical showPkgFile;       // True if we show file instead of package during debugging
extern logical showColors;
extern logical interactive;      /* interactive instruction tracing option */
extern logical runStats;

extern logical traceMemory;      /* memory tracing */
extern logical validateMemory;   // Validate heap after every allocation
extern logical tracePkg;
extern logical traceManifest;
extern logical traceVerify;

extern long initHeapSize;        /* How much memory to give the heap */
extern long maxHeapSize;         // Maximum permitted size of heap
extern long initStackSize;       /* How big is the stack */
extern long maxStackSize;        // How big may the stack grow?

extern char bootEntry[MAX_SYMB_LEN];
extern char bootInit[MAX_SYMB_LEN];
extern char bootVer[MAX_SYMB_LEN];
extern PackageRec bootPkge;

extern char CWD[MAXFILELEN];
extern char repoDir[MAXFILELEN];

int getStarOptions(int argc, char **argv);
char * defltCWD();

#endif
