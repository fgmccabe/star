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
#endif

#define STAR_DEBUGGER "STAR_DEBUGGER"
#define STAR_MAIN "STAR_MAIN"
#define STAR_BOOT "STAR_BOOT"
#define STAR_DBG_OPTS "STAR_DEBUG_OPTS"

extern char *copyright;

extern logical enableVerify;      // Do we verify code that is loaded into engine
extern logical tracing;        /* tracing option */
extern logical insDebugging;
extern logical lineDebugging;
extern logical interactive;      /* interactive instruction tracing option */
extern logical runStats;

extern logical traceMemory;      /* memory tracing */
extern logical tracePkg;
extern logical traceManifest;
extern logical traceVerify;

extern long initHeapSize;    /* How much memory to give the heap */
extern long initStackSize;    /* How big is the stack */

extern char entry[MAX_SYMB_LEN];
extern char bootPkg[MAX_SYMB_LEN];  // boot package
extern char bootVer[MAX_SYMB_LEN];
extern PackageRec bootPkge;

extern char CWD[MAXFILELEN];
extern char repoDir[MAXFILELEN];


int getOptions(int argc, char **argv);
void defltCWD();

#endif
