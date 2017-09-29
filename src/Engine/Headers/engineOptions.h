#ifndef _OPTIONS_H_
#define _OPTIONS_H_

/*
 * Command line option flags
 */

#include "config.h"
#include "cafeOptions.h"

#ifdef ALLTRACE
#define MEMTRACE
#define TRACEEXEC
#endif

#ifdef MEMTRACE
extern logical traceMemory;
#endif

#ifdef TRACEEXEC
extern logical tracing;
#endif

extern char copyRight[];

extern logical tracing;        /* tracing option */
extern logical debugging;
extern logical interactive;      /* interactive instruction tracing option */

extern logical traceMemory;      /* memory tracing */

extern long initHeapSize;    /* How much memory to give the heap */
extern long initStackSize;    /* How big is the stack */

extern char entry[MAX_SYMB_LEN];
extern char bootPkg[MAX_SYMB_LEN];  // boot package
extern char bootVer[MAX_SYMB_LEN];

int getOptions(int argc, char **argv);
void usage(char *name);
void defltCWD();

#endif
