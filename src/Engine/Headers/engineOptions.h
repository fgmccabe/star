#ifndef _OPTIONS_H_
#define _OPTIONS_H_

/*
 * Command line option flags
 */

#include "config.h"
#include "starOptions.h"

#ifdef ALLTRACE
#define TRACEMEM
#define TRACEEXEC
#define TRACEMANIFEST
#define TRACEPKG
#define TRACESTATS
#define TRACEVERIFY
#endif

extern char copyRight[];

extern logical tracing;        /* tracing option */
extern logical insDebugging;
extern logical lineDebugging;
extern logical interactive;      /* interactive instruction tracing option */
extern logical traceCount;

extern logical traceMemory;      /* memory tracing */
extern logical tracePkg;
extern logical traceManifest;

extern long initHeapSize;    /* How much memory to give the heap */
extern long initStackSize;    /* How big is the stack */

extern char entry[MAX_SYMB_LEN];
extern char bootPkg[MAX_SYMB_LEN];  // boot package
extern char bootVer[MAX_SYMB_LEN];

extern char CWD[MAXFILELEN];
extern char repoDir[MAXFILELEN];


int getOptions(int argc, char **argv);
void usage(char *name);
void defltCWD();

#endif
