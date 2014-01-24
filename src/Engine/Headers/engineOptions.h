#ifndef _OPTIONS_H_
#define _OPTIONS_H_

/*
 * Command line option flags
 */

#include "config.h"
#include "ooio.h"
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

extern logical tracing;		    /* tracing option */
extern logical interactive;	    /* interactive instruction tracing option */

extern logical traceMemory;	    /* memory tracing */

extern long heapSize;		/* How much memory to give the heap */
extern long stackSize;		/* How big is the stack */

int getOptions(int argc, char **argv);
void usage(char *name);

#endif
