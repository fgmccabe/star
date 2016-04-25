#ifndef OPTIONS_H_
#define OPTIONS_H_

extern logical tracing;    /* tracing option */
extern logical debugAssem;   /* debug the assembling process */
extern logical debugParse;   /* debug the parsing process */
extern logical traceMemory;    /* memory tracing */
extern logical debugCodeGen;   /* debug code generation */
extern logical parseOnly;    /* set to true for parsing only */
extern logical compileOnly;    /* set to true for compile only */

extern long heapSize;   /* How much memory to give the heap */
extern long stackSize;      /* How big is the stack */


extern long parseSize(char *text);


extern int getOptions(int argc, char **argv);

extern void cafeUsage(char *name);

#endif