/*
 * Process the command line options
 */
#include "config.h"
#include "engine.h"
#include <ooio.h>
#include <stdlib.h>

#include "options.h"
#include "version.h"			/* Version ID for the Cafe system */

logical tracing = False;	    /* tracing option */
logical interactive = False;	    /* interaction instruction tracing */

logical traceMemory = False;	    /* memory tracing */

long heapSize = 200*1024;		/* How much memory to give the heap */
long stackSize = 1024;			/* How big is the stack */

static long parseSize(char *text);

static retCode debugOption(char *option,logical enable,void *cl)
{
  char *c = option;
  while(*c){
    switch(*c++){
    case 't':		/* tracing mode */
#ifdef TRACEEXEC
      tracing = True;
      interactive = False;
      continue;
#else
      logMsg(logFile,"Tracing not enabled\n");
      return Error;
#endif

    case 'i':		/* instruction debugging mode */
#ifdef TRACEEXEC
      tracing = True;
      interactive = True;
      continue;
#else
      logMsg(logFile,"Tracing not enabled\n");
      return Error;
#endif

    case 'm':		/* trace memory allocations  */
#ifdef MEMTRACE
      traceMemory = True;
      continue;
#else
      logMsg(logFile,"memory tracing not enabled");
      return Error;
#endif 
      break;

    case '*':		/* trace everything */
#ifdef TRACEEXEC
      tracing = True;
      interactive = True;
#endif
#ifdef MEMTRACE
      traceMemory = True;
#endif
      break;
    }
  }
  return Ok;
}

static retCode heapSizeOption(char *option,logical enable,void *cl)
{
  heapSize = parseSize(option);
  return Ok;
}

static retCode stackSizeOption(char *option,logical enable,void *cl)
{
  stackSize = parseSize(option);
  return Ok;
}

static retCode displayVersion(char *option,logical enable,void *cl)
{
  return outMsg(logFile,"%s",version);
}

static logical isDigit(char ch)
{
  return ch>='0' && ch<='9';
}

long parseSize(char *text)
{
  char *p = text;
  int scale = 1;
  while(*p!='\0' && isDigit(*p))
    p++;
  if(*p!='\0'){
    switch(*p){
    case 'k':
    case 'K':
      scale = 1024;
      break;
    case 'm':
    case 'M':
      scale = 1024*1024;
      break;
    case 'g':
    case 'G':
      scale = 1024*1024*1024;
      break;
    }
    *p = '\0';
  }
  return atoi(text)*scale;
}

Option options[] = {
  { 'd', "debug", True, debugOption, NULL, "-d|--debug <flags>"},
  { 'h', "heapSize", True, heapSizeOption, NULL, "-h|--heapSize <size>[kmG]"},
  { 's', "stackSize", True, stackSizeOption, NULL, "-s|--stackSize <size>[kmG]"},
  { 'v', "version", False, displayVersion, NULL, "-v|--version"}};

int getOptions(int argc, char **argv)
{
  //  splitFirstArg(argc,argv,&argc,&argv);

  return processOptions(argc,argv,options,NumberOf(options));
}

void usage(char *name)
{
  showUsage(name,options,NumberOf(options));
}

