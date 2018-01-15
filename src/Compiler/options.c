/*
 * Process the command line options
 */
#include "config.h"
#include "compiler.h"
#include <ooio.h>
#include <stdlib.h>

#include "version.h"			/* Version ID for the Cafe system */
#include "options.h"
#include "asmOptions.h"

logical tracing = False;		/* tracing option */
logical debugAssem = False;		/* debug the assembling process */
logical debugParse = False;		/* debug the parsing process */
logical traceMemory = False;		/* memory tracing */
logical debugCodeGen = False;		/* debug code generation */
logical parseOnly = False;		/* set to true for parsing only */
logical compileOnly = False;		/* set to true for compile only */

long heapSize = 200*1024;		/* How much memory to give the heap */
long stackSize = 1024;			/* How big is the stack */

#ifdef ALLTRACE
extern int yy_flex_debug;
#endif

static retCode debugOption(char *option,logical enable,void *cl)
{
  char *c = option;
  while(*c){
    switch(*c++){
    case 'a':			/* trace assembly */
#ifdef TRACEASSM
      debugAssem = True;
      continue;
#else
      logMsg(logFile,"Assembly debugging not enabled\n");
      return Error;
#endif

    case 'c':			/* trace code generation */
#ifdef TRACECODEGEN
      debugCodeGen = True;
      continue;
#else
      logMsg(logFile,"Code generation debugging not enabled\n");
      return Error;
#endif

    case 't':		/* tracing mode */
#ifdef TRACEEXEC
      tracing = True;
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

    case 'p':		/* help with debugging the parser */
#ifdef DEBUGPARSE
      debugParse = True;
      yydebug = 1;
      yy_flex_debug = 0;
      continue;
#else
      logMsg(logFile,"debug parse not enabled");
      return Error;
#endif
      break;

    case 'l':		/* help with debugging the tokenizer */
#ifdef DEBUGPARSE
      yy_flex_debug = 1;
      continue;
#else
      logMsg(logFile,"debug token not enabled");
      return Error;
#endif
      break;

    case '*':		/* trace everything */
#ifdef ALLTRACE
      tracing = True;
      debugParse = True;
      yy_flex_debug = 1;
      traceMemory = True;
      debugAssem = True;
      debugCodeGen = True;
#else
      logMsg(logFile,"debugging not enabled\n");
      return Error;
#endif
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

static retCode setCompileOnly(char *option,logical enable,void *cl)
{
  compileOnly = True;
  return Ok;
}

static retCode setParseOnly(char *option,logical enable,void *cl)
{
  parseOnly = True;
  return Ok;
}

static retCode setHome(char *option,logical enable,void *cl)
{
  setCafeHome(uniDuplicate(option));
  return Ok;
}

static retCode setLogFile(char *option,logical envale, void *cl)
{
  char buff[MAXFILELEN];
  _uni((unsigned char*)option,buff,NumberOf(buff));
  initLogfile(buff);
  return Ok;
}

static logical isDigit(char ch)
{
  return ch>='0' && ch<='9';
}

long parseSize(char *text)
{
  char *p = text;
  long scale = 1;
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
  { 'D', "debug", True, debugOption, Null, "-d|--debug <flags>"},
  { 'h', "heapSize", True, heapSizeOption, Null,"-h|--heapSize <size>[kmG]"},
  { 's', "stackSize", True, stackSizeOption, Null,"-s|--stackSize <size>[kmG]"},
  { 'v', "version", False, displayVersion, Null,"-v|--version"},
  { 'P', "parseOnly", False, setParseOnly, Null,"-P|--parseOnly"},
  { 'C', "compileOnly", False, setCompileOnly, Null, "-C|--compileOnly"},
  { 'H', "cafeHome", True, setHome, Null, "-H|--cafeHome <path>"},
  { 'L', "logFile", True, setLogFile, Null, "-L|--logFile <path>"}};

int getOptions(int argc, char **argv)
{
  //  splitFirstArg(argc,argv,&argc,&argv);

  return processOptions(argc,argv,options,NumberOf(options));
}

void cafeUsage(char *name)
{
  showUsage(name,options,NumberOf(options));
}

