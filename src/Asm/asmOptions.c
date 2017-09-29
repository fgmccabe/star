/*
 * Process the command line options
 */
#include "config.h"
#include "asm.h"

#include "version.h"			/* Version ID for the Cafe system */
#include <options.h>
#include "asmOptions.h"

logical debugAssem = False;		/* debug the assembling process */
logical debugParse = False;		/* debug the parsing process */
logical parseOnly = False;		/* set to true for parsing only */

extern int ss_flex_debug;

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

    case 'p':		/* help with debugging the parser */
#ifdef DEBUGPARSE
      debugParse = True;
      ss_flex_debug = 1;
      continue;
#else
      logMsg(logFile,"debug parse not enabled");
      return Error;
#endif
      break;

    case 'l':		/* help with debugging the tokenizer */
#ifdef DEBUGPARSE
      ss_flex_debug = 1;
      continue;
#else
      logMsg(logFile,"debug token not enabled");
      return Error;
#endif
      break;

    case '*':		/* trace everything */
#ifdef ALLTRACE
      debugParse = True;
      ss_flex_debug = 1;
      debugAssem = True;
#else
      logMsg(logFile,"debugging not enabled\n");
      return Error;
#endif
    }
  }
  return Ok;
}

static retCode displayVersion(char *option,logical enable,void *cl)
{
  return outMsg(logFile,"%s",version);
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

static retCode setLogFile(char *option,logical enable, void *cl)
{
  return initLogfile(option);
}

static retCode setOutput(char *option,logical enable,void *cl)
{
  setOutputFile(option);
  return Ok;
}

Option options[] = {
  { 'd', "debug", True, debugOption, Null, "-d|--debug <flags>"},
  { 'v', "version", False, displayVersion, Null,"-v|--version"},
  { 'P', "parseOnly", False, setParseOnly, Null,"-P|--parseOnly"},
  { 'H', "cafeHome", True, setHome, Null, "-H|--cafeHome <path>"},
  { 'L', "logFile", True, setLogFile, Null, "-L|--logFile <path>"},
  { 'o', "output", True, setOutput, Null, "-o|--output <path>"}};

int getOptions(int argc, char **argv)
{
  splitFirstArg(argc,argv,&argc,&argv);
  return processOptions(argc,argv,options,NumberOf(options));
}

void usage(char *name)
{
  showUsage(name,options,NumberOf(options));
}

