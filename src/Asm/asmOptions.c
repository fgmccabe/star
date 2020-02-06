/*
 * Process the command line options
 */
#include "config.h"
#include "asm.h"

#include <cmdOptions.h>
#include "asmOptions.h"
#include "manifestP.h"

logical debugAssem = False;    /* debug the assembling process */
logical parseOnly = False;    /* set to true for parsing only */

static retCode debugOption(char *option, logical enable, void *cl) {
  char *c = option;
  while (*c) {
    switch (*c++) {
      case 'a':      /* trace assembly */
#ifdef TRACEASSM
        debugAssem = True;
#else
      logMsg(logFile,"Assembly insDebugging not enabled\n");
      return Error;
#endif
        continue;

      case 'p':    /* help with insDebugging the parser */
#ifdef DEBUGPARSE
        ssdebug = 1;
#else
      logMsg(logFile,"debug parse not enabled");
      return Error;
#endif
        continue;

      case 'l':    /* help with insDebugging the tokenizer */
#ifdef DEBUGPARSE
        ss_flex_debug = 1;
#else
      logMsg(logFile,"debug token not enabled");
      return Error;
#endif
        continue;

      case 'M':     /* Trace manifest mgt */
#ifdef TRACEMANIFEST
        traceManifest = True;
#else
      logMsg(logFile, "manifest tracing not enabled\n");
      return Error;
#endif
        continue;

      case '*':    /* trace everything */
#ifdef ALLTRACE
        ss_flex_debug = 1;
        ssdebug = 1;
        debugAssem = True;
        traceManifest = True;
#else
      logMsg(logFile,"insDebugging not enabled\n");
      return Error;
#endif
        continue;

      default:;
    }
  }
  return Ok;
}

static retCode displayVersion(char *option, logical enable, void *cl) {
  return outMsg(logFile, "%s", version);
}

static retCode setParseOnly(char *option, logical enable, void *cl) {
  parseOnly = True;
  return Ok;
}

static retCode setHome(char *option, logical enable, void *cl) {
  setStarHome(uniDuplicate(option));
  return Ok;
}

static retCode setLogFile(char *option, logical enable, void *cl) {
  return initLogfile(option);
}

static retCode setRepoDir(char *option, logical enable, void *cl) {
  setManifestPath(option);
  return Ok;
}

static retCode setVersion(char *option, logical enable, void *cl) {
  setPkgVersion(option);
  return Ok;
}

Option options[] = {
  {'d', "debug",       hasArgument, Null,         debugOption,    Null, "-d|--debug <flags>"},
  {'v', "version",     noArgument,  Null,         displayVersion, Null, "-v|--version"},
  {'P', "parseOnly",   noArgument,  Null,         setParseOnly,   Null, "-P|--parseOnly"},
  {'H', "starHome",    hasArgument, STAR_HOME,    setHome,        Null, "-H|--starHome <path>"},
  {'L', "logFile",     hasArgument, STAR_LOGFILE, setLogFile,     Null, "-L|--logFile <path>"},
  {'R', "repository",  hasArgument, STAR_REPO,    setRepoDir,     Null, "-R|--repository <path>"},
  {'V', "pkg-version", hasArgument, Null,         setVersion,     Null, "-V|--set-version <version>"}};

int getAsmOptions(int argc, char **argv) {
  splitFirstArg(argc, argv, &argc, &argv);
  return processOptions(copyright, argc, argv, options, NumberOf(options));
}
