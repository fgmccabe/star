/*
 * Process the command line options
 */
#include "config.h"
#include "asm.h"

#include <cmdOptions.h>
#include "asmOptions.h"
#include "manifest.h"

logical debugAssem = False;    /* debug the assembling process */
logical parseOnly = False;    /* set to true for parsing only */
logical traceManifest = False;

static retCode debugOption(char *option, logical enable, void *cl) {
  char *c = option;
  while (*c) {
    switch (*c++) {
      case 'a':      /* trace assembly */
#ifdef TRACEASSM
        debugAssem = True;
        continue;
#else
      logMsg(logFile,"Assembly insDebugging not enabled\n");
      return Error;
#endif

      case 'p':    /* help with insDebugging the parser */
#ifdef DEBUGPARSE
        ss_flex_debug = 1;
        ssdebug = 1;
        continue;
#else
      logMsg(logFile,"debug parse not enabled");
      return Error;
#endif
        break;

      case 'l':    /* help with insDebugging the tokenizer */
#ifdef DEBUGPARSE
        ss_flex_debug = 1;
        continue;
#else
      logMsg(logFile,"debug token not enabled");
      return Error;
#endif
        break;

      case '*':    /* trace everything */
#ifdef ALLTRACE
        ss_flex_debug = 1;
        ssdebug = 1;
        debugAssem = True;
#else
      logMsg(logFile,"insDebugging not enabled\n");
      return Error;
#endif
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
  setCafeHome(uniDuplicate(option));
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
  {'d', "debug",       True,  Null,        debugOption,    Null, "-d|--debug <flags>"},
  {'v', "version",     False, Null,        displayVersion, Null, "-v|--version"},
  {'P', "parseOnly",   False, Null,        setParseOnly,   Null, "-P|--parseOnly"},
  {'H', "starHome",    True, STAR_HOME,    setHome,        Null, "-H|--starHome <path>"},
  {'L', "logFile",     True, STAR_LOGFILE, setLogFile,     Null, "-L|--logFile <path>"},
  {'R', "repository",  True, STAR_REPO,    setRepoDir,     Null, "-R|--repository <path>"},
  {'V', "pkg-version", True,  Null,        setVersion,     Null, "-V|--set-version <version>"}};

int getOptions(int argc, char **argv) {
  splitFirstArg(argc, argv, &argc, &argv);
  return processOptions(copyright, argc, argv, options, NumberOf(options));
}
