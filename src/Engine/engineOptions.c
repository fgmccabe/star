/*
 * Process the command line options
 */
#include "config.h"
#include "engine.h"
#include <stdlib.h>
#include <getopt.h>

#include "options.h"
#include "version.h"      /* Version ID for the Cafe system */
#include "debug.h"

#include "manifest.h"
#include "engineOptions.h"
#include "heapP.h"

long initHeapSize = 200 * 1024;    /* How much memory to give the heap */
long initStackSize = 1024;      /* How big is the stack */

logical insDebugging = False;  // instruction tracing option
logical tracing = False;      /* tracing option */
logical enableVerify = True;  // true if we wish to enable code verification
// symbolic insDebugging generation
logical traceVerify = False;  // true if tracing code verification
logical traceMessage = False;  // true if tracing message passing
logical traceLock = False;  /* true if tracing locks */
logical traceManifest = False;
logical tracePkg = False;
logical traceMemory = False;      /* memory tracing */
logical stressMemory = False;      /* stress GC */
logical interactive = False;      /* interaction instruction tracing */
logical runStats = False; // Count instructions etc.

char CWD[MAXFILELEN] = "";
char bootPkg[MAX_SYMB_LEN] = "star.boot";  // boot package
char bootVer[MAX_SYMB_LEN] = "*";
char entry[MAX_SYMB_LEN] = "star.boot@__boot";  // entry point class
char debugPkg[MAX_SYMB_LEN] = "";  // Standard debug package

static retCode displayVersion(char *option, logical enable, void *cl) {
  return outMsg(logFile, "%s", version);
}

static logical isDigit(char ch) {
  return (logical) (ch >= '0' && ch <= '9');
}

static integer parseSize(char *text) {
  char *p = text;
  integer scale = 1;
  while (*p != '\0' && isDigit(*p))
    p++;
  if (*p != '\0') {
    switch (*p) {
      case 'k':
      case 'K':
        scale = 1024;
        break;
      case 'm':
      case 'M':
        scale = 1024 * 1024;
        break;
      case 'g':
      case 'G':
        scale = 1024 * 1024 * 1024;
        break;
      default:;
    }
  }
  return parseInteger(text, (integer) (p - text)) * scale;
}

void defltCWD() {
  // set up working directory
  if (uniIsLit(CWD, "")) {
    char cbuff[MAXFILELEN];
    char *cwd = getcwd(cbuff, NumberOf(cbuff)); /* compute current starting directory */
    if (cwd == NULL)
      syserr("cant determine current directory");
    else
      strMsg(CWD, NumberOf(CWD), "%s/", cwd);
  }
}

static retCode debugOption(char *option, logical enable, void *cl) {
  char *c = option;

  while (*c) {
    switch (*c++) {
      case 'd':    /* single step instruction tracing */
#ifdef TRACEEXEC
        insDebugging = True;
        tracing = True;
        continue;
#else
      logMsg(logFile, "Instruction-level insDebugging not enabled\n");
      return Error;
#endif

      case 'v':    /* turn on verify tracing */
#ifdef TRACEVERIFY
        traceVerify = True;
        continue;
#else
      logMsg(logFile, "code verification not enabled\n");
      return Error;
#endif

      case 'm':    /* trace memory allocations  */
#ifdef TRACEMEM
        if (traceMemory)
          stressMemory = True;
        else
          traceMemory = True;

        atexit(dumpGcStats);
        continue;
#else
      logMsg(logFile,"memory tracing not enabled");
            return -1;
#endif

      case 'l':    /* trace synch locks */
#ifdef LOCKTRACE
        traceLock = True;
        continue;
#else
      logMsg(logFile, "sync tracing not enabled");
      return Error;
#endif

      case 'G':    /* Internal symbolic tracing */
#ifdef TRACEEXEC
        lineDebugging = True;
        interactive = False;
        continue;
#else
      logMsg(logFile, "tracing not enabled");
      return Error;
#endif

      case 'g':    /* Internal symbolic debugging */
        lineDebugging = True;
        interactive = True;
        continue;

      case 'I':
#ifdef TRACESTATS
#ifdef TRACEEXEC
        runStats = True;
        atexit(dumpInsCount);
        break;
#endif
#else
      logMsg(logFile, "instruction counting not enabled");
      return Error;
#endif

      case 'M':     /* Trace manifest mgt */
#ifdef TRACEMANIFEST
        traceManifest = True;
#else
      logMsg(logFile, "Resource tracing not enabled\n");
      return Error;
#endif

      case 'P':    /* trace package operations  */
#ifdef TRACEPKG
        tracePkg = True;
        continue;
#else
      logMsg(logFile,"package tracing not enabled");
            return -1;
#endif

      case '*':    /* trace everything */
#ifdef ALLTRACE
        traceVerify = True;
        runStats = True;
        atexit(dumpInsCount);
        traceMessage = True;
        if (traceMemory)
          stressMemory = True;
        else
          traceMemory = True;
        atexit(dumpGcStats);
        tracePkg = True;
        continue;
#else
      logMsg(logFile,"insDebugging not enabled\n");
            return Error;
#endif
      default:;
    }
  }

  return Ok;
}

static retCode setLogFile(char *option, logical enable, void *cl) {
  return initLogfile(option);
}

static retCode setRepoDir(char *option, logical enable, void *cl) {
  setManifestPath(option);
  return Ok;
}

static retCode symbolDebug(char *option, logical enable, void *cl) {
  lineDebugging = True;  /* turn on symbolic insDebugging */
  interactive = True;       // Initially its also interactive
  return Ok;
}

static retCode setDebugger(char *option, logical enable, void *cl) {
  uniCpy(debugPkg, NumberOf(debugPkg), option);
  return Ok;
}

static retCode setMainEntry(char *option, logical enable, void *cl) {
  uniCpy(entry, NumberOf(entry), option);
  return Ok;
}

static retCode setWD(char *option, logical enable, void *cl) {
  uniCpy(CWD, NumberOf(CWD), option);
  return Ok;
}

static retCode setBootPkg(char *option, logical enable, void *cl) {
  uniCpy(bootPkg, NumberOf(bootPkg), option);
  return Ok;
}

static retCode setVerify(char *option, logical enable, void *cl) {
  enableVerify = (logical) !enableVerify;
  return Ok;
}

static retCode setHeapSize(char *option, logical enable, void *cl) {
  initHeapSize = parseSize(option);
  if (initHeapSize == 0)
    return Error;
  return Ok;
}

static retCode setStackSize(char *option, logical enable, void *cl) {
  initStackSize = parseSize(optarg);
  return Ok;
}

Option options[] = {
  {'d', "debug",        True,  debugOption,    Null, "-d|--debug <flags>"},
  {'g', "symbol-debug", False, symbolDebug,    Null, "-g|--symbol-debug"},
  {'G', "debugger",     True,  setDebugger,    Null, "-G|--debugger"},
  {'v', "version",      False, displayVersion, Null, "-v|--version"},
  {'b', "boot-pkg",     True,  setBootPkg,     Null, "-b|--boot-pkg <pkg>"},
  {'m', "main",         True,  setMainEntry,   Null, "-m|--main"},
  {'L', "logFile",      True,  setLogFile,     Null, "-L|--logFile <path>"},
  {'R', "repository",   True,  setRepoDir,     Null, "-R|--repository <path>"},
  {'W', "set-wd",       True,  setWD,          Null, "-W|--set-wd <dir>"},
  {'V', "verify",       False, setVerify,      Null, "-V|--verify"},
  {'h', "heap",         True,  setHeapSize,    Null, "-h|--heap <size>"},
  {'s', "stack",        True,  setStackSize,   Null, "-s|--stack <size>"},};

logical lineDebugging = False;

static void showStar(ioPo out) {
  outMsg(out, "star - %s", copyRight);
}

int getOptions(int argc, char **argv) {
  splitFirstArg(argc, argv, &argc, &argv);
  return processOptions(copyRight, argc, argv, options, NumberOf(options));
}
