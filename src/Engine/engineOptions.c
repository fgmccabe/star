/*
 * Process the command line options
 */
#include "config.h"
#include "engine.h"
#include <stdlib.h>
#include <getopt.h>
#include <unistd.h>

#include "cmdOptions.h"
#include "version.h"      /* Version ID for the Star system */
#include "debug.h"

#include "manifest.h"
#include "engineOptions.h"
#include "heapP.h"

long initHeapSize = 200 * 1024;   /* How much memory to give the heap */
long maxHeapSize = 1024 * 1024 * 1024; // Maximum heap size 1G cells
long initStackSize = 1024;        /* How big is the stack */
long maxStackSize = 100 * 1024;     /* 100K cells is max stack size */

logical insDebugging = False;     // instruction tracing option
logical lineDebugging = False;
logical debugDebugging = False;
logical tracing = False;          /* tracing option */
int debuggerPort = 0;                // Debug port to establish listener on
logical showPkgFile = False;      // True if we show file names instead of package names
logical showColors = True;        // True if we want to show colored output

logical traceVerify = False;      // true if tracing code verification
logical traceMessage = False;     // true if tracing message passing
logical traceLock = False;        /* true if tracing locks */
logical traceManifest = False;
logical tracePkg = False;
logical traceMemory = False;      /* memory tracing */
logical validateMemory = False;   // Validate heap after every allocation
logical interactive = False;      /* interaction instruction tracing */
logical runStats = False;         // Count instructions etc.

char CWD[MAXFILELEN] = "";
char bootVer[MAX_SYMB_LEN] = "*";
char bootInit[MAX_SYMB_LEN] = "star.boot@init";

PackageRec bootPkge = {.packageName="star.boot", .version="*"};

char bootEntry[MAX_SYMB_LEN] = "star.boot#__boot";  // entry point

static retCode displayVersion(char *option, logical enable, void *cl) {
  return outMsg(logFile, "star - %s", version);
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

char * defltCWD() {
  // set up working directory
  if (uniIsLit(CWD, "")) {
    char cbuff[MAXFILELEN];
    char *cwd = getcwd(cbuff, NumberOf(cbuff)); /* compute current starting directory */
    if (cwd == NULL)
      syserr("cant determine current directory");
    else
      uniTrim(cwd, uniStrLen(cwd), "", "/", CWD, NumberOf(CWD));
  }
  return CWD;
}

static retCode debugOption(char *option, logical enable, void *cl) {
  char *c = option;

  while (*c) {
    switch (*c++) {
      case 'd':    /* single step instruction tracing */
#ifdef TRACEEXEC
        insDebugging = True;
        tracing = True;
        interactive = True;
        continue;
#else
      logMsg(logFile, "Instruction-level debugging not enabled\n");
      return Error;
#endif

      case 'D':    /*  instruction tracing */
#ifdef TRACEEXEC
        insDebugging = True;
        tracing = True;
        interactive = False;
        continue;
#else
      logMsg(logFile, "Instruction-level tracing not enabled\n");
      return Error;
#endif

      case 'u':    /*  debug the debugger */
#ifdef TRACE_DBG
        debugDebugging = True;
        continue;
#else
      logMsg(logFile, "Debugging tracing not enabled\n");
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
        traceMemory = True;

        atexit(dumpGcStats);
        continue;
#else
      logMsg(logFile,"memory tracing not enabled");
            return -1;
#endif

      case 'H':    /* validate heap after allocations  */
#ifdef TRACEMEM
        validateMemory = True;
        continue;
#else
      logMsg(logFile,"memory validation not enabled");
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
        tracing = True;
        continue;
#else
      logMsg(logFile, "tracing not enabled");
      return Error;
#endif

      case 'g':    /* Internal symbolic debugging */
        lineDebugging = True;
        interactive = True;
        tracing = True;
        continue;

      case 's':
#ifdef TRACESTATS
        if (runStats) {
          atexit(dumpInsStats);
        } else {
          atexit(dumpInsCount);
        }
        runStats = True;
        break;
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
      case 'F':   // Show file name instead of package
        showPkgFile = True;
        continue;

      case 'C': // Toggle showing colors
        showColors = !showColors;
        continue;

      default:;
    }
  }

  return Ok;
}

static retCode debugOptHelp(ioPo out, char opt, char *usage, void *cl) {
  return outMsg(out, "    -d|--debug <"
                     #ifdef TRACEEXEC
                     "d|D|"
                     #endif
                     #ifdef TRACEVERIFY
                     "v|"
                     #endif
                     #ifdef TRACEMEM
                     "m|H|"
                     #endif
                     #ifdef LOCKTRACE
                     "l|"
                     #endif
                     #ifdef TRACEEXEC
                     "G|g|"
                     #endif
                     #ifdef TRACESTATS
                     "s|"
                     #endif
                     #ifdef TRACEMANIFEST
                     "M|"
                     #endif
                     #ifdef TRACEPKG
                     "P"
                     #endif
                     "F"
                     ">\n%_");
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
  displayDepth = 10;    // Set up larger display depth
  return Ok;
}

static retCode setDebuggerPort(char *option, logical enable, void *cl) {
  debuggerPort = parseInteger(option, uniStrLen(option));
  if (debuggerPort <= 0)
    return Error;
  else {
    lineDebugging = True;   /* set up for remote symbolic insDebugging */
    interactive = True;
    showPkgFile = True;
    showColors = False;
  }
  return Ok;
}

static retCode setMainEntry(char *option, logical enable, void *cl) {
  uniCpy(bootEntry, NumberOf(bootEntry), option);
  return Ok;
}

static retCode setWD(char *option, logical enable, void *cl) {
  uniCpy(CWD, NumberOf(CWD), option);
  return Ok;
}

static retCode setBootPkg(char *option, logical enable, void *cl) {
  uniCpy(&bootPkge.packageName[0], NumberOf(bootPkge.packageName), option);
  strMsg(bootInit, NumberOf(bootInit), "%s@init", option);
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

static retCode setMaxHeapSize(char *option, logical enable, void *cl) {
  maxHeapSize = parseSize(option);
  if (maxHeapSize == 0)
    return Error;
  return Ok;
}

static retCode setDisplayDepth(char *option, logical enable, void *cl) {
  displayDepth = parseSize(option);
  return Ok;
}

static retCode setStackSize(char *option, logical enable, void *cl) {
  initStackSize = parseSize(option);
  return Ok;
}

static retCode setMaxStackSize(char *option, logical enable, void *cl) {
  maxStackSize = parseSize(option);
  return Ok;
}

Option options[] = {
  {'d', "debug",         hasArgument, STAR_DBG_OPTS,      debugOption,     Null, "-d|--debug <flags>", debugOptHelp},
  {'p', "depth",         hasArgument, STAR_DBG_OPTS,      setDisplayDepth, Null, "-p|--depth <depth>"},
  {'g', "symbol-debug",  noArgument,  SYMBOL_DEBUG,       symbolDebug,     Null, "-g|--symbol-debug"},
  {'G', "debugger-port", hasArgument, STAR_DEBUGGER_PORT, setDebuggerPort, Null, "-G|--debugger-port"},
  {'v', "version",       noArgument,  Null,               displayVersion,  Null, "-v|--version"},
  {'b', "boot-pkg",      hasArgument, STAR_BOOT,          setBootPkg,      Null, "-b|--boot-pkg <pkg>"},
  {'m', "main",          hasArgument, STAR_MAIN,          setMainEntry,    Null, "-m|--main <entry>"},
  {'L', "logFile",       hasArgument, STAR_LOGFILE,       setLogFile,      Null, "-L|--logFile <path>"},
  {'r', "repository",    hasArgument, STAR_REPO,          setRepoDir,      Null, "-r|--repository <path>"},
  {'w', "set-wd",        hasArgument, STAR_WD,            setWD,           Null, "-w|--set-wd <dir>"},
  {'V', "verify",        noArgument,  STAR_VERIFY,        setVerify,       Null, "-V|--verify code"},
  {'h', "heap",          hasArgument, STAR_INIT_HEAP,     setHeapSize,     Null, "-h|--heap <size>"},
  {'H', "max-heap",      hasArgument, STAR_MAX_HEAP,      setMaxHeapSize,  Null, "-H|--max-heap <size>"},
  {'s', "stack",         hasArgument, STAR_INIT_STACK,    setStackSize,    Null, "-s|--stack <size>"},
  {'S', "max-stack",     hasArgument, STAR_MAX_STACK,     setMaxStackSize, Null, "-S|--max-stack <size>"},};

int getStarOptions(int argc, char **argv) {
  splitFirstArg(argc, argv, &argc, &argv);
  return processOptions(copyright, argc, argv, options, NumberOf(options));
}

