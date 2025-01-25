/*
 * Process the command line options
 */
#include "config.h"
#include <stdlib.h>
#include <unistd.h>
#include <jit.h>

#include "version.h"      /* Version ID for the Star system */

#include "cmdOptions.h"
#include "streamDecodeP.h"
#include "heapP.h"
#include "stackP.h"
#include "manifestP.h"
#include "verifyP.h"
#include "debugP.h"
#include "engineP.h"
#include "buddyP.h"

char CWD[MAXFILELEN] = "";
char rootCap[MAXFILELEN] = "/";

PackageRec mainPkge = {.packageName="star.bboot", .version="*"};
char mainEntry[MAX_SYMB_LEN] = "star.bboot#__boot";  // entry point
static logical bootSet = False;

static retCode displayVersion(char *option, logical enable) {
  return outMsg(logFile, "star - %s", version);
}

char *defltCWD() {
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

static retCode debugOption(char *option, logical enable) {
  char *c = option;

  while (*c) {
    switch (*c++) {
      case 'd':    /* single step instruction tracing */
        insDebugging = True;
        tracing = True;
        interactive = True;
        continue;

      case 'D':    /*  instruction tracing */
        insDebugging = True;
        tracing = True;
        interactive = False;
        continue;

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
        if (traceVerify < detailedTracing)
          traceVerify++;
        logMsg(logFile, "Verification tracing enabled");
        continue;
#else
        logMsg(logFile, "code verification not enabled\n");
        return Error;
#endif

      case 'a':    /* trace memory allocations  */
#ifdef TRACEMEM
        traceAllocs = True;

        logMsg(logFile, "Memory allocation tracing enabled");
        continue;
#else
        logMsg(logFile,"memory tracing not enabled");
              return -1;
#endif

      case 'm': {    /* trace memory activity  */
#ifdef TRACEMEM
        if (traceMemory < detailedTracing)
          traceMemory++;
        logMsg(logFile, "GC tracing enabled");
        continue;
#else
        logMsg(logFile,"memory tracing not enabled");
  return -1;
#endif
      }

      case 'H':    /* validate heap after allocations  */
#ifdef TRACEMEM
        validateMemory = True;
        logMsg(logFile, "Heap validation enabled");
        continue;
#else
        logMsg(logFile,"memory validation not enabled");
        return -1;
#endif

      case 'S':    /* trace stack operations  */
#ifdef TRACESTACK
        if (traceStack < detailedTracing)
          traceStack++;

        stackVerify = True;
        logMsg(logFile, "Stack tracing (level %d) enabled", traceStack);
        continue;
#else
        logMsg(logFile,"stack operation tracing not enabled");
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
        lineDebugging = True;
        interactive = False;
        tracing = True;
        logMsg(logFile, "Symbolic tracing enabled");
        continue;

      case 'g':    /* Internal symbolic debugging */
        lineDebugging = True;
        interactive = True;
        tracing = True;
        continue;

      case 'j':
#ifdef TRACEJIT
        traceJit = True;
        continue;
#else
        logMsg(logFile, "jit tracing not enabled");
        return Error;
#endif

      case 's':
        collectStats = True;
        atexit(dumpStats);
        logMsg(logFile, "Statistics collection enabled");
        break;

      case 'M':     /* Trace manifest mgt */
#ifdef TRACEMANIFEST
        if (traceManifest < detailedTracing)
          traceManifest++;
        logMsg(logFile, "Manifest tracing enabled");
#else
        logMsg(logFile, "Resource tracing not enabled\n");
        return Error;
#endif
        continue;

      case 'P':    /* trace package operations  */
#ifdef TRACEPKG
        if (tracePkg < detailedTracing)
          tracePkg++;
        logMsg(logFile, "Package tracing enabled\n");
        continue;
#else
        logMsg(logFile,"package tracing not enabled");
        return -1;
#endif

      case 'Q':    /* trace decoding operations  */
#ifdef TRACEDECODE
        if (traceDecode < detailedTracing)
          traceDecode++;
        logMsg(logFile, "Decoding tracing enabled\n");
        continue;
#else
        logMsg(logFile,"decode tracing not enabled");
        return -1;
#endif

      case 'B':    /* trace stack memory allocations operations  */
#ifdef TRACE_BUDDY_MEMORY
        traceBuddyMemory = True;
        logMsg(logFile, "Buddy memory allocator tracing enabled\n");
        continue;
#else
        logMsg(logFile,"stack memory tracing not enabled");
        return -1;
#endif

      case 'F':   // Show file name instead of package
        showPkgFile = True;
        logMsg(logFile, "Show file names enabled\n");
        continue;

      case 'C': // Toggle showing colors
        showColors = !showColors;
        logMsg(logFile, "Show colors %s\n", (showColors ? "enabled" : "disabled"));
        continue;

      default:;
    }
  }

  return Ok;
}

static retCode debugOptHelp(ioPo out, char opt, char *usage) {
  return outMsg(out, "    -d|--debug <"
                     "d|D|"
                     #ifdef TRACEVERIFY
                     "v|"
                     #endif
                     #ifdef TRACEMEM
                     "m|H|"
                     #endif
                     #ifdef LOCKTRACE
                     "l|"
                     #endif
                     "G|g|"
                     #ifdef TRACESTACK
                     "O|"
                     #endif
                     #ifdef TRACEJIT
                     "j|"
                     #endif
                     #ifdef TRACESTATS
                     "s|"
                     #endif
                     #ifdef TRACEMANIFEST
                     "M|"
                     #endif
                     #ifdef TRACECAPABILITY
                     "C|"
                     #endif
                     #ifdef TRACEDECODE
                     "Q|"
                     #endif
                     #ifdef TRACEPKG
                     "P|"
                     #endif
                     #ifdef TRACE_BUDDY_MEMORY
                     "B|"
                     #endif
                     "F"
                     ">\n%_");
}

static retCode setLogFile(char *option, logical enable) {
  return initLogfile(option);
}

static retCode setRepoDir(char *option, logical enable) {
  setManifestPath(option);
  return Ok;
}

static retCode symbolDebug(char *option, logical enable) {
  lineDebugging = True;  /* turn on symbolic insDebugging */
  interactive = True;       // Initially its also interactive
  return Ok;
}

static retCode setDebuggerPort(char *option, logical enable) {
  debuggerPort = parseInt(option, uniStrLen(option));
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

static retCode setBootEntry(char *option, logical enable) {
  uniCpy(mainEntry, NumberOf(mainEntry), option);
  return Ok;
}

static retCode setWD(char *option, logical enable) {
  uniCpy(CWD, NumberOf(CWD), option);
  return Ok;
}

static retCode setRootCapability(char *option, logical enable) {
  uniCpy(rootCap, NumberOf(rootCap), option);
  return Ok;
}

static retCode setPkgMain(char *option, logical enable) {
  tryRet(parsePkg(option, uniStrLen(option), &mainPkge));
  strMsg(mainEntry, NumberOf(mainEntry), "%s@_main", mainPkge.packageName);
  bootSet = True;
  return Ok;
}

static retCode setJitThreshold(char *option, logical enable) {
  jitThreshold = parseInt(option, uniStrLen(option));
  if (jitThreshold == 0)
    jitOnLoad = True;
  return Ok;
}

static retCode setVerify(char *option, logical enable) {
  enableVerify = (logical) !enableVerify;
  return Ok;
}

static retCode setHeapSize(char *option, logical enable) {
  initHeapSize = parseSize(option);
  if (initHeapSize == 0)
    return Error;
  return Ok;
}

static retCode setMaxHeapSize(char *option, logical enable) {
  maxHeapSize = parseSize(option);
  if (maxHeapSize == 0)
    return Error;
  return Ok;
}

static retCode setMaxLabels(char *option, logical enable) {
  maxLabels = parseInt(option, uniStrLen(option));
  if (maxLabels == 0)
    return Error;
  return Ok;
}

static retCode setDisplayDepth(char *option, logical enable) {
  displayDepth = parseSize(option);
  return Ok;
}

static retCode setDefaultSize(char *option, logical enable) {
  integer size = parseSize(option);
  if (size < minStackSize) {
    outMsg(logFile, "default size should be at least %d\n", minStackSize);
    return Error;
  } else if (size > stackRegionSize / 2) {
    outMsg(logFile, "size should no larger than %d\n", stackRegionSize / 2);
    return Error;
  } else if (size != (1 << lg2(size))) {
    outMsg(logFile, "size should be a power of 2, suggesting %d\n", 1 << (lg2(size) + 1));
    size = 1 << (lg2(size) + 1);
  }

  defaultStackSize = size;

  return Ok;
}

static retCode setMinStackSize(char *option, logical enable) {
  minStackSize = parseSize(option);
  if (minStackSize < MINMINSTACKSIZE) {
    outMsg(logFile, "minimum stack size should be at least %d\n", MINMINSTACKSIZE);
    minStackSize = MINMINSTACKSIZE;
  } else if (minStackSize != (1 << lg2(minStackSize))) {
    outMsg(logFile, "minimum stack size should be a power of 2, suggesting %d\n", 1 << (lg2(minStackSize) + 1));
    minStackSize = 1 << (lg2(minStackSize) + 1);
  }

  return Ok;
}

static retCode setStackRegionSize(char *option, logical enable) {
  stackRegionSize = parseSize(option);
  if (stackRegionSize != (1 << lg2(stackRegionSize))) {
    outMsg(logFile, "maximum stack region size should be a power of 2, suggesting %d", 1 << lg2(stackRegionSize));
    stackRegionSize = 1 << lg2(stackRegionSize);
  }
  return Ok;
}

static retCode setEnableTimers(char *option, logical enable) {
  enableTimers = enable;
  atexit(reportTimers);

  return Ok;
}

Option options[] = {
  {'d', "debug",         hasArgument, STAR_DBG_OPTS,      debugOption,        "-d|--debug <flags>", debugOptHelp},
  {'p', "print-depth",   hasArgument, STAR_DBG_OPTS,      setDisplayDepth,    "-p|--print-depth <depth>"},
  {'g', "symbol-debug",  noArgument,  SYMBOL_DEBUG,       symbolDebug,        "-g|--symbol-debug"},
  {'G', "debugger-port", hasArgument, STAR_DEBUGGER_PORT, setDebuggerPort,    "-G|--debugger-port"},
  {'v', "version",       noArgument,  Null,               displayVersion,     "-v|--version"},
  {'b', "main-pkg",      hasArgument, STAR_BOOT,          setPkgMain,         "-b|--main-pkg <pkg>"},
  {'j', "threshold",     hasArgument, STAR_JIT_THRESHOLD, setJitThreshold,    "-j|--threshold <count>"},
  {'m', "main",          hasArgument, STAR_MAIN,          setBootEntry,       "-m|--main <entry>"},
  {'L', "logFile",       hasArgument, STAR_LOGFILE,       setLogFile,         "-L|--logFile <path>"},
  {'r', "repository",    hasArgument, STAR_REPO,          setRepoDir,         "-r|--repository <path>"},
  {'w', "set-wd",        hasArgument, STAR_WD,            setWD,              "-w|--set-wd <dir>"},
  {'W', "set-root-cap",  hasArgument, STAR_ROOT_WD,       setRootCapability,  "-W|--set-root-cap <dir>"},
  {'V', "verify",        noArgument,  STAR_VERIFY,        setVerify,          "-V|--toggle code verify"},
  {'h', "heap",          hasArgument, STAR_INIT_HEAP,     setHeapSize,        "-h|--heap <size>"},
  {'H', "max-heap",      hasArgument, STAR_MAX_HEAP,      setMaxHeapSize,     "-H|--max-heap <size>"},
  {'s', "min-stack",     hasArgument, STAR_MIN_STACK,     setMinStackSize,    "-s|--min-stack <size>"},
  {'S', "dflt-stack",    hasArgument, STAR_DFLT_STACK,    setDefaultSize,     "-S|--default-stack <size>"},
  {'R', "stack-region",  hasArgument, STAR_STACK_REGION,  setStackRegionSize, "-R|--stack-region <size>"},
  {'l', "max-labels",    hasArgument, STAR_MAX_LABELS,    setMaxLabels,       "-l|--max-labels <size>"},
  {'t', "enable-timers", noArgument,  Null,               setEnableTimers,    "-t|--enable-timers"},
};

int getEngineOptions(int argc, char **argv) {
  splitFirstArg(argc, argv, &argc, &argv);
  int narg = processOptions(copyright, argc, argv, options, NumberOf(options));

  if (narg > 0 && narg < argc && !bootSet) {
    setPkgMain(argv[narg], True);
    return narg + 1;
  } else
    return narg;
}

