/*
 * Process the command line options
 */
#include "config.h"
#include "engine.h"
#include <stdlib.h>
#include <getopt.h>
#include <memory.h>

#include "options.h"
#include "version.h"      /* Version ID for the Cafe system */

long initHeapSize = 200 * 1024;    /* How much memory to give the heap */
long initStackSize = 1024;      /* How big is the stack */

logical debugging = False;  // instruction tracing option
logical enableVerify = True;  // true if we wish to enable code verification
logical SymbolDebug = False;  // symbolic debugging generation
logical traceVerify = False;  // true if tracing code verification
logical traceMessage = False;  // true if tracing message passing
logical tracePut = False;  // true if tracing term freeze
logical traceLock = False;  /* true if tracing locks */
logical traceResource = False;
logical traceMemory = False;      /* memory tracing */
logical stressMemory = False;      /* stress GC */
logical tracing = False;      /* tracing option */
logical traceCalls = False;      /* top-level function call tracing */
logical interactive = False;      /* interaction instruction tracing */
logical traceCount = False; // Count instructions etc.

char CWD[MAXFILELEN] = "";
char repoDir[MAXFILELEN] = "";
char bootPkg[MAX_SYMB_LEN] = "star.boot";  // boot package
char bootVer[MAX_SYMB_LEN] = "*";
char entry[MAX_SYMB_LEN] = "";  // entry point class
char debugPkg[MAX_SYMB_LEN] = "";  // Standard debug package

static struct {
  codePoint option; // The name of the option
  char value[MAXFILELEN];           /* the value of the option */
} Options[32];  // An array of them

static int optCount = 0;                /* How many do we have? */
static long parseSize(char *text);

static retCode displayVersion(char *option, logical enable, void *cl) {
  return outMsg(logFile, "%s", version);
}

static logical isDigit(char ch) {
  return (logical) (ch >= '0' && ch <= '9');
}

long parseSize(char *text) {
  char *p = text;
  int scale = 1;
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
    *p = '\0';
  }
  return atoi(text) * scale;
}

static void parsePkgOpt(char *opt, char pkg[], long pkgLen, char ver[], long verLen) {
  long colonPos = uniIndexOf(opt, strlen(opt), 0, ':');

  if (colonPos > 0) {
    uniNCpy(pkg, pkgLen, opt, colonPos);
    uniCpy(ver, verLen, &opt[colonPos + 1]);
  } else {
    uniCpy(pkg, pkgLen, opt);
    uniCpy(ver, verLen, "*");
  }
}

int getOptions(int argc, char **argv) {
  int opt;
  extern char *optarg;
  extern int optind;

  splitFirstArg(argc, argv, &argc, &argv);

  for (; optCount < NumberOf(Options) &&
         (opt = getopt(argc, argv, GNU_GETOPT_NOPERMUTE "m:D:d:gG:vVh:s:L:r:b:R:")) >= 0; optCount++) {
    Options[optCount].option = (codePoint) opt;     /* store the option */

    if (optarg != NULL) {
      strncpy(Options[optCount].value, optarg, NumberOf(Options[optCount].value));
    } else
      Options[optCount].value[0] = '\0';

    switch (opt) {
      case 'D': {      /* turn on various debugging options */
        char *c = optarg;

        while (*c) {
          switch (*c++) {
            case 'e':    /* Escape call tracing */
#ifdef EXECTRACE
              traceCalls = True;
              continue;
#else
              logMsg(logFile, "Escape tracing not enabled\n");
              return -1;
#endif

            case 'd':    /* single step instruction tracing */
#ifdef EXECTRACE
              debugging = True;
              continue;
#else
              logMsg(logFile, "Instruction-level debugging not enabled\n");
              return -1;
#endif

            case 'v':    /* turn on verify tracing */
#ifdef VERIFYTRACE
              traceVerify = True;
              continue;
#else
              logMsg(logFile, "code verification not enabled\n");
              return -1;
#endif

            case 'm':    /* trace memory allocations  */
#ifdef MEMTRACE
              if (traceMemory)
                stressMemory = True;
              else
                traceMemory = True;
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
              return -1;
#endif

            case 'p':    /* trace put-style operations */
#ifdef EXECTRACE
              tracePut = True;
              continue;
#else
              logMsg(logFile, "put tracing not enabled");
              return -1;
#endif

            case 'G':    /* Internal symbolic tracing */
#ifdef EXECTRACE
              SymbolDebug = True;
              interactive = False;
              continue;
#else
              logMsg(logFile, "tracing not enabled");
              return -1;
#endif

            case 'g':    /* Internal symbolic debugging */
              SymbolDebug = True;
              interactive = True;
              continue;

            case 'I':
#ifdef STATSTRACE
#ifdef EXECTRACE
              traceCount = True;
              atexit(dumpInsCount);
              break;
#endif
#else
              logMsg(logFile, "instruction counting not enabled");
              return -1;
#endif

            case 'r':     /* Trace resource mgt */
#ifdef RESOURCETRACE
              traceResource = True;
#else
              logMsg(logFile, "Resource tracing not enabled\n");
              return -1;
#endif
            case '*':    /* trace everything */
#ifdef ALLTRACE
              traceCalls = True;
              debugging = True;
              interactive = True;
              traceVerify = True;
              traceCount = True;
              traceMessage = True;
              if (traceMemory)
                stressMemory = True;
              else
                traceMemory = True;
              tracePut = True;              /* term freeze */
              traceResource = True;
#else
            logMsg(logFile,"debugging not enabled\n");
            return -1;
#endif
            default:;
          }
        }
        break;
      }

      case 'g': {
        SymbolDebug = True;  /* turn on symbolic debugging */
        interactive = True;       // Initially its also interactive
        break;
      }

      case 'G': {        /* non-default debugging package */
        strMsg(debugPkg, NumberOf(debugPkg), "%s", optarg);
        break;
      }

      case 'm': {                          /* modify the entry point */
        uniCpy(entry, NumberOf(entry), optarg);
        break;
      }

      case 'r': {
        strMsg(repoDir, NumberOf(repoDir), "%s", optarg);
        break;
      }

      case 'd': {                      /* non-standard initial working directory */
        strMsg(CWD, NumberOf(CWD), "%s", optarg);
        break;
      }

      case 'b': {
        parsePkgOpt(optarg, bootPkg, NumberOf(bootPkg), bootVer, NumberOf(bootVer));
        break;
      }

      case 'R': {                          /* fix the random seed */
        srand((unsigned int) atoi(optarg));
        break;
      }

      case 'L': {
        char fn[MAXFILELEN];
        strncpy(fn, optarg, NumberOf(fn));

        if (initLogfile(fn) != Ok) {
          logMsg(logFile, "log file %s not found", optarg);
          return -1;
        }
        break;
      }

      case 'v':                           /* Display version ID */
        outMsg(logFile, "%s", version);
        outMsg(logFile, "%s", copyRight);
        break;

      case 'V':                      /* Turn on (will be off) code verification */
        enableVerify = (logical) !enableVerify;
        break;

      case 'h':                           /* set up heap size */
        initHeapSize = parseSize(optarg);
        break;

      case 's':                           /* set up initial size of a thread */
        initStackSize = parseSize(optarg);
        break;

      default:
        break;                            /* ignore options we dont understand */
    }
  }
  return optind;
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
