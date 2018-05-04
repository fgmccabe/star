/*
 * main program for the run-time
 */
#include "utils.h"
#include <stdlib.h>
#include <engineP.h>
#include <globals.h>
#include <labelsP.h>
#include <cellP.h>
#include <iochnnlP.h>
#include <arrayP.h>
#include "manifest.h"
#include "clock.h"
#include "args.h"
#include "formioP.h"
#include "arithP.h"
#include "strP.h"
#include "debug.h"

char copyRight[] = "(c) 2010-2018 F.G.McCabe\nApache Licence 2.0";

int main(int argc, char **argv) {
  int narg;

#ifdef HAVE_LOCALECONV
  setlocale(LC_ALL,"");		/* set up locale */
#endif

#ifdef LOCALEDIR
  bindtextdomain(PACKAGE,LOCALEDIR);
  textdomain(PACKAGE);
#endif

  initLogfile("-");

  if ((narg = getOptions(argc, argv)) < 0) {
    exit(1);
  }

  initArith();
  initStr();
  initLbls();
  initGlobals();
  initCell();
  initCode();
  initLocks();
  initTerm();
  initLists();
  initIoChnnl();
  initThr();
  initTime();        /* Initialize time stuff */

  defltCWD();
  defltRepoDir();

  installMsgProc('M', showMtdLbl);
  installMsgProc('L', showLoc);

  /* IMPORTANT -- Keep the order of these set up calls */

  // Set up repository directory
  initHeap(initHeapSize);
  installEscapes();

  loadManifest();

  char errMsg[MAXLINE];

  if (loadPackage(bootPkg, bootVer, errMsg, NumberOf(errMsg), Null) != Ok) {
    logMsg(logFile, "Could not load boot pkg %s/%s: %s", bootPkg, bootVer, errMsg);
    exit(99);
  }

  init_args(argv, argc, narg);    /* Initialize the argument list */

  setupSignals();
  initEngine();

#ifdef TRACEEXEC
  if (traceCount)
    atexit(dumpInsCount);
#endif

#ifdef TRACEMEM
  if (traceMemory)
    atexit(dumpGcStats);
#endif

  switch(bootstrap(entry)){
    case Ok:
      return EXIT_SUCCEED;          /* exit the runtime system cleanly */
    case Error:
      return EXIT_ERROR;
    default:
      return EXIT_FAIL;
  }
}
