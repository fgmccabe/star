/*
 * main program for the run-time
 */
#include "utils.h"
#include <stdlib.h>
#include <engineP.h>
#include <globals.h>
#include <cellP.h>
#include <iochnnlP.h>
#include <consP.h>
#include "ideal.h"
#include <formexts.h>
#include <optionP.h>
#include "capabilityP.h"
#include "manifest.h"
#include "clock.h"
#include "args.h"
#include "closureP.h"
#include "formioP.h"
#include "arithP.h"
#include "charP.h"
#include "stringsP.h"
#include "debug.h"
#include "editline.h"
#include "starP.h"
#include "futureP.h"

#include "stringBufferP.h"
#include "continuationP.h"

char *copyright = "(c) 2010 and beyond F.G.McCabe\nApache Licence 2.0";

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

  if ((narg = getEngineOptions(argc, argv)) < 0) {
    exit(1);
  }       /* Initialize time stuff */
  initTimers();
  initHistory(/*".star"*/Null);
  initHeap(initHeapSize);
  initStacks();
  initContinuations();
  initArith();
  initChars();
  initStrings();
  initLbls();
  initGlobals();
  initCons();
  initIdeal();
  initOption();
  initFuture();
  initClosure();
  initCell();
  initCode();
  initTerm();
  initCapability();
  initIoChnnl();
  initThr();
  initTime();

  char *rootWd = defltCWD();
  defltRepoDir();

  setupDebugChannels();

  installMsgProc('M', showMtdLbl);
  installMsgProc('L', showLoc);
  installMsgProc('T', showTerm);
  installMsgProc('P', dispPkgNm);
  installMsgProc('B', showStringBuffer);
  installMsgProc('A', showLabel);
  installMsgProc('I', showIdentifier);
  installMsgProc('C', genQuotedChr);
  installMsgProc('Q', genQuotedStr);

  /* IMPORTANT -- Keep the order of these set up calls */

  installEscapes();
  initEngine();

  loadManifest();
  pruneResources("signature");

  char errMsg[MAXLINE];

  {
    timerPo loadTimer = startTimer("load");
    if (loadPackage(&mainPkge, errMsg, NumberOf(errMsg), Null) != Ok) {
      logMsg(logFile, "Could not load boot pkg %s: %s", pkgName(&mainPkge), errMsg);
      exit(99);
    }
    pauseTimer(loadTimer);
  }

  init_args(argv, argc, narg);    /* Initialize the argument list */

  setupSignals();
  atexit(dumpStack);

  capabilityPo rootCap = allocateCapability(globalHeap, rootWd, uniStrLen(rootWd), stdPerms);

  switch (bootstrap(globalHeap, mainEntry, rootWd, rootCap)) {
    case Ok:
      return EXIT_SUCCEED;          /* exit the runtime system cleanly */
    case Error:
      return EXIT_ERROR;
    default:
      return EXIT_FAIL;
  }
}
