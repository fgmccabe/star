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
#include <memoP.h>
#include <consP.h>
#include <formexts.h>
#include "capabilityP.h"
#include "manifest.h"
#include "clock.h"
#include "args.h"
#include "formioP.h"
#include "arithP.h"
#include "strP.h"
#include "debug.h"
#include "editline.h"
#include "jit.h"

#include "stringBufferP.h"

char *copyright = "(c) 2010-2020 F.G.McCabe\nApache Licence 2.0";

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
  }
  initHistory(".star");
  initHeap(initHeapSize);
  initJit();
  initArith();
  initStr();
  initLbls();
  initGlobals();
  initCons();
  initCell();
  initMemo();
  initCode();
  initLocks();
  initTerm();
  initCapability();
  initIoChnnl();
  initThr();
  initTime();        /* Initialize time stuff */

  char *rootWd = defltCWD();
  defltRepoDir();

  setupDebugChannels();

  installMsgProc('M', showMtdLbl);
  installMsgProc('L', showLoc);
  installMsgProc('T', showTerm);
  installMsgProc('P', dispPkgNm);
  installMsgProc('B', showStringBuffer);
  installMsgProc('A', showLabel);
  installMsgProc('Q', genQuotedStr);

  /* IMPORTANT -- Keep the order of these set up calls */

  // Set up repository directory
  installEscapes();
  initEngine();

  loadManifest();
  pruneResources("signature");

  char errMsg[MAXLINE];

  if (loadPackage(&bootPkge, errMsg, NumberOf(errMsg), Null) != Ok) {
    logMsg(logFile, "Could not load boot pkg %s: %s", pkgName(&bootPkge), errMsg);
    exit(99);
  }

  init_args(argv, argc, narg);    /* Initialize the argument list */

  setupSignals();

  switch (bootstrap(bootEntry, rootWd)) {
    case Ok:
      return EXIT_SUCCEED;          /* exit the runtime system cleanly */
    case Error:
      return EXIT_ERROR;
    default:
      return EXIT_FAIL;
  }
}
