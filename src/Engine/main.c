/*
 * main program for the run-time
 */
#include "utils.h"
#include <stdlib.h>
#include <engineP.h>
#include <globals.h>
#include "constantsP.h"
#include <cellP.h>
#include <iochnnlP.h>
#include <consP.h>
#include "eitherP.h"
#include "ideal.h"
#include <formexts.h>
#include <optionP.h>
#include "manifest.h"
#include "clock.h"
#include "args.h"
#include "closureP.h"
#include "formioP.h"
#include "arithP.h"
#include "bignumP.h"
#include "charP.h"
#include "stringsP.h"
#include "debug.h"
#include "editline.h"
#include "starP.h"
#include "futureP.h"
#include "singleP.h"
#include "vectP.h"
#include "ioops.h"
#include "escapeP.h"

#include "stringBufferP.h"
#include "ltype.h"

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
  installMsgProc('Y', showLS);

  if ((narg = getEngineOptions(argc, argv)) < 0) {
    exit(1);
  }

  initTimers();     /* Initialize time stuff */
  initHistory(/*".star"*/Null);
  initHeap(initHeapSize);
  initLbls();
  initArith();
  initBignum();
  initChars();
  initStrings();
  initSingle();
  initConstants();
  initGlobals();
  initCons();
  initEither();
  initVect();
  initIdeal();
  initOption();
  initFuture();
  initClosure();
  initCell();
  initJit();
  initCode();
  initTerm();
  initStacks();
  initIoChnnl();
  initThr();
  initTime();
  initIoOps();

  char *rootWd = defltCWD();
  defltRepoDir();


  /* IMPORTANT -- Keep the order of these set up calls */

  installEscapes();
  initEngine();

  loadManifest();
  pruneResources("signature");

  char errMsg[MAXLINE];

  {
    timerPo loadTimer = startTimer("load");
    if (loadPackage(&mainPkge, errMsg, NumberOf(errMsg), Null) != Ok) {
      logMsg(logFile, "Problem in loading program %P: %S", &mainPkge, errMsg, uniStrLen(errMsg));
      exit(99);
    }
    pauseTimer(loadTimer);
  }

  init_args(argv, argc, narg);    /* Initialize the argument list */

  setupSignals();

  switch (bootstrap(globalHeap, mainEntry, rootWd)) {
    case Normal:
      return successCode;          /* exit the runtime system cleanly */
    default:
      return failCode;
  }
}
