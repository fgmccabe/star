/*
 * main program for the run-time
 */
#include "utils.h"
#include <stdlib.h>
#include "../Infra/Headers/manifest.h"
#include "clock.h"
#include "args.h"
#include "engine.h"
#include "formioP.h"
#include "codeP.h"

char copyRight[] = "(c) 2010-2018 F.G.McCabe\nAll rights reserved";

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

  strMsg(entry, NumberOf(entry), "star.boot@__boot");

  defltRepoDir();
  defltCWD();

  installMsgProc('M', showMtdLbl);
  initCode();

  if ((narg = getOptions(argc, argv)) < 0) {
    usage(argv[0]);
    exit(1);
  }

  /* IMPORTANT -- Keep the order of these set up calls */

  // Set up repository directory
  initHeap(initHeapSize);

  loadDefltManifest();

  initClass();        /* Initialize the class handlers */
  initPrograms();      /* Initialize program handling */
  initDict();        /* Start up the dictionaries */
  install_escapes();      /* Initialize the escape table */
  initFiles();        /* initialize file tables */
  init_args(argv, argc, narg);    /* Initialize the argument list */
  init_time();        /* Initialize time stuff */
  setupSignals();

  initEngine();

#ifdef EXECTRACE
  if (traceCount)
    atexit(dumpInsCount);
#endif

  bootstrap(entry, bootPkg, bootVer);

  return EXIT_SUCCEED;          /* exit the lo system cleanly */
}
    
    
