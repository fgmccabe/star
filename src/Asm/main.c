/*
 * main program for the assembler
 */
#include <ooio.h>
#include "asm.h"
#include "manifest.h"
#include "asmOptions.h"
#include "formexts.h"
#include "formioP.h"
#include "errors.h"
#include <stdlib.h>

char *copyright = "(c) 2010-2018 F.G.McCabe\nAll rights reserved";
char *version = PACKAGE " assembler - " VERSION " - " __DATE__;

int main(int argc, char **argv) {
  int narg;

#ifdef HAVE_LOCALECONV
  setlocale(LC_ALL,"");		/* set up locale */
#endif

#ifdef LOCALEDIR
  bindtextdomain(PACKAGE,LOCALEDIR);
  textdomain(PACKAGE);
#endif

  {
    char fn[] = {'-', 0};
    initLogfile(fn);
  }

  if ((narg = getOptions(argc, argv)) < 0) {
    exit(1);
  }

  initAssem();

  if (loadManifest() == Error) {
    outMsg(logFile, "error in loading repository");
    exit(99);
  }

  installMsgProc('Q', genQuotedStr);

  if (narg < argc) {
    char path[1024];
    uniCpy(path, NumberOf(path), argv[narg]);

    parseContent(path);

    reportErrorCount();
  }
}
