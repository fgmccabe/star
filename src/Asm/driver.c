/*
 * main driver to parse and assemble a file
 */
#include "config.h"
#include "asm.h"
#include "errors.h"
#include "assemP.h"
#include "manifest.h"

#include <stdlib.h>

extern int ssparse(ioPo file, pkPo *pkg);

static void initStdUri();

retCode parseContent(char *path) {
  initStdUri();
  ioPo file = openInFile(path, utf8Encoding);

  if (file != Null) {
    pkPo pkg = Null;
    ssparse(file, &pkg);

    if (debugAssem)
      dumpPkgCode(pkg);

    closeFile(file);      /* close the source string file */
    if (isErrorFree()) {
      char buff[MAXFILELEN], outFn[MAXFILELEN];
      char *codeName = manifestOutPath(&pkg->pkg, "co", buff, NumberOf(buff));
      char *outPath = repoRsrcPath(codeName, outFn, NumberOf(outFn));
      ioPo out = openOutFile(outPath, rawEncoding);
      retCode ret = encodePkg(out, pkg);

      closeFile(out);

      if (ret == Ok) {
        ret = addToManifest(&pkg->pkg, "code", codeName);

        if (ret == Ok)
          ret = flushManifest();
      }

      return ret;
    } else {
      outMsg(logFile, "output not written\n");
      return Fail;
    }
  } else
    return Fail;
}

static char *starHome = NULL;

void setStarHome(char *home) {
  starHome = home;
}

static char *PKG_VERSION = "*";

void setPkgVersion(char *vers) {
  PKG_VERSION = vers;
}

char *defltPkgVersion() {
  return PKG_VERSION;
}

static void initStdUri() {
  if (starHome == NULL) {
    char *home = getenv(STAR_HOME);

    if (home != NULL)
      starHome = uniDuplicate(home);
    else {
      char buff[MAXFILELEN];
      strMsg(buff, NumberOf(buff), "/opt/star/share/stdlib/");
      starHome = uniDuplicate(buff);
    }
  }
}
