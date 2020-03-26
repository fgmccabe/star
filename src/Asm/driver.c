/*
 * main driver to parse and assemble a file
 */
#include "config.h"
#include "asm.h"
#include "errors.h"
#include "assemP.h"
#include "manifest.h"

#include <stdlib.h>
#include <manifestP.h>

extern int ssparse(ioPo file, pkPo *pkg);
int ssdebug = 0;

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
      ioPo out = newOutFile(outPath, rawEncoding);
      tryRet(encodePkg(out, pkg));

      tryRet(closeFile(out));

      tryRet(addToManifest(&pkg->pkg, "code", codeName, uniStrLen(codeName)));

      tryRet(addToManifest(&pkg->pkg, "source", path, uniStrLen(path)));

      tryRet(addToManifest(&pkg->pkg, "signature", pkg->signature, uniStrLen(pkg->signature)));

      tryRet(flushManifest());

      return Ok;
    } else {
      outMsg(logFile, "output not written\n");
      return Fail;
    }
  } else {
    outMsg(logFile, "cannot file source file %s\n", path);

    return Fail;
  }
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
