/*
 * main driver to parse and assemble a file
 */
#include "config.h"
#include "asm.h"
#include "errors.h"
#include "assemP.h"
#include "manifest.h"

#include <stdlib.h>

extern int ssparse(ioPo file, pkgPo *pkg);

static void initStdUri();

retCode parseContent(char *path) {
  initStdUri();
  ioPo file = openInFile(path, utf8Encoding);

  if (file != Null) {
    codePoint ch;

    retCode ret = inChar(file, &ch);

    if (ret == Ok) {
      if (ch == '#') {      /* look for standard #!/.... header */
        ret = inChar(file, &ch);
        if (ret == Ok && ch == '!') {
          while ((inChar(file, &ch)) == Ok && ch != uniEOF &&
                 ch != '\n');              /* consume the interpreter statement */
        } else {
          unGetChar(file, ch);
          unGetChar(file, '#');
        }
      } else
        unGetChar(file, ch);

      pkgPo pkg = Null;
      ssparse(file, &pkg);

      if (debugAssem)
        dumpPkgCode(pkg);

      closeFile(file);      /* close the source string file */
      if (isErrorFree()) {
        char buff[MAXFILELEN], outFn[MAXFILELEN];
        char *codeName = manifestOutPath(pkg->name, pkg->version, "co", buff, NumberOf(buff));
        char *outPath = repoRsrcPath(codeName,outFn,NumberOf(outFn));
        ioPo out = openOutFile(outPath, rawEncoding);
        ret = encodePkg(out, pkg);

        closeFile(out);

        if(ret==Ok){
          ret = addToManifest(pkg->name,pkg->version,"code",codeName);

          if(ret==Ok)
            ret = flushManifest();
        }

        return ret;
      } else {
        outMsg(logFile, "output not written\n");
        return Fail;
      }
    } else
      return Fail;
  } else
    return Fail;
}

static char *CAFE_HOME = NULL;

void setCafeHome(char *home) {
  CAFE_HOME = home;
}


static void initStdUri() {
  if (CAFE_HOME == NULL) {
    char *home = getenv("CAFE_HOME");

    if (home != NULL)
      CAFE_HOME = uniDuplicate(home);
    else {
      char buff[MAXFILELEN];
      strMsg(buff, NumberOf(buff), "/opt/cafe/share/stdlib/");
      CAFE_HOME = uniDuplicate(buff);
    }
  }
}
