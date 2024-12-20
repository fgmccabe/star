//
// Created by Francis McCabe on 12/20/24.
//


/* Generate a Star module that knows about escapes */

#include <stdio.h>
#include <getopt.h>
#include "signature.h"
#include "ooio.h"
#include "formioP.h"
#include "formexts.h"
#include "template.h"
#include <ctype.h>
#include "starescapes.h"

char *prefix = "star.comp.escapes";
char *templateFn = "escapes.star.plate";
char date[MAXLINE] = "";

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "t:d:")) >= 0) {
    switch (opt) {
      case 't':
        templateFn = optarg;
        break;
      case 'd':
        uniCpy(date, NumberOf(date), optarg);
        break;
      default:;
    }
  }
  return optind;
}

static void genStarEsc(ioPo out, char *name, char *sig, char *cmt);
static void genStarIsEsc(ioPo out, char *name);

int main(int argc, char **argv) {
  initLogfile("-");
  installMsgProc('P', genQuotedStr);
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    return 1;
  } else {
    if (uniStrLen(date) == 0) {
      time_t rawtime;
      time(&rawtime);
      struct tm *timeinfo = localtime(&rawtime);

      strftime(date, NumberOf(date), "%c", timeinfo);
    }
    ioPo plate = openInFile(templateFn, utf8Encoding);

    if (plate == Null) {
      outMsg(logFile, "cannot find template file %s\n", templateFn);
      return 1;
    }

    ioPo out = ((narg < argc) ? openOutFile(argv[narg], utf8Encoding) : Stdout());

    // Template variables
    hashPo vars = newHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Date", date);

    strBufferPo escapeBuffer = newStringBuffer();

#undef escape
#define escape(name, type, cmt) genStarEsc(O_IO(escapeBuffer),#name,type,cmt);

#include "escapes.h"

    integer insLen;
    char *allCode = getTextFromBuffer(escapeBuffer, &insLen);
    hashPut(vars, "Escapes", allCode);

    strBufferPo isEscBuffer = newStringBuffer();

#undef escape
#define escape(name, type, cmt) genStarIsEsc(O_IO(isEscBuffer),#name);

#include "escapes.h"

    integer isLen;
    char *escCode = getTextFromBuffer(isEscBuffer, &isLen);
    hashPut(vars, "IsEscape", escCode);

    processTemplate(out, plate, vars, NULL, NULL);

    closeIo(out);
    return (0);
  }
}

void genStarEsc(ioPo out, char *name, char *sig, char *cmt) {
  outStr(O_IO(out), "    | ");
  dumpStr(name, out);
  outStr(O_IO(out), " => .some(");
  dumpStarSig(sig, out);
  outStr(O_IO(out), ")\n");
}

void genStarIsEsc(ioPo out, char *name) {
  outStr(O_IO(out), "    | ");
  dumpStr(name, out);
  outMsg(O_IO(out), " => .true\n");
}
