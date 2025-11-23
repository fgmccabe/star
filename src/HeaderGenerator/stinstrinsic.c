#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "signature.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include "formexts.h"
#include "starsig.h"

/* Generate a Star module, that knows about the intrinsics */

char *prefix = "star.comp.intrinsics";
char *templateFn = "intrinsics.star.plate";
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
      default: ;
    }
  }
  return optind;
}

static void genStarIntrinsic(ioPo out, char *name, char *tipe, char *op, logical Alloc, char *cmt);

int main(int argc, char **argv) {
  initLogfile("-");
  installMsgProc('P', genQuotedStr);
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
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
      exit(1);
    }

    ioPo out;

    if (narg < argc)
      out = openOutFile(argv[narg], utf8Encoding);
    else
      out = Stdout();

    // Template variables
    hashPo vars = newHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Date", date);

    // Set up the assembler proper
    strBufferPo mnemBuff = newStringBuffer();

#undef intrinsic
#define intrinsic(NM, Tp, Op, Alloc, cmt) genStarIntrinsic(O_IO(mnemBuff),#NM,Tp,Op,Alloc, cmt);

#include "intrinsics.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Intrinsics", allCode);

    processTemplate(out, plate, vars, NULL, NULL);

    closeIo(out);
    exit(0);
  }
}

static char *capitalize(char *str);

static void genStarIntrinsic(ioPo out, char *name, char *tipe, char *op, logical Alloc, char *cmt) {
  outMsg(out, "    | \"%s\" => .some((", name);
  dumpStarSig(tipe, out);
  if (tipe[0] == throwSig)
    outMsg(out, ",(Lb)=>.i%s(Lb), %s))  -- %s\n", capitalize(op), (Alloc ? ".true" : ".false"), cmt);
  else
    outMsg(out, ",(_)=>.i%s, %s))  -- %s\n", capitalize(op), (Alloc ? ".true" : ".false"), cmt);
}

static char *capitalize(char *str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char) ('A' + (buffer[0] - 'a'));
  }
  return buffer;
}
