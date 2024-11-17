#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "hash.h"
#include "unistr.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include <assert.h>
#include "formexts.h"

/* Generate the opcodes.h header file */

char *templateFn = "opcodes.plate";
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

static integer staropHash();
static void insOp(ioPo out, char *mnem, int op, char *cmt);

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

    // Set up the opcodes for the type definition
    strBufferPo typeBuff = newStringBuffer();

    int Op = 0;

#undef instruction
#define instruction(M, A1, A2, Dl, _, Cmt) insOp(O_IO(typeBuff),#M,Op, Cmt); Op++;

#include "instructions.h"

    integer tpLen;
    char *typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

    static char hashBuff[64];
    strMsg(hashBuff,NumberOf(hashBuff),"%ld",staropHash());
    hashPut(vars, "Hash", hashBuff);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    flushOut();
    closeIo(out);
    exit(0);
  }
}

void insOp(ioPo out, char *mnem, int op, char *cmt) {
  outMsg(out, "    %s = %d,            // %s\n", mnem,op,cmt);
}

static integer opHash(char *mnem,int op){
  return hash61(strhash(mnem)*37+op);
}

integer staropHash(){
  integer hash = 0;
  int Op = 0;

#undef instruction
#define instruction(M, A1, A2, Dl, _, Cmt) hash = hash61(hash*39+opHash(#M,Op)); Op++;

#include "instructions.h"

  return hash;
}

