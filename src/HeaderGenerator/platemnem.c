#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "opcodes.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include <stringBuffer.h>
#include "formexts.h"

/* Generate a Prolog or Star module, that knows how to assemble a program */

enum {
  genProlog, genStar
} genMode = genProlog;

char *prefix = "star.comp.assem";
char *templateFn = "assem.star.plate";
char date[MAXLINE] = "";

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "ps:t:d:")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 's':
        genMode = genStar;
        prefix = optarg;
        break;
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

static void genIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);
static void bmpPc(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);
static void showIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);

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
    hashPo vars = NewHash(8, (hashFun) uniHash, (compFun) uniCmp, NULL);
    hashPut(vars, "Date", date);

    // Set up the assembler proper
    bufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, Dl, cmt) genIns(O_IO(mnemBuff),#M,M,A1,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(&insLen, mnemBuff);
    hashPut(vars, "Mnem", allCode);

    // Set up the label generator

    bufferPo lblBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, Dl, cmt) bmpPc(O_IO(lblBuff),#M,M,A1,cmt);

#include "instructions.h"

    integer lblLen;
    char *lblCode = getTextFromBuffer(&lblLen, lblBuff);
    hashPut(vars, "Lbls", lblCode);

    // Set up the display code
    bufferPo showBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, Dl, cmt) showIns(O_IO(showBuff),#M,M,A1,cmt);

#include "instructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(&showLen, showBuff);
    hashPut(vars, "Show", showCode);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    closeFile(out);
    exit(0);
  }
}

static char *genArg(ioPo out, char *sep, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      return sep;
    case lit:
    case glb:
    case Es:
    case i32:
    case arg:
    case lcl:
    case lcs:
    case off:
      outMsg(out, "%sV", sep);
      return ",";
    default:
      printf("Problem in generating opcode type\n");
      exit(11);
  }
}

char *tail = "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).\n";

static void genCode(ioPo out, int op, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "%d|M]) :- Pc1 is Pc+1,\n", op);
      break;
    case lit:
      outMsg(out, "%d,LtNo|M]) :- Pc1 is Pc+3,\n", op);
      outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).\n");
      return;
    case i32:
    case arg:
    case lcl:
    case lcs:
      outMsg(out, "%d,V|M]) :- Pc1 is Pc+3,\n", op);
      break;
    case glb:
    case Es:                              // escape code (0..65535)
      outMsg(out, "%d,V|M]) :- Pc1 is Pc+3,\n", op);
      break;
    case off:                            // program counter relative offset
      outMsg(out, "%d,Off|M]) :- Pc1 is Pc+3,\n", op);
      outMsg(out, "      findLbl(V,Lbls,Tgt),\n");
      outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
      break;
    default:
      outMsg(out, "Unknown instruction type code\n");
      exit(1);
  }
  outMsg(out, "%s", tail);
}

static char *capitalize(char *str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char) ('A' + (buffer[0] - 'a'));
  }
  return buffer;
}

static void genIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  char *sep = "(";

  outMsg(out, "mnem([i%s", capitalize(mnem));

  sep = genArg(out, sep, A1);

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  outMsg(out, "%s|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[", sep);

  genCode(out, op, A1);
}

void bmpPc(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  outMsg(out, "genLblTbl([i%s", capitalize(mnem));

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case i32:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      outMsg(out, "(_)");
      break;

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }

  outMsg(out, "|Ins],Pc,Lbls,Lbx) :- !, ");

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "Pc1 is Pc+1, ");
      break;
    case lit:
    case i32:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      outMsg(out, "Pc1 is Pc+3, ");
      break;
  }

  outMsg(out, " genLblTbl(Ins,Pc1,Lbls,Lbx).\n");
}

static void showIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  outMsg(out, "showMnem([i%s", capitalize(mnem));

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case i32:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      outMsg(out, "(XX)");
      break;

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }

  outMsg(out, "|Ins],Pc,Lbls,O,Ox) :- !,\n");

  outMsg(out, "  appInt(Pc,O,O0),\n  appStr(\":\",O0,O00),\n");
  outMsg(out, "  appStr(\"%P \",O00,O1),\n", mnem);

  char *Oy = "O1";

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "  appNl(O1,O2),\n");
      Oy = "O2";
      break;
    case lit:
      outMsg(out,"  showTerm(XX,0,O1,O2),\n");
      outMsg(out, "  appNl(O2,O3),\n");
      Oy = "O3";
      break;
    case Es:
      outMsg(out,"  appStr(XX,O1,O2),\n");
      outMsg(out, "  appNl(O2,O3),\n");
      Oy = "O3";
      break;

    case i32:
    case arg:
      outMsg(out, "  appInt(XX,O1,O2),\n");
      outMsg(out, "  appNl(O2,O3),\n");
      Oy = "O3";
      break;
    case lcl:
    case lcs:
    case glb:
    case off:
      outMsg(out, "  appStr(XX,O1,O2),\n");
      outMsg(out, "  appNl(O2,O3),\n");
      Oy = "O3";
      break;
  }

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "  Pc1 is Pc+1,\n");
      break;
    case lit:
    case i32:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      outMsg(out, "  Pc1 is Pc+3,\n");
      break;
  }

  outMsg(out, "  showMnem(Ins,Pc1,Lbls,%s,Ox).\n", Oy);
}
