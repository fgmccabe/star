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

/* Generate a Prolog module, that knows how to assemble a program */

char *prefix = "star.comp.assem";
char *templateFn = "assem.pl.plate";
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

static char *dot(opAndSpec A);
static void genPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);
static void prologPc(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);
static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);
static void insOp(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt);

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
#define instruction(M, A1, Dl, cmt) genPrologIns(O_IO(mnemBuff),#M,M,A1,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the label generator

    bufferPo lblBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, Dl, cmt) prologPc(O_IO(lblBuff),#M,M,A1,cmt);

#include "instructions.h"

    integer lblLen;
    char *lblCode = getTextFromBuffer(lblBuff, &lblLen);
    hashPut(vars, "Lbls", lblCode);

    // Set up the display code
    bufferPo showBuff = newStringBuffer();
#undef instruction
#define instruction(M, A1, Dl, cmt) showPrologIns(O_IO(showBuff),#M,M,A1,cmt);

#include "instructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(showBuff, &showLen);
    hashPut(vars, "Show", showCode);

    // Set up the opcodes for the type definition
    bufferPo typeBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, Dl, Cmt) insOp(O_IO(typeBuff),#M,M,A1,Cmt);

#include "instructions.h"

    integer tpLen;
    char *typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

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
    case sym:
    case lne:
    case glb:
    case Es:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
      outMsg(out, "%sV", sep);
      return ",";
    case off:
      outMsg(out, "%sV", sep);
      return ",";
    default:
      printf("Problem in generating opcode type\n");
      exit(11);
  }
}

char *tail = "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n";

static void genPrologCode(ioPo out, int op, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "%d|M]) :- Pc1 is Pc+1,\n", op);
      break;
    case lne:
    case lit:
    case sym:
      outMsg(out, "%d,LtNo|M]) :- Pc1 is Pc+3,\n", op);
      outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
      return;
    case i32:
    case art:
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

static void genPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  char *sep = "(";

  outMsg(out, "mnem([i%s", capitalize(mnem));

  sep = genArg(out, sep, A1);

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  outMsg(out, "%s|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[", sep);

  genPrologCode(out, op, A1);
}

char *dot(opAndSpec A) {
  switch (A) {
    case nOp:
    case tOs:
      return ".";
    default:
      return "";
  }
}

void prologPc(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  outMsg(out, "genLblTbl([i%s", capitalize(mnem));

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case sym:
    case lne:
    case i32:
    case art:
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
    case sym:
    case lne:
    case i32:
    case art:
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

void insOp(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  outMsg(out, "    %si%s", dot(A1), mnem);
  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case lne:
      outMsg(out, "(term)");
      break;
    case sym:
      outMsg(out, "(termLbl)");
      break;
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
      outMsg(out, "(integer)");
      break;

    case off:
      outMsg(out, "(assemLbl)");
      break;

    case Es:
    case glb:
      outMsg(out, "(string)");
      break;

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
  outMsg(out, " |\n");
}

static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, char *cmt) {
  outMsg(out, "showMnem([i%s", capitalize(mnem));

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case sym:
    case lne:
    case i32:
    case art:
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
    case lne:
    case lit:
    case sym:
      outMsg(out, "  showTerm(XX,0,O1,O2),\n");
      outMsg(out, "  appNl(O2,O3),\n");
      Oy = "O3";
      break;
    case Es:
      outMsg(out, "  appStr(XX,O1,O2),\n");
      outMsg(out, "  appNl(O2,O3),\n");
      Oy = "O3";
      break;

    case i32:
    case art:
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
    case sym:
    case lne:
    case i32:
    case art:
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
