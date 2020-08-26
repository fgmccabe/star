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
static void genPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static void prologPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static integer insSize(OpCode op, opAndSpec A1, opAndSpec A2);

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
    bufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genPrologIns(O_IO(mnemBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the label generator

    bufferPo lblBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) prologPc(O_IO(lblBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer lblLen;
    char *lblCode = getTextFromBuffer(lblBuff, &lblLen);
    hashPut(vars, "Lbls", lblCode);

    // Set up the display code
    bufferPo showBuff = newStringBuffer();
#undef instruction
#define instruction(M, A1, A2, Dl, cmt) showPrologIns(O_IO(showBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(showBuff, &showLen);
    hashPut(vars, "Show", showCode);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    closeFile(out);
    exit(0);
  }
}

static integer insSize(OpCode op, opAndSpec A1, opAndSpec A2) {
  integer sz = 1;

  switch (A1) {
    default:
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
      sz += 2;
      break;
  }
  switch (A2) {
    default:
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
      sz += 2;
      break;
  }
  return sz;
}

static char *genArg(ioPo out, char *sep, opAndSpec A, char *var) {
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
      outMsg(out, "%s%s", sep, var);
      return ",";
    case off:
      outMsg(out, "%s%s", sep, var);
      return ",";
    default:
      printf("Problem in generating opcode type\n");
      exit(11);
  }
}

static char *capitalize(char *str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char) ('A' + (buffer[0] - 'a'));
  }
  return buffer;
}

static void genPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  char *sep = "(";

  outMsg(out, "mnem([i%s", capitalize(mnem));

  sep = genArg(out, sep, A1, "V");
  sep = genArg(out, sep, A2, "W");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, "|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,[%d", op);

  // Mega hack :(
  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "|M]) :- Pc1 is Pc+1,\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
      break;
    case lne:
    case lit:
    case sym:
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",LtNo|M]) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
          break;
        case i32:
        case art:
        case arg:
        case lcl:
        case lcs:
        case glb:
        case Es:
          outMsg(out, ",LtNo,W|M]) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
          break;
        case off:
          outMsg(out, ",LtNo,Off|M]) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      findLbl(W,Lbls,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
          break;
        default:
          outMsg(out, "Unknown instruction type code\n");
          exit(1);
      }
      break;
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case glb:
    case Es:
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",V|M]) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
          break;
        case i32:
        case art:
        case arg:
        case lcl:
        case lcs:
        case glb:
        case Es:
          outMsg(out, ",V,W|M]) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
          break;
        case off:
          outMsg(out, ",V,Off|M]) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLbl(W,Lbls,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
          break;
        default:
          outMsg(out, "Unknown instruction type code\n");
          exit(1);
      }
      break;
    case off:                            // program counter relative offset
      outMsg(out, ",Off|M]) :- Pc1 is Pc+3,\n");
      outMsg(out, "      findLbl(V,Lbls,Tgt),\n");
      outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n");
      break;
    default:
      outMsg(out, "Unknown instruction type code\n");
      exit(1);
  }
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

void prologPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "genLblTbl([i%s", capitalize(mnem));

  char *sep = genArg(out, "(", A1, "_");
  sep = genArg(out, sep, A2, "_");
  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, "|Ins],Pc,Lbls,Lbx) :- !, ");
  outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
  outMsg(out, " genLblTbl(Ins,Pc1,Lbls,Lbx).\n");
}

typedef struct {
  integer chV;
  integer pcV;
  char *sep;
} OpRes;

static void showSep(ioPo out, OpRes *res) {
  if (res->sep != Null) {
    outMsg(out, "  appStr(\",\",O%d,O%d),\n", res->chV, res->chV + 1);
    res->chV++;
  }
  res->sep = ",";
}

static void showOperand(ioPo out, opAndSpec A, char *vn, OpRes *resIn) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lne:
    case lit:
    case sym:
      showSep(out, resIn);
      outMsg(out, "  showTerm(%s,0,O%d,O%d),\n", vn, resIn->chV, resIn->chV + 1);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV+1, resIn->pcV);
      resIn->chV++;
      resIn->pcV++;
      break;
    case Es:
    case lcl:
    case lcs:
    case glb:
    case off:
      showSep(out, resIn);
      outMsg(out, "  appStr(%s,O%d,O%d),\n", vn, resIn->chV, resIn->chV + 1);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV+1, resIn->pcV);
      resIn->chV++;
      resIn->pcV++;
      break;

    case i32:
    case art:
    case arg:
      showSep(out, resIn);
      outMsg(out, "  appInt(%s,O%d,O%d),\n", vn, resIn->chV, resIn->chV + 1);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV+1, resIn->pcV);
      resIn->chV++;
      resIn->pcV++;
      break;
  }
}

static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "showMnem([i%s", capitalize(mnem));

  char *sep = genArg(out, "(", A1, "U");
  sep = genArg(out, sep, A2, "V");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, "|Ins],Pc,Lbls,O,Ox) :- !,\n");

  outMsg(out, "  appInt(Pc,O,O0),\n  appStr(\":\",O0,O00),\n");
  outMsg(out, "  appStr(\"%P \",O00,O1),\n", mnem);
  outMsg(out, "  Pc0 is Pc+1,\n");

  OpRes res1 = {.pcV=0, .chV=1, .sep=Null};

  showOperand(out, A1, "U", &res1);
  showOperand(out, A2, "V", &res1);

  outMsg(out, "  appNl(O%d,O%d),\n", res1.chV, res1.chV + 1);
  outMsg(out, "  showMnem(Ins,Pc%ld,Lbls,O%d,Ox).\n", res1.pcV, res1.chV + 1);
}
