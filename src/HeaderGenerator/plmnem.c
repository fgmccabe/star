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
    strBufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genPrologIns(O_IO(mnemBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the label generator

    strBufferPo lblBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) prologPc(O_IO(lblBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer lblLen;
    char *lblCode = getTextFromBuffer(lblBuff, &lblLen);
    hashPut(vars, "Lbls", lblCode);

    // Set up the display code
    strBufferPo showBuff = newStringBuffer();
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
    case lVl:
    case tPe:
    case cDe:
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
    case lVl:
    case tPe:
    case cDe:
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
    case tPe:
    case sym:
    case lne:
    case glb:
    case Es:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case off:
    case cDe:
    case lVl:
      outMsg(out, "%s%s", sep, var);
      return ",";
    default:
      check(False, "Cannot generate argument code");
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

  outMsg(out, "|Ins],Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc,Pcx,Ends,[%d", op);

  // Mega hack :(
  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      outMsg(out, "|M],Cdx) :- Pc1 is Pc+1,\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
      break;
    case lne:
    case lit:
    case sym:
    case tPe:
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",LtNo|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case i32:
        case art:
        case arg:
        case lcl:
        case lcs:
        case glb:
        case Es:
          outMsg(out, ",LtNo,W|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case off:
          outMsg(out, ",LtNo,Off|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      findLbl(W,Lbls,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case lVl:
          outMsg(out, ",LtNo,Off|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      findLevel(W,Ends,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case cDe:
          outMsg(out, ",LtNo,Off|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      assemBlock(W,Lbls,Lt1,Lt2,Lc,Lc2,Lns,Ln2,Pc1,Pc2,Ends,M,Mx),\n");
          outMsg(out, "      pcGap(Pc1,Pc2,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt2,Ltx,Lc2,Lcx,Ln2,Lnx,Pc2,Pcx,Ends,Mx,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
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
          outMsg(out, ",V|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case i32:
        case art:
        case arg:
        case lcl:
        case lcs:
        case glb:
        case Es:
          outMsg(out, ",V,W|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case off:
          outMsg(out, ",V,Off|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLbl(W,Lbls,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
          exit(1);
      }
      break;
    case lVl:
      outMsg(out, ",Off|M],Cdx) :- Pc1 is Pc+3,\n");
      outMsg(out, "      findLevel(V,Ends,Tgt),\n");
      outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
      break;
    case off:                            // program counter relative offset
      outMsg(out, ",Off|M],Cdx) :- Pc1 is Pc+3,\n");
      outMsg(out, "      findLbl(V,Lbls,Tgt),\n");
      outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
      break;
    default:
      check(False, "Cannot generate instruction");
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

  // Mega hack :(
  switch (A1) {
    case nOp:                             // No operand
    case tOs:
    case lne:
    case lit:
    case sym:
    case tPe:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case glb:
    case Es:
    case off:
    case lVl:
      switch (A2) {
        case nOp:
        case tOs:
        case i32:
        case art:
        case arg:
        case lcl:
        case lcs:
        case glb:
        case Es:
        case off:
        case tPe:
        case lVl: {
          char *sep = genArg(out, "(", A1, "_A");
          sep = genArg(out, sep, A2, "_B");
          if (strcmp(sep, ",") == 0)
            outStr(out, ")");

          outMsg(out, "|Ins],Pc,Pcx,Lbls,Lbx) :- !, ");
          outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
          outMsg(out, " genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).\n");
          break;
          case cDe: { // code in second argument
            char *sep = genArg(out, "(", A1, "_");
            sep = genArg(out, sep, A2, "B");
            if (strcmp(sep, ",") == 0)
              outStr(out, ")");

            outMsg(out, "|Ins],Pc,Pcx,Lbls,Lbx) :- !, ");
            outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
            outMsg(out, "  genLblTbl(B,Pc1,Pc2,Lbls,Lb1), genLblTbl(Ins,Pc2,Pcx,Lb1,Lbx).\n");
          }
          break;
          default:
            check(False, "Cannot generate instruction");
        }
      }
      break;
    case cDe:
      switch (A2) {
        case nOp:
        case tOs:
        case i32:
        case art:
        case arg:
        case lcl:
        case lcs:
        case glb:
        case Es:
        case off:
        case tPe:
        case lVl: {
          char *sep = genArg(out, "(", A1, "A");
          sep = genArg(out, sep, A2, "_");
          if (strcmp(sep, ",") == 0)
            outStr(out, ")");

          outMsg(out, "|Ins],Pc,Pcx,Lbls,Lbx) :- !, ");
          outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
          outMsg(out, "  genLblTbl(A,Pc1,Pc2,Lbls,Lb1), genLblTbl(Ins,Pc2,Pcx,Lb1,Lbx).\n");
          break;
        }
        case cDe: { // code in second argument
          char *sep = genArg(out, "(", A1, "A");
          sep = genArg(out, sep, A2, "B");
          if (strcmp(sep, ",") == 0)
            outStr(out, ")");

          outMsg(out, "|Ins],Pc,Pcx,Lbls,Lbx) :- !, ");
          outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
          outMsg(out,
                 "  genLblTbl(A,Pc1,Pc2,Lbls,Lb1), genLblTbl(B,Pc2,Pc3,Lb1,Lb2), genLblTbl(Ins,Pc3,Pcx,Lb2,Lbx).\n");
          break;
        }
        default:
          check(False, "Cannot generate instruction");
      }
  }
}

typedef struct {
  integer pcV;
  char *sep;
} OpRes;

static void showOperand(ioPo out, opAndSpec A, char *vn, char *Vtxt, OpRes *resIn) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lne:
    case lit:
    case sym:
    case tPe:
      outMsg(out, "  ssTrm(%s,0,%s),\n", vn, Vtxt);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV + 1, resIn->pcV);
      resIn->pcV++;
      break;
    case Es:
    case lcl:
    case lcs:
    case glb:
    case off:
      outMsg(out, "  %s=ss(%s),\n", Vtxt, vn);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV + 1, resIn->pcV);
      resIn->pcV++;
      break;
    case cDe:
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV + 1, resIn->pcV);
      outMsg(out, "  %s=iv(nl(2),TT),\n", Vtxt);
      outMsg(out, "  showMnem(%s,0,Lbls,TT),\n", vn);
      resIn->pcV++;
      break;

    case i32:
    case art:
    case arg:
    case lVl:
      outMsg(out, "  %s=ix(%s),\n", Vtxt, vn);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV + 1, resIn->pcV);
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

  char *sep1 = "";
  char *sep2 = "";
  char *V1 = "";
  char *V2 = "";

  if (A1 != nOp) {
    if (A1 != tOs) {
      sep1 = ", ss(\" \"), ";
      V1 = "UU";
    }

    if (A2 != nOp) {
      if (A2 != tOs) {
        sep2 = ", ss(\",\"), ";
        V2 = "VV";
      }
    }
  }

  outMsg(out, "|Ins],Pc,Lbls,[sq([ix(Pc),ss(\":\"),ss(\"%P\")%s%s%s%s])|II]) :- !,\n", mnem, sep1, V1, sep2, V2);
  outMsg(out, "  Pc0 is Pc+1,\n");

  OpRes res1 = {.pcV=0};

  showOperand(out, A1, "U", "UU", &res1);
  showOperand(out, A2, "V", "VV", &res1);

  outMsg(out, "  showMnem(Ins,Pc%ld,Lbls,II).\n", res1.pcV);
}
