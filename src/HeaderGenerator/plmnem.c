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
#include <assert.h>

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
static void genPrologHwm(ioPo out, char *mnem, int op, int delta, opAndSpec A1, opAndSpec A2, char *cmt);
static void prologPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static integer insSize(OpCode op, opAndSpec A1, opAndSpec A2);
static integer staropHash();

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

    {
      // Set up the hwm calculator
      strBufferPo hwmBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genPrologHwm(O_IO(hwmBuff),#M,M,Dl,A1,A2,cmt);

#include "instructions.h"

      integer insLen;
      char *allCode = getTextFromBuffer(hwmBuff, &insLen);
      hashPut(vars, "Hwm", allCode);
    }


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

    static char hashBuff[64];
    strMsg(hashBuff, NumberOf(hashBuff), "%ld", staropHash());
    hashPut(vars, "Hash", hashBuff);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    closeIo(out);
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
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
    case tPe:
    case bLk:
      sz += 2;
      break;
  }
  switch (A2) {
    default:
      break;
    case lit:
    case sym:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
    case bLk:
    case tPe:
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
    case glb:
    case Es:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case off:
    case bLk:
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
      switch (A2) {
        case i32: {
          outMsg(out, ",W|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        }
        case off: {
          outMsg(out, ",Off|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLbl(W,Lbls,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        }
        default:
          outMsg(out, "|M],Cdx) :- Pc1 is Pc+1,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
      }
      break;
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
    case lVl:
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
          outMsg(out, ",V,W|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        case off:
          outMsg(out, ",Cd,Off|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
          outMsg(out, "      findLbl(W,Lbls,Tgt),\n");
          outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
          outMsg(out, "      isEscape(V,Cd),!,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
          exit(1);
      }
      break;
    case off:                            // program counter relative offset
      outMsg(out, ",Off|M],Cdx) :- Pc1 is Pc+3,\n");
      outMsg(out, "      findLbl(V,Lbls,Tgt),\n");
      outMsg(out, "      pcGap(Pc1,Tgt,Off),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pcx,Ends,M,Cdx).\n");
      break;
    case bLk:
      outMsg(out, ",B|M],Cdx) :- Pc1 is Pc+%d,\n", insSize(op, A1, A2));
      outMsg(out, "      assemBlock(V,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,Pc2,Ends,B,[]),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc2,Pcx,Ends,M,Cdx).\n");
    default:
      break;
      check(False, "Cannot generate instruction");
      exit(1);
  }
}

// Construct the HWM calculator
static void genPrologHwm(ioPo out, char *mnem, int op, int delta, opAndSpec A1, opAndSpec A2, char *cmt) {
  char *sep = "(";

  outMsg(out, "hwm([i%s", capitalize(mnem));

  sep = genArg(out, sep, A1, "V");
  sep = genArg(out, sep, A2, "W");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, "|Ins],Cur,H,Hwm) :-\n");
  if (delta != 0) {
    outMsg(out, "  Cur1 is Cur%s%d,\n", (delta > 0 ? "+" : ""), delta);
    outMsg(out, "  (Cur1>H -> H1 = Cur1 ; H1 = H),\n");
    outMsg(out, "  hwm(Ins,Cur1,H1,Hwm).\n");
  } else {
    outMsg(out, "  hwm(Ins,Cur,H,Hwm).\n");
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
        case tPe: {
          char *sep = genArg(out, "(", A1, "_A");
          sep = genArg(out, sep, A2, "_B");
          if (strcmp(sep, ",") == 0)
            outStr(out, ")");

          outMsg(out, "|Ins],Pc,Pcx,Lbls,Lbx) :- !, ");
          outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
          outMsg(out, " genLblTbl(Ins,Pc1,Pcx,Lbls,Lbx).\n");
          break;
          default:
            check(False, "Cannot generate instruction");
        }
        case bLk: {
          check(False, "Illegal operand");
        }
      }
      break;
    case bLk: {
      check(A2 == nOp, "illegal second operand");
      char *sep = genArg(out, "(", A1, "A");
      sep = genArg(out, sep, A2, "_B");
      if (strcmp(sep, ",") == 0)
        outStr(out, ")");

      outMsg(out, "|Ins],Pc,Pcx,Lbls,Lbx) :- !, ");
      outMsg(out, "Pc1 is Pc+%d, ", insSize(op, A1, A2));
      outMsg(out, "genLblTbl(A,Pc1,Pc2,Lbls,Lb1), ");
      outMsg(out, " genLblTbl(Ins,Pc2,Pcx,Lb1,Lbx).\n");
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
    case lit:
    case sym:
    case tPe:
      outMsg(out, "  ssTrm(%s,0,%s),\n", vn, Vtxt);
      outMsg(out, "  bumpPc(Pc%ld,2,Pc%ld),\n", resIn->pcV, resIn->pcV + 1);
      resIn->pcV++;
      break;
    case lcl:
    case lcs:
    case glb:
    case off:
      outMsg(out, "  %s=ss(%s),\n", Vtxt, vn);
      outMsg(out, "  bumpPc(Pc%ld,2,Pc%ld),\n", resIn->pcV, resIn->pcV + 1);
      resIn->pcV++;
      break;

    case i32:
    case art:
    case arg:
    case lVl:
      outMsg(out, "  %s=ix(%s),\n", Vtxt, vn);
      outMsg(out, "  bumpPc(Pc%ld,2,Pc%ld),\n", resIn->pcV, resIn->pcV + 1);
      resIn->pcV++;
      break;
    case Es:
      outMsg(out, "  %s=ss(%s),!,\n", Vtxt, vn);
      outMsg(out, "  bumpPc(Pc%ld,2,Pc%ld),\n", resIn->pcV, resIn->pcV + 1);

      outMsg(out, "  bumpPc(Pc%ld,2,Pc%ld),\n", resIn->pcV, resIn->pcV + 1);
      resIn->pcV++;
      break;
    case bLk:
      outMsg(out, "  blockPc(Pc,SPc),\n", resIn->pcV);
      outMsg(out, "  showMnem(%s, SPc, _, Lbls, Ms),\n", vn);
      outMsg(out, "  %s = iv(ss(\"\\n\"), Ms),\n", Vtxt);
//      outMsg(out, "  %s=ss(%s),!,\n", Vtxt, vn);
      outMsg(out, "  Pc%ld is Pc%ld+2,\n", resIn->pcV, resIn->pcV + 1);
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

  switch (A1) {
    case nOp:
      break;
    default:
      sep1 = ", ss(\" \"), ";
      V1 = "UU";
    case tOs:
      switch (A2) {
        case nOp:
        case tOs:
          break;
        default:
          sep2 = ", ss(\",\"), ";
          V2 = "VV";
          break;

          break;
      }
  }

  outMsg(out, "|Ins],Pc,PcX,Lbls,[sq([PcDx,ss(\":\"),ss(\"%P\")%s%s%s%s])|II]) :- !,\n", mnem, sep1, V1, sep2, V2);
  outMsg(out, "  showPc(Pc,PcDx),\n");
  outMsg(out, "  bumpPc(Pc,1,Pc0),\n");

  OpRes res1 = {.pcV=0};

  showOperand(out, A1, "U", "UU", &res1);
  showOperand(out, A2, "V", "VV", &res1);

  outMsg(out, "  showMnem(Ins,Pc%ld,PcX,Lbls,II).\n", res1.pcV);
}

static integer opHash(char *mnem, int op) {
  return hash61(strhash(mnem) * 37 + op);
}

integer staropHash() {
  integer hash = 0;

#undef instruction
#define instruction(M, A1, A2, Dl, Cmt) hash = hash61(hash*39+opHash(#M,M));

#include "instructions.h"

  return hash;
}
