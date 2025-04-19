#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "opcodes.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
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

static void genPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);

static void genStackHwm(ioPo out, char *mnem, int op, int delta, opAndSpec A1, opAndSpec A2, char *cmt);

static void genLocalHwm(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);

static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);

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
#define instruction(M, A1, A2, Dl, _, cmt) genStackHwm(O_IO(hwmBuff),#M,M,Dl,A1,A2,cmt);

#include "instructions.h"

      integer insLen;
      char *allCode = getTextFromBuffer(hwmBuff, &insLen);
      hashPut(vars, "stackHwm", allCode);
    }

    {
      // Set up the localHwm calculator
      strBufferPo hwmBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, _, cmt) genLocalHwm(O_IO(hwmBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

      integer insLen;
      char *allCode = getTextFromBuffer(hwmBuff, &insLen);
      hashPut(vars, "localHwm", allCode);
    }


    // Set up the assembler proper
    strBufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, _, cmt) genPrologIns(O_IO(mnemBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the display code
    strBufferPo showBuff = newStringBuffer();
#undef instruction
#define instruction(M, A1, A2, Dl, _, cmt) showPrologIns(O_IO(showBuff),#M,M,A1,A2,cmt);

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

  outMsg(out, "|Ins],Lbls,Lt,Ltx,Ln,Lnx,Pc,Pcx,LsMap,[%d", op);

  // Mega hack :(
  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      switch (A2) {
        case i32: {
          outMsg(out, ",W|M],Cdx) :-\n");
          outMsg(out, "     Pc1 is Pc+1,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        case bLk: {
          outMsg(out, ",Lm|B],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      assemBlock(V,none,Lbls,Lt,Lt1,Ln,Ln1,Pc1,Pc2,LsMap,B,[]),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln1,Lnx,Pc2,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        case lVl: {
          outMsg(out, ",Lvl|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLevel(W,Lbls,0,Lvl),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        default:
          outMsg(out, "|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
      }
      break;
    case lit:
    case sym:
    case tPe:
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",LtNo|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case i32:
        case art:
        case arg:
        case glb:
          outMsg(out, ",LtNo,W|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lcs:
        case lcl: {
          outMsg(out, ",LtNo,Off|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      findLocal(W,LsMap,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        case bLk: {
          outMsg(out, ",LtNo,B|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      assemBlock(W,none,Lbls,Lt1,Lt2,Ln,Ln1,Pc1,Pc2,LsMap,B,[]),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt2,Ltx,Ln1,Lnx,Pc2,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        case lVl:
          outMsg(out, ",LtNo,Lvl|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLit(Lt,V,LtNo,Lt1),\n");
          outMsg(out, "      findLevel(W,Lbls,0,Lvl),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
          exit(1);
      }
      break;
    case i32:
    case art:
    case arg:
    case glb:
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",V|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case i32:
        case art:
        case arg:
        case glb:
        case Es:
          outMsg(out, ",V,W|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lVl:
          outMsg(out, ",V,Lvl|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLevel(W,Lbls,0,Lvl),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lit:
          outMsg(out, ",V,LtNo|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLit(Lt,W,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
          exit(1);
      }
      break;
    case lcs: {
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",Off|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLocal(V,LsMap,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lcs:
        case lcl: {
          outMsg(out, ",VOff,WOff|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      declareLocal(V,Lc,Lc0,VOff),\n");
          outMsg(out, "      findLocal(W,LsMap,WOff),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        case lVl:
          outMsg(out, ",V,Lvl|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLevel(W,Lbls,0,Lvl),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lit: // Special case: we know we are declaring the variable
          outMsg(out, ",Off,LtNo|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLocal(V,LsMap,Off),\n");
          outMsg(out, "      findLit(Lt,W,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbl0,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
          exit(1);
      }
      break;
    }
    case lcl: {
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",Off|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLocal(V,LsMap,Off),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lcl:
        case lcs: {
          outMsg(out, ",VOff,WOff|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLocal(V,LsMap,VOff),\n");
          outMsg(out, "      findLocal(W,LsMap,WOff),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        case lVl:
          outMsg(out, ",V,Lvl|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLevel(W,Lbls,0,Lvl),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lit: // Special case: we know we are declaring the variable
          outMsg(out, ",Off,LtNo|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLocal(V,LsMap,Off),\n");
          outMsg(out, "      findLit(Lt,W,LtNo,Lt1),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        default:
          check(False, "Cannot generate instruction");
          exit(1);
      }
      break;
    }
    case Es:
      switch (A2) {
        case nOp:
        case tOs:
          outMsg(out, ",V|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        case lVl:{
          outMsg(out, ",V,Lvl|M],Cdx) :-\n");
          outMsg(out, "      Pc1 is Pc+1,\n");
          outMsg(out, "      findLevel(W,Lbls,0,Lvl),\n");
          outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
          break;
        }
        default:
          check(False, "Illegal second operand");
          exit(1);
      }
      break;
    case lVl:                            // program counter relative offset
      outMsg(out, ",Lvl|M],Cdx) :-\n");
      outMsg(out, "      Pc1 is Pc+1,\n");
      outMsg(out, "      findLevel(V,Lbls,0,Lvl),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt,Ltx,Ln,Lnx,Pc1,Pcx,LsMap,M,Cdx).\n");
      break;
    case bLk:
      outMsg(out, ",B|M],Cdx) :-\n");
      outMsg(out, "      Pc1 is Pc+1,\n");
      outMsg(out, "      assemBlock(V,none,Lbls,Lt,Lt1,Ln,Ln1,Pc1,Pc2,LsMap,B,[]),\n");
      outMsg(out, "      mnem(Ins,Lbls,Lt1,Ltx,Ln1,Lnx,Pc2,Pcx,LsMap,M,Cdx).\n");
    default:
      break;
      check(False, "Cannot generate instruction");
      exit(1);
  }
}

static void genHwmOp(ioPo out, opAndSpec A, char *argVar, integer *currH, integer *H) {
  switch (A) {
    case bLk: {
      outMsg(out, "  stackHwm(%s,CH%d,H%d,H%d),\n", argVar, *currH, *H, (*H) + 1);
      (*H)++;
      return;
    }
    default:;
  }
}

static char *genHWmArg(ioPo out, char *sep, opAndSpec A, char *var) {
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
    case lVl:
      outMsg(out, "%s_", sep, var);
      return ",";
    case bLk:
      outMsg(out, "%s%s", sep, var);
      return ",";
    default:
      check(False, "Cannot generate argument code");
  }
}

// Construct the HWM calculator
static void genStackHwm(ioPo out, char *mnem, int op, int delta, opAndSpec A1, opAndSpec A2, char *cmt) {
  char *sep = "(";

  outMsg(out, "stackHwm([i%s", capitalize(mnem));

  sep = genHWmArg(out, sep, A1, "V");
  sep = genHWmArg(out, sep, A2, "W");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, "|Ins],CH0,H0,Hwm) :-\n");

  integer HVar = 0;
  integer CVar = 0;
  if (delta != 0) {
    outMsg(out, "  CH%d is CH%d%s%d,\n", CVar + 1, CVar, (delta > 0 ? "+" : ""), delta);
    outMsg(out, "  (CH%d>H%d -> H%d = CH%d ; H%d = H%d),\n", CVar + 1, HVar, HVar + 1, CVar + 1, HVar + 1, HVar);
    CVar++;
    HVar++;
  }
  genHwmOp(out, A1, "V", &CVar, &HVar);
  genHwmOp(out, A2, "W", &CVar, &HVar);

  outMsg(out, "  stackHwm(Ins,CH%d,H%d,Hwm).\n", CVar, HVar);
}

static char *genLocalHWmArg(ioPo out, char *sep, opAndSpec A, char *var) {
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
    case lVl:
      outMsg(out, "%s_", sep, var);
      return ",";
    case lcl:
    case lcs:
    case bLk:
      outMsg(out, "%s%s", sep, var);
      return ",";
    default:
      check(False, "Cannot generate argument code");
  }
}

static void genLocalHwmOp(ioPo out, opAndSpec A, char *var, integer *C, integer *H) {
  switch (A) {
    case nOp:
    case tOs:
    case i32:
    case art:
    case arg:
    case glb:
    case Es:
    case lVl:
    case lit:
    case sym:
    case tPe:
      break;
    case lcl:
    case lcs:
      outMsg(out, "  countLocal(%s,C%d,C%d,H%d,H%d),\n", var, *C, (*C) + 1, *H, (*H) + 1);
      (*C)++;
      (*H)++;
      break;
    case bLk: {
      outMsg(out, "  localHwm(%s,C%d,C%d,H%d,H%d),\n", var, *C, (*C)+1, *H, (*H) + 1);
      (*H)++;
      (*C)++;
      break;
    }
    default:
      check(False, "Cannot generate localHWM");
      exit(1);
  }
}

// Construct the localHWM calculator
static void genLocalHwm(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  char *sep = "(";

  outMsg(out, "localHwm([i%s", capitalize(mnem));

  sep = genLocalHWmArg(out, sep, A1, "V");
  sep = genLocalHWmArg(out, sep, A2, "W");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, "|Ins],C0,Cx,H0,Hwm) :-\n");

  integer HVar = 0;
  integer CVar = 0;

  genLocalHwmOp(out, A1, "V", &CVar, &HVar);
  genLocalHwmOp(out, A2, "W", &CVar, &HVar);

  outMsg(out, "  localHwm(Ins,C%d,Cx,H%d,Hwm).\n", CVar, HVar);
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
      break;
    case lcl:
    case lcs:
      outMsg(out, "  %s=ss(%s),\n", Vtxt, vn);
      break;

    case glb:
      outMsg(out, "  %s=ss(%s),!,\n", Vtxt, vn);
      break;
    case i32:
    case art:
    case arg:
      outMsg(out, "  %s=ix(%s),\n", Vtxt, vn);
      break;
    case lVl:
    case Es:
      outMsg(out, "  %s=ss(%s),!,\n", Vtxt, vn);
      break;
    case bLk:
      outMsg(out, "  blockPc(Pc,SPc),\n", resIn->pcV);
      outMsg(out, "  showMnems(%s, SPc, Ms),\n", vn);
      outMsg(out, "  pcSpace(SPc,Dp),\n");
      outMsg(out, "  %s = sq([nl(Dp),iv(nl(Dp), Ms)]),\n", Vtxt);
      break;
  }
}

static void showPrologIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "showMnem(i%s", capitalize(mnem));

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
    case tOs:
      break;
    default:
      sep1 = ", ss(\" \"), ";
      V1 = "UU";
  }

  switch (A2) {
    case nOp:
    case tOs:
      break;
    default:
      if (A1 != tOs)
        sep2 = ", ss(\",\"), ";
      else
        sep2 = ", ss(\" \"), ";
      V2 = "VV";
      break;
  }

  outMsg(out, ",Pc,sq([PcDx,ss(\": \"),ss(\"%P\")%s%s%s%s])) :- !,\n", mnem, sep1, V1, sep2, V2);
  outMsg(out, "  showPc(Pc,PcDx),\n");

  OpRes res1 = {.pcV=0};

  showOperand(out, A1, "U", "UU", &res1);
  showOperand(out, A2, "V", "VV", &res1);

  outMsg(out, "  true.\n", res1.pcV);
}

static integer opHash(char *mnem, int op) {
  return hash61(strhash(mnem) * 37 + op);
}

integer staropHash() {
  integer hash = 0;

#undef instruction
#define instruction(M, A1, A2, Dl, _, Cmt) hash = hash61(hash*39+opHash(#M,M));

#include "instructions.h"

  return hash;
}
