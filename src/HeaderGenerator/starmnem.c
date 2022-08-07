#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "opcodes.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include <stringBuffer.h>
#include <assert.h>
#include "formexts.h"

/* Generate a Star module, that knows how to assemble a program */

char *prefix = "star.comp.assem";
char *templateFn = "assem.star.plate";
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
static void genStarIns(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt);
static void starPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static void showStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static void insOp(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);

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

#undef instruction
#define instruction(M, A1, A2, Dl, Cmt) insOp(O_IO(typeBuff),#M,M,A1,A2,Cmt);

#include "instructions.h"

    integer tpLen;
    char *typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

    // Set up the assembler proper
    strBufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genStarIns(O_IO(mnemBuff),#M,M,A1,A2,Dl,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the label generator

    strBufferPo lblBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) starPc(O_IO(lblBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer lblLen;
    char *lblCode = getTextFromBuffer(lblBuff, &lblLen);
    hashPut(vars, "Lbls", lblCode);

    // Set up the display code
    strBufferPo showBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) showStarIns(O_IO(showBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(showBuff, &showLen);
    hashPut(vars, "Show", showCode);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    flushOut();
    closeFile(out);
    exit(0);
  }
}

static char *genArg(ioPo out, char *sep, opAndSpec A, char *V) {
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
    case cDe:
    case tPe:
      outMsg(out, "%s%s", sep, V);
      return ",";
    case off:
      outMsg(out, "%sal(%s)", sep, V);
      return ",";
    default:
      assert(False);
  }
}

char *tail = "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Lns,Lnx,Pc1,M).\n";

static char *capitalize(char *str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char) ('A' + (buffer[0] - 'a'));
  }
  return buffer;
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

static void genStarIns(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt) {
  char *sep = "(";

  outMsg(out, "  mnem([%si%s", dot(A), mnem);

  sep = genArg(out, sep, A, "U");

  sep = genArg(out, sep, B, "V");

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  outMsg(out, "%s,..Ins],Code,Lbls,Lts,Lns,Lcs,Pc,MxLcl,Ends) ", sep);

  switch (A) {
    case nOp:                             // No operand
    case tOs:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> mnem(Ins,Code++[intgr(%d)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).\n", op);
          return;
        case off:
          outMsg(out,
                 "where Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(%d),intgr(Tgt-Pc-3)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> mnem(Ins,Code++[intgr(%d),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          return;
        default:
          check(False, "invalid second operand");
      }
    case lne:
    case lit:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[intgr(%d),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
                 op);
          return;
        case off:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(%d),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 op);
          return;
        case cDe:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) && (BlkCde,Lt2,Lns1,Lcs1,Pc1,Lc1) .= mnem(V,[],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl) => mnem(Ins,Code++[intgr(%d),intgr(LtNo),intgr(Pc1-Pc-5),..BlkCde],Lbls,Lt2,Lns1,Lcs1,Pc1,Lc1,Ends).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    case tPe:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,strg(U::string)) => mnem(Ins,Code++[intgr(%d),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
                 op);
          return;
        case off:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,strg(U::string)) && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(%d),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case sym: {                            // symbol
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,symb(U))";

      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => mnem(Ins,Code++[intgr(%d),intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n", cond,
                 op);
          return;
        case off:
          outMsg(out,
                 "%s && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(%d),intgr(LtNo),intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }
    case i32:
    case art:
    case arg:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> mnem(Ins,Code++[intgr(%d),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          return;
        case off:
          outMsg(out,
                 "where Tgt ^= Lbls[V] =>  mnem(Ins,Code++[intgr(%d),intgr(U),intgr(Tgt-Pc-5)],Lbls,Lts,Lns,Lcs,Pc+5,MxLcl,Ends).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    case lcl:
    case lcs:
      check(B == nOp, "second operand not nOp");
      outMsg(out, "=> mnem(Ins,Code++[intgr(%d),intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).\n", op);
      return;
    case glb:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> mnem(Ins,Code++[intgr(%d),strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          return;
        case off:
          outMsg(out, "where Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(%d),strg(U),intgr(Tgt-Pc-5)],Lbls,Lts,Lns,Lcs,Pc+5,MxLcl,Ends).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case Es: {                              // escape code (0..65535)
      char *cond = "where Cd^=isEscape(U)";

      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => mnem(Ins,Code++[intgr(%d),intgr(Cd)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", cond, op);
          break;
        case off:
          outMsg(out,
                 "%s && Tgt ^= Lbls[V] => mnem(Ins,Code++[intgr(%d),intgr(Cd),intgr(Tgt-Pc-5)],Lbls,Lts,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
      break;
    }
    case off:                            // program counter relative offset
      check(B == nOp, "second operand not nOp");
      outMsg(out,
             "where Tgt ^= Lbls[U] => mnem(Ins,Code++[intgr(%d),intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
             op);
      return;
    default:
      check(False, "invalid second operand");
  }
}

static integer insSize(OpCode op, opAndSpec A1, opAndSpec A2) {
  integer sz = 1;

  switch (A1) {
    default:
      break;
    case tPe:
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
    case cDe:
      sz += 2;
      break;
  }
  switch (A2) {
    default:
      break;
    case tPe:
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
    case cDe:
      sz += 2;
      break;
  }
  return sz;
}

void starPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  genLblTbl([%si%s", dot(A1), mnem);

  char *sep = genArg(out, "(", A1, "A");
  sep = genArg(out, sep, A2, "B");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, ",..Ins],Pc,Lbls) ");

  int increment = insSize(op, A1, A2);

  if (A1 == cDe) {
    if (A2 == cDe) {
      outMsg(out,
             "where (Pc1,Lb1).=genLblTbl(A,Pc+%d,Lbls) && (Pc2,Lb2) .= genLblTbl(B,Pc1,Lb1) => genLblTbl(Ins,Pc2,Lb2).\n",
             increment);
    } else
      outMsg(out, "where (Pc1,Lb1).=genLblTbl(A,Pc+%d,Lbls) => genLblTbl(Ins,Pc1,Lb1).\n", increment);
  } else if (A2 == cDe) {
    outMsg(out, "where (Pc1,Lb1).=genLblTbl(B,Pc+%d,Lbls) => genLblTbl(Ins,Pc1,Lb1).\n", increment);
  } else
    outMsg(out, " => genLblTbl(Ins,Pc+%d,Lbls).\n", increment);
}

static char *opAndTp(opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      return Null;
    case lit:
    case lne:
      return "data";
    case sym:
      return "termLbl";
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
      return "integer";
    case off:
      return "assemLbl";
    case Es:
    case glb:
      return "string";
    case cDe:
      return "cons[assemOp]";
    case tPe:
      return "ltipe";
    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
}

void insOp(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  char *dot = ((A1 == tOs || A1 == nOp) && (A2 == nOp) ? "." : "");
  outMsg(out, "    %si%s", dot, mnem);
  char *T1 = opAndTp(A1);
  char *T2 = opAndTp(A2);

  if (T1 != Null) {
    outMsg(out, "(%s", T1);
    if (T2 != Null)
      outMsg(out, ",%s)", T2);
    else
      outMsg(out, ")");
  } else if (T2 != Null)
    outMsg(out, "(%s)", T2);

  outMsg(out, " |\n");
}

static logical genDisp(ioPo out, opAndSpec A, char *Nm) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
    default:
      return False;
    case lne:
    case tPe:
    case lit:
    case sym:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case glb:
    case off:
    case cDe:
    case Es:
      outMsg(out, " $(%s)", Nm);
      return True;
  }
}

static void showStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  showMnem([%si%s", dot(A1), mnem);

  char *sep = genArg(out, "(", A1, "U");
  sep = genArg(out, sep, A2, "V");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, ",..Ins],Pc) => ");

  outMsg(out, "\"$(Pc)\\: %P", mnem);

  genDisp(out, A1, "U");
  genDisp(out, A2, "V");
  outMsg(out, "\\n\" ++ showMnem(Ins,Pc+%d).\n", insSize(op, A1, A2));
}
