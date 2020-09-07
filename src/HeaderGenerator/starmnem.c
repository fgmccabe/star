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
static void genStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, int delta, char *cmt);
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

    // Set up the assembler proper
    bufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genStarIns(O_IO(mnemBuff),#M,M,A1,A2,Dl,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the label generator

    bufferPo lblBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) starPc(O_IO(lblBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer lblLen;
    char *lblCode = getTextFromBuffer(lblBuff, &lblLen);
    hashPut(vars, "Lbls", lblCode);

    // Set up the display code
    bufferPo showBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) showStarIns(O_IO(showBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(showBuff, &showLen);
    hashPut(vars, "Show", showCode);

    // Set up the opcodes for the type definition
    bufferPo typeBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, Cmt) insOp(O_IO(typeBuff),#M,M,A1,A2,Cmt);

#include "instructions.h"

    integer tpLen;
    char *typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

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
    case ix32:
    case art:
    case arg:
    case lcl:
    case lcs:
      outMsg(out, "%s%s", sep, V);
      return ",";
    case off:
      outMsg(out, "%sal(%s)", sep, V);
      return ",";
    default:
      printf("Problem in generating opcode type\n");
      exit(11);
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

  outMsg(out, "%s,..Ins],Lbls,Lts,Lns,Lcs,Pc,MxLcl,Code) ", sep);

  switch (A) {
    case nOp:                             // No operand
    case tOs:
      check(B == nOp, "second operand not nOp");
      outMsg(out, "=> mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,[intgr(%d),..Code]).\n", op);
      break;
    case lne:
    case lit:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(%d),..Code]).\n",
                 op);
          return;
        case off:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,[intgr(Tgt-Pc-5),intgr(LtNo),intgr(%d),..Code]).\n",
                 op);
          return;
        default:
          outMsg(out, "Cannot generate instruction code\n");
          exit(1);
      }

    case sym:                            // symbol
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,symb(U)) => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,[intgr(LtNo),intgr(%d),..Code]).\n",
                 op);
          return;
        case off:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,symb(U)) && Tgt ^= Lbls[V] => mnem(Ins,Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,[intgr(Tgt-Pc-5),intgr(LtNo),intgr(%d),..Code]).\n",
                 op);
          return;
        default:
          outMsg(out, "Cannot generate instruction code\n");
          exit(1);
      }
    case ix32:
    case art:
    case arg:
      check(B == nOp, "second operand not nOp");
      outMsg(out, "=> mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(U),intgr(%d),..Code]).\n", op);
      break;
    case lcl:
    case lcs:
      check(B == nOp, "second operand not nOp");
      outMsg(out, "=> mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),[intgr(U),intgr(%d),..Code]).\n", op);
      break;
    case glb:
    case Es:                              // escape code (0..65535)
      check(B == nOp, "second operand not nOp");
      outMsg(out, "=> mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[strg(U),intgr(%d),..Code]).\n", op);
      break;
    case off:                            // program counter relative offset
      check(B == nOp, "second operand not nOp");
      outMsg(out, "where Tgt ^= Lbls[U] => mnem(Ins,Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,[intgr(Tgt-Pc-3),intgr(%d),..Code]).\n",
             op);
      break;
    default:
      outMsg(out, "Unknown instruction type code\n");
      exit(1);
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
    case ix32:
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
    case ix32:
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

void starPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  genLblTbl([%si%s", dot(A1), mnem);

  char *part1 = Null;
  char *part2 = Null;

  switch (A1) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case sym:
    case lne:
    case ix32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      part1 = "(_";
      break;

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }

  switch (A2) {
    case nOp:                             // No operand
    case tOs:
      break;
    case lit:
    case sym:
    case lne:
    case ix32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      part2 = ",_";
      break;

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }

  if (part1 != Null) {
    outStr(out, part1);
    if (part2 != Null) {
      outStr(out, part2);
    }
    outStr(out, ")");
  }

  outMsg(out, ",..Ins],Pc,Lbls) => ");

  int increment = 1;

  switch (A1) {
    default:
      break;
    case lit:
    case sym:
    case lne:
    case ix32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      increment = 3;
      break;
  }
  switch (A2) {
    default:
      break;
    case lit:
    case sym:
    case lne:
    case ix32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
      increment += 2;
      break;
  }
  outMsg(out, "genLblTbl(Ins,Pc+%d,Lbls).\n", increment);
}

static char *opAndTp(opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      return Null;
    case lit:
    case lne:
      return "term";
    case sym:
      return "termLbl";
    case ix32:
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
    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
}

void insOp(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "    %si%s", dot(A1), mnem);
  char *T1 = opAndTp(A1);
  char *T2 = opAndTp(A2);

  if (T1 != Null) {
    outMsg(out, "(%s", T1);
    if (T2 != Null)
      outMsg(out, ",%s)", T2);
    else
      outMsg(out, ")");
  }

  outMsg(out, " |\n");
}

static logical genDisp(ioPo out, opAndSpec A, char *Nm) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
    default:
      return False;
    case lne:
    case lit:
    case sym:
    case ix32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case glb:
    case off:
      outMsg(out, ",ss(\" \"),disp(%s)", Nm);
      return True;
    case Es:
      outMsg(out, ",ss(\" \"),ss(%s)", Nm);
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

  outMsg(out, "single(ssSeq([disp(Pc),ss(\":\"),ss(\"%P\")", mnem);

  genDisp(out, A1,"U");
  genDisp(out,A2,"V");
  outMsg(out, ",ss(\"\\n\")])) ++ showMnem(Ins,Pc+%d).\n",insSize(op,A1,A2));
}
