#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "opcodes.h"
#include "hash.h"
#include "unistr.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include <assert.h>
#include "formexts.h"

/* Generate a Star module, that knows how to assemble a program */

char *prefix = "star.comp.assem";
char *templateFn = "assem.star.plate";
char date[MAXLINE] = "";

static integer staropHash();

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
static void genStarMnem(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt);
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
#define instruction(M, A1, A2, Dl, cmt) genStarMnem(O_IO(mnemBuff),#M,M,A1,A2,Dl,cmt);

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

    static char hashBuff[64];
    strMsg(hashBuff,NumberOf(hashBuff),"%ld",staropHash());
    hashPut(vars, "Hash", hashBuff);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    flushOut();
    closeIo(out);
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
    case glb:
    case Es:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case tPe:
    case bLk:
    case lVl:
      outMsg(out, "%s%s", sep, V);
      return ",";
    case off:
      outMsg(out, "%s.al(%s)", sep, V);
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

static void genStarMnem(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt) {
  char *sep = "(";

  outMsg(out, "  mnem([.i%s", mnem);

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
          outMsg(out, "=> mnem(Ins,Code++[.intgr(%d)],Lbls,Lts,Lns,Lcs,Pc+1,MxLcl,Ends).\n", op);
          return;
        case off:
          outMsg(out,
                 "where Tgt ?= Lbls[V] => mnem(Ins,Code++[.intgr(%d),.intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> mnem(Ins,Code++[.intgr(%d),.intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          return;
        default:
          check(False, "invalid second operand");
      }
    case lit:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) => mnem(Ins,Code++[.intgr(%d),.intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
                 op);
          return;
        case off:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= Lbls[V] => mnem(Ins,Code++[.intgr(%d),.intgr(LtNo),.intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
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
                 "where (Lt1,LtNo) .= findLit(Lts,.strg(U::string)) => mnem(Ins,Code++[.intgr(%d),.intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
                 op);
          return;
        case off:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,.strg(U::string)) && Tgt ?= Lbls[V] => mnem(Ins,Code++[.intgr(%d),.intgr(LtNo),.intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case sym: {                            // symbol
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,.symb(U))";

      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => mnem(Ins,Code++[.intgr(%d),.intgr(LtNo)],Lbls,Lt1,Lns,Lcs,Pc+3,MxLcl,Ends).\n", cond,
                 op);
          return;
        case off:
          outMsg(out,
                 "%s && Tgt ?= Lbls[V] => mnem(Ins,Code++[.intgr(%d),.intgr(LtNo),.intgr(Tgt-Pc-5)],Lbls,Lt1,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
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
          outMsg(out, "=> mnem(Ins,Code++[.intgr(%d),.intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          return;
        case off:
          outMsg(out,
                 "where Tgt ?= Lbls[V] =>  mnem(Ins,Code++[.intgr(%d),.intgr(U),.intgr(Tgt-Pc-5)],Lbls,Lts,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    case lcl:
    case lcs:
      check(B == nOp || B == tOs, "second operand not nOp");
      outMsg(out, "=> mnem(Ins,Code++[.intgr(%d),.intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,max(U,MxLcl),Ends).\n", op);
      return;
    case glb:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> mnem(Ins,Code++[.intgr(%d),.strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          return;
        case off:
          outMsg(out,
                 "where Tgt ?= Lbls[V] => mnem(Ins,Code++[.intgr(%d),.strg(U),.intgr(Tgt-Pc-5)],Lbls,Lts,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case Es: {                              // escape name
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> mnem(Ins,Code++[.intgr(%d),.strg(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
          break;
        case off:
          outMsg(out,
                 "&& Tgt ?= Lbls[V] => mnem(Ins,Code++[.intgr(%d),.strg(U),.intgr(Tgt-Pc-5)],Lbls,Lts,Lns,Lcs,Pc+5,MxLcl,Ends).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
      break;
    }
    case off:                            // program counter relative offset
      check(B == nOp || B == tOs, "second operand not nOp");
      outMsg(out,
             "where Tgt ?= Lbls[U] => mnem(Ins,Code++[.intgr(%d),.intgr(Tgt-Pc-3)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n",
             op);
      return;
    case bLk: {                           // A nested block of instructions
      outMsg(out,
             "where (Blk,Lts1,Lns1,Lcs1,Pc1,MxLcl1) .= mnem(U,[],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends) => mnem(Ins,Code++[.intgr(%d),mkTpl(Blk::cons[data])],Lbls,Lts1,Lns1,Lcs1,Pc1,MxLcl1,Ends).\n",
             op);
      return;
      case lVl:{                          // Break out of a nesting sequence of blocks
        switch (B) {
          case nOp:
          case tOs:
            outMsg(out, "=> mnem(Ins,Code++[.intgr(%d),.intgr(U)],Lbls,Lts,Lns,Lcs,Pc+3,MxLcl,Ends).\n", op);
            return;
          default:
            check(False, "Cannot generate instruction code");
        }
      }
    }
    default:
      check(False, "invalid first operand");
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
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case Es:
    case glb:
    case off:
    case bLk:
    case lVl:
      sz += 2;
      break;
  }
  switch (A2) {
    default:
      break;
    case tPe:
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
    case lVl:
      sz += 2;
      break;
  }
  return sz;
}

void starPc(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  genLblTbl([.i%s", mnem);

  char *sep = genArg(out, "(", A1, "A");
  sep = genArg(out, sep, A2, "B");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, ",..Ins],Pc,Lbls) ");

  int increment = insSize(op, A1, A2);

  outMsg(out, " => genLblTbl(Ins,Pc+%d,Lbls).\n", increment);
}

static char *opAndTp(opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      return Null;
    case lit:
      return "data";
    case sym:
      return "termLbl";
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case lVl:
      return "integer";
    case off:
      return "assemLbl";
    case Es:
    case glb:
      return "string";
    case tPe:
      return "ltipe";
    case bLk:
      return "cons[assemOp]";
    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
}

void insOp(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "    .i%s", mnem);
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
    case tPe:
    case lit:
    case sym:
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
    case glb:
    case Es:
    case bLk:
    case lVl:
      outMsg(out, " $(%s)", Nm);
      return True;
    case off:
      outMsg(out, " #(%s):", Nm);
      return True;
  }
}

static void showStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  showMnem([.i%s", mnem);

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

// Create an entry of the form:
// insEffect([.ins(O),..Ins],[_,..Stk]) => [Tp,..Stk] -- Comment

static void starInsEffect(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  insEffect([.i%s", mnem);

  char *sep = genArg(out, "(", A1, "U");
  sep = genArg(out, sep, A2, "V");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, ",..Ins],) => ");

  outMsg(out, "\"$(Pc)\\: %P", mnem);

  genDisp(out, A1, "U");
  genDisp(out, A2, "V");
  outMsg(out, "\\n\" ++ showMnem(Ins,Pc+%d).\n", insSize(op, A1, A2));
}

static integer opHash(char *mnem,int op){
  return hash61(strhash(mnem)*37+op);
}

integer staropHash(){
  integer hash = 0;

#undef instruction
#define instruction(M, A1, A2, Dl, Cmt) hash = hash61(hash*39+opHash(#M,M));

#include "instructions.h"

  return hash;
}

