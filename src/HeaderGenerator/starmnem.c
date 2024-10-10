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
#define instruction(M, A1, A2, Dl, Tp, Cmt) insOp(O_IO(typeBuff),#M,M,A1,A2,Cmt);

#include "instructions.h"

    integer tpLen;
    char *typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

    // Set up the assembler proper
    strBufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, Tp, cmt) genStarMnem(O_IO(mnemBuff),#M,M,A1,A2,Dl,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the display code
    strBufferPo showBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, Tp, cmt) showStarIns(O_IO(showBuff),#M,M,A1,A2,cmt);

#include "instructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(showBuff, &showLen);
    hashPut(vars, "Show", showCode);

    static char hashBuff[64];
    strMsg(hashBuff, NumberOf(hashBuff), "%ld", staropHash());
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
    case lNe:
      outMsg(out, "%s%s", sep, V);
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

  outMsg(out, "  mnem(.i%s", mnem);

  sep = genArg(out, sep, A, "U");

  sep = genArg(out, sep, B, "V");

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  outMsg(out, "%s,Lbls,Lts,Lcs) ", sep);

  switch (A) {
    case nOp:                             // No operand
    case tOs:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d)],Lts,Lcs).\n", op);
          return;
        case bLk: {                           // A nested block of instructions
          outMsg(out,
                 "where (Blk,Lts1,Lcs1) .= assemBlock(U,[],[\"\",..Lbls],Lts,Lcs) => ([.intgr(%d),mkTpl(Blk::cons[data])],Lts1,Lcs1).\n",
                 op);
          return;
        }
        case lVl:
          outMsg(out,
                 "where Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(Tgt)],Lts,Lcs).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> ([.intgr(%d),.intgr(U)],Lbls,Lts,Lcs).\n", op);
          return;
        default:
          check(False, "invalid second operand");
      }
    case lit:
    case lNe:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(%d),.intgr(LtNo)],Lt1,Lcs).\n",
                 op);
          return;
        case lVl:
          outMsg(out,
                 "where (Lt1,LtNo) .= findLit(Lts,U) && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Lt1,Lcs).\n",
                 op);
          return;
        case lcl:
          outMsg(out, "where (Lt1,LtNo) .= findLit(Lts,U) => ([.intgr(%d),.intgr(LtNo),.intgr(V)],Lt1,Lcs).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    case tPe: {
      char *findLit = "(Lt1,LtNo) .= findLit(Lts,.strg(U::string))";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where %s => ([.intgr(%d),.intgr(LtNo)],Lt1,Lcs).\n", findLit, op);
          return;
        case bLk: {                             // A nested block of instructions
          outMsg(out,
                 "where %s && (Blk,Lts1,Lcs1) .= assemBlock(V,[],[\"\",..Lbls],Lt1,Lcs) => ([.intgr(%d),.intgr(LtNo),mkTpl(Blk::cons[data])],Lts1,Lcs1).\n",
                 findLit, op);
          return;
        }
        case lVl:
          outMsg(out,
                 "where %s) && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Lt1,Lcs).\n",
                 findLit, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }

    case sym: {                            // symbol
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,.symb(U))";

      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Lt1,Lcs).\n", cond, op);
          return;
        case lVl:
          outMsg(out,
                 "%s && Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Lvl)],Lt1,Lcs).\n",
                 cond, op);
          return;
        case lcl:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo),.intgr(V)],Lt1,Lcs).\n", cond, op);
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
          outMsg(out, "=> ([.intgr(%d),.intgr(U)],Lts,Lcs).\n", op);
          return;
        case lVl:
          outMsg(out,
                 "where Lvl ?= findLevel(Lbls,V) =>  ([.intgr(%d),.intgr(U),.intgr(Lvl)],Lts,Lcs).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    case lcl:
    case lcs:
      check(B == nOp || B == tOs, "second operand not nOp");
      outMsg(out, "=> ([.intgr(%d),.intgr(U)],Lts,Lcs).\n", op);
      return;
    case glb:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d),.strg(U)],Lts,Lcs).\n", op);
          return;
        case lVl:
          outMsg(out,
                 "where Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.strg(U),.intgr(Lvl)],Lts,Lcs).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case Es: {                              // escape name
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d),.strg(U)],Lts,Lcs).\n", op);
          break;
        case lVl:
          outMsg(out,
                 "&& Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.strg(U),.intgr(Lvl)],Lts,Lcs).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
      break;
    }
    case bLk: {                           // A nested block of instructions
      outMsg(out,
             "where (Blk,Lts1,Lcs1) .= assemBlock(U,[],[\"\",..Lbls],Lts,Lcs) => ([.intgr(%d),mkTpl(Blk::cons[data])],Lts1,Lcs1).\n",
             op);
      return;
    }
    case lVl: {                          // Break out of a nesting sequence of blocks
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out,
                 "where Lvl ?= findLevel(Lbls,U) => ([.intgr(%d),.intgr(Lvl)],Lts,Lcs).\n",
                 op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }
    default:
      check(False, "invalid first operand");
  }
}

static char *opAndTp(opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
    case tOs:
      return Null;
    case lit:
    case lNe:
      return "data";
    case sym:
      return "termLbl";
    case i32:
    case art:
    case arg:
    case lcl:
    case lcs:
      return "integer";
    case lVl:
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
    case lNe:
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
  }
}

static void showStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  showIns(.i%s", mnem);

  char *sep = genArg(out, "(", A1, "U");
  sep = genArg(out, sep, A2, "V");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, ",Pc) => \"%P", mnem);

  genDisp(out, A1, "U");
  genDisp(out, A2, "V");
  outMsg(out, "\".\n");
}

static integer opHash(char *mnem, int op) {
  return hash61(strhash(mnem) * 37 + op);
}

integer staropHash() {
  integer hash = 0;

#undef instruction
#define instruction(M, A1, A2, Dl, Tp, Cmt) hash = hash61(hash*39+opHash(#M,M));

#include "instructions.h"

  return hash;
}

