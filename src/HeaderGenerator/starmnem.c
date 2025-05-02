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

static void genStarMnem(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt);
static void showStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);
static void genStackHwm(ioPo out, char *mnem, int op, int delta, opAndSpec A1, opAndSpec A2, char *cmt);
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

    {
      // Set up the hwm calculator
      strBufferPo hwmBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genStackHwm(O_IO(hwmBuff),#M,M,Dl,A1,A2,cmt);

#include "instructions.h"

      integer insLen;
      char *allCode = getTextFromBuffer(hwmBuff, &insLen);
      hashPut(vars, "stackHwm", allCode);
    }

    // Set up the assembler proper
    strBufferPo mnemBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) genStarMnem(O_IO(mnemBuff),#M,M,A1,A2,Dl,cmt);

#include "instructions.h"

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Mnem", allCode);

    // Set up the display code
    strBufferPo showBuff = newStringBuffer();

#undef instruction
#define instruction(M, A1, A2, Dl, cmt) showStarIns(O_IO(showBuff),#M,M,A1,A2,cmt);

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
      outMsg(out, "%s%s", sep, V);
      return ",";
    default:
      assert(False);
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

static void genStarMnem(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt) {
  char *sep = "(";

  outMsg(out, "  mnem(.i%s", capitalize(mnem));

  sep = genArg(out, sep, A, "U");

  sep = genArg(out, sep, B, "V");

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  outMsg(out, "%s,Pc,Lbls,Lts,Lcs,Lns) ", sep);

  switch (A) {
    case nOp:                             // No operand
    case tOs:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d)],Pc+1,Lts,Lns).\n", op);
          return;
        case bLk: {                           // A nested block of instructions
          outMsg(out,
                 "where (Blk,Pc1,Lts1,Lns1) .= assemBlock(U,[],Pc+1,[.none,..Lbls],Lts,Lcs,Lns) => ([.intgr(%d),mkTpl(Blk::cons[data])],Pc1,Lts1,Lns1).\n",
                 op);
          return;
        }
        case lVl:
          outMsg(out,
                 "where Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(Tgt)],Pc+1,Lts,Lns).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> ([.intgr(%d),.intgr(U)],Pc+1,Lbls,Lns).\n", op);
          return;
        default:
          check(False, "invalid second operand");
      }
    case sym:{
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,.symb(U))";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        case lVl:
          outMsg(out, "%s && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).\n", cond,
                 op);
          return;
        case lcl:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo),.intgr(V)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }
    case lit: {
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,U)";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        case lVl:
          outMsg(out, "%s && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).\n", cond,
                 op);
          return;
        case lcl:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo),.intgr(V)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }
    case tPe: {
      char *lit = "where (Lt1,LtNo) .= findLit(Lts,.strg(U::string))";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Pc+1,Lt1,Lns).\n", lit, op);
          return;
        case bLk: {                             // A nested block of instructions
          outMsg(out, "%s && (Blk,Pc1,Lts1,Lns1) .= assemBlock(V,[],Pc+1,[.none,..Lbls],Lt1,Lcs,Lns) =>\n"
                      "    ([.intgr(%d),.intgr(LtNo),mkTpl(Blk::cons[data])],Pc1,Lts1,Lns1).\n",
                 lit, op);
          return;
        }
        case lVl:
          outMsg(out,
                 "%s) && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).\n",
                 lit, op);
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
          outMsg(out, "=> ([.intgr(%d),.intgr(U)],Pc+1,Lts,Lns).\n", op);
          return;
        case lVl:
          outMsg(out,
                 "where Lvl ?= findLevel(Lbls,V) =>  ([.intgr(%d),.intgr(U),.intgr(Lvl)],Pc+1,Lts,Lns).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> ([.intgr(%d),.intgr(U),.intgr(V)],Pc+1,Lts,Lns).\n", op);
          return;
        case bLk: {                             // A nested block of instructions
          outMsg(out, "where (Blk,Pc1,Lts1,Lns1) .= assemBlock(V,[],Pc+1,[.none,..Lbls],Lt1,Lcs,Lns) =>\n"
                      "    ([.intgr(%d),.intgr(U),mkTpl(Blk::cons[data])],Pc1,Lts1,Lns1).\n",op);
          return;
        }
        default:
          check(False, "Cannot generate instruction code");
      }
    case lcl:
    case lcs: {
      char *cond = "Off ?= findLocal(U,Lcs)";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "where %s => ([.intgr(%d),.intgr(Off)],Pc+1,Lts,Lns).\n", cond, op);
          return;
        case lit:
          outMsg(out,
                 "where %s && (Lt1,LtNo) .= findLit(Lts,V) => ([.intgr(%d),.intgr(Off),.intgr(LtNo)],Pc+1,Lt1,Lns).\n",
                 cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code: invalid second operand");
      }
    }
    case glb:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d),.strg(U)],Pc+1,Lts,Lns).\n", op);
          return;
        case lVl:
          outMsg(out, "where Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.strg(U),.intgr(Lvl)],Pc+1,Lts,Lns).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case Es: {                              // escape name
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d),.strg(U)],Pc+1,Lts,Lns).\n", op);
          break;
        case lVl:
          outMsg(out, "where Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.strg(U),.intgr(Lvl)],Pc+1,Lts,Lns).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
      break;
    }
    case bLk: {                           // A nested block of instructions
      check(False, "block not allowed as first argument");
    }
    case lVl: {                          // Break out of a nesting sequence of blocks
      check(False, "invalid first operand");
    }
  }
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
      return "integer";
    case lcl:
    case lcs:
      return "string";
    case lVl:
      return "assemLbl";
    case Es:
    case glb:
      return "string";
    case tPe:
      return "ltipe";
    case bLk:
      return "multi[assemOp]";
    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
}

void insOp(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "    | .i%s", capitalize(mnem));
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

  outMsg(out, "\n");
}


// Generate the disassembler




static void genDiss(ioPo out, char *mnem, int op, opAndSpec A, opAndSpec B, int delta, char *cmt) {
  char *lp = "";
  char *sep = "";
  char *rp = "";

  outMsg(out, "  diss([.intgr(%d),", op);

  sep = genArg(out, lp, A, "U");

  sep = genArg(out, sep, B, "V");

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  outMsg(out, "%s,..Code]) => [.i", sep,capitalize(mnem));

  switch (A) {
    case nOp:                             // No operand
    case tOs:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d)],Pc+1,Lts,Lns).\n", op);
          return;
        case bLk: {                           // A nested block of instructions
          outMsg(out,
                 "where (Blk,Pc1,Lts1,Lns1) .= assemBlock(U,[],Pc+1,[.none,..Lbls],Lts,Lcs,Lns) => ([.intgr(%d),mkTpl(Blk::cons[data])],Pc1,Lts1,Lns1).\n",
                 op);
          return;
        }
        case lVl:
          outMsg(out,
                 "where Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(Tgt)],Pc+1,Lts,Lns).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> ([.intgr(%d),.intgr(U)],Pc+1,Lbls,Lns).\n", op);
          return;
        default:
          check(False, "invalid second operand");
      }
    case sym:{
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,.symb(U))";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        case lVl:
          outMsg(out, "%s && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).\n", cond,
                 op);
          return;
        case lcl:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo),.intgr(V)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }
    case lit: {
      char *cond = "where (Lt1,LtNo) .= findLit(Lts,U)";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        case lVl:
          outMsg(out, "%s && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).\n", cond,
                 op);
          return;
        case lcl:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo),.intgr(V)],Pc+1,Lt1,Lns).\n", cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    }
    case tPe: {
      char *lit = "where (Lt1,LtNo) .= findLit(Lts,.strg(U::string))";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "%s => ([.intgr(%d),.intgr(LtNo)],Pc+1,Lt1,Lns).\n", lit, op);
          return;
        case bLk: {                             // A nested block of instructions
          outMsg(out, "%s && (Blk,Pc1,Lts1,Lns1) .= assemBlock(V,[],Pc+1,[.none,..Lbls],Lt1,Lcs,Lns) =>\n"
                      "    ([.intgr(%d),.intgr(LtNo),mkTpl(Blk::cons[data])],Pc1,Lts1,Lns1).\n",
                 lit, op);
          return;
        }
        case lVl:
          outMsg(out,
                 "%s) && Tgt ?= findLevel(Lbls,V) => ([.intgr(%d),.intgr(LtNo),.intgr(Tgt)],Pc+1,Lt1,Lns).\n",
                 lit, op);
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
          outMsg(out, "=> ([.intgr(%d),.intgr(U)],Pc+1,Lts,Lns).\n", op);
          return;
        case lVl:
          outMsg(out,
                 "where Lvl ?= findLevel(Lbls,V) =>  ([.intgr(%d),.intgr(U),.intgr(Lvl)],Pc+1,Lts,Lns).\n",
                 op);
          return;
        case i32:
          outMsg(out, "=> ([.intgr(%d),.intgr(U),.intgr(V)],Pc+1,Lts,Lns).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
    case lcl:
    case lcs: {
      char *cond = "Off ?= findLocal(U,Lcs)";
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "where %s => ([.intgr(%d),.intgr(Off)],Pc+1,Lts,Lns).\n", cond, op);
          return;
        case lit:
          outMsg(out,
                 "where %s && (Lt1,LtNo) .= findLit(Lts,V) => ([.intgr(%d),.intgr(Off),.intgr(LtNo)],Pc+1,Lt1,Lns).\n",
                 cond, op);
          return;
        default:
          check(False, "Cannot generate instruction code: invalid second operand");
      }
    }
    case glb:
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d),.strg(U)],Pc+1,Lts,Lns).\n", op);
          return;
        case lVl:
          outMsg(out, "where Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.strg(U),.intgr(Lvl)],Pc+1,Lts,Lns).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }

    case Es: {                              // escape name
      switch (B) {
        case nOp:
        case tOs:
          outMsg(out, "=> ([.intgr(%d),.strg(U)],Pc+1,Lts,Lns).\n", op);
          break;
        case lVl:
          outMsg(out, "where Lvl ?= findLevel(Lbls,V) => ([.intgr(%d),.strg(U),.intgr(Lvl)],Pc+1,Lts,Lns).\n", op);
          return;
        default:
          check(False, "Cannot generate instruction code");
      }
      break;
    }
    case bLk: {                           // A nested block of instructions
      check(False, "block not allowed as first argument");
    }
    case lVl: {                          // Break out of a nesting sequence of blocks
      check(False, "invalid first operand");
    }
  }
}










static void genHwmOp(ioPo out, opAndSpec A, char *argVar, integer *currH, integer *H) {
  switch (A) {
    case bLk: {
      outMsg(out, "  (CH%d,H%d) = stkHwm(%s,CH%d,H%d);\n", (*currH) + 1, (*H) + 1, argVar, *currH, *H);
      (*H)++;
      (*currH)++;
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

  outMsg(out, "  stkHwm([.i%s", capitalize(mnem));

  sep = genHWmArg(out, sep, A1, "V");
  sep = genHWmArg(out, sep, A2, "W");

  if (strcmp(sep, ",") == 0)
    outStr(out, ")");

  outMsg(out, ",..Ins],CH0,H0) => valof{\n");

  integer currH = 0;
  integer currCH = 0;

  genHwmOp(out, A1, "V", &currCH, &currH);
  genHwmOp(out, A2, "W", &currCH, &currH);

  if (delta != 0) {
    outMsg(out, "    CH%d = CH%d%s%d;\n", currCH + 1, currCH, (delta > 0 ? "+" : ""), delta);
    currCH++;
  }

  if (delta > 0 || A1 == bLk || A2 == bLk)
    outMsg(out, "    valis stkHwm(Ins,CH%d,(CH%d>H%d??CH%d||H%d))\n", currCH, currCH, currH, currCH, currH);
  else
    outMsg(out, "    valis stkHwm(Ins,CH%d,H%d)\n", currCH, currH);

  outMsg(out, "  }\n");
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
    case glb:
    case Es:
      outMsg(out, " $(%s)", Nm);
      return True;
    case lcl:
    case lcs:
    case lVl:
      outMsg(out, " #(%s)", Nm);
      return True;
    case bLk:
      outMsg(out,"\\n#(showBlock(%s,Pc))",Nm);
      return True;
  }
}

static void showStarIns(ioPo out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  outMsg(out, "  showIns(.i%s", capitalize(mnem));

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
#define instruction(M, A1, A2, Dl, Cmt) hash = hash61(hash*39+opHash(#M,M));

#include "instructions.h"

  return hash;
}

