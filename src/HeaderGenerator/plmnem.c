#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "opcodes.h"

/* Generate a Prolog module, that knows how to assemble a program*/

#undef instruction
#define instruction(M, A1, cmt) genIns(out,#M,M,A1,cmt);

#define lastInstruction

static void genIns(FILE *out, char *mnem, int op, opAndSpec A1, char *cmt);

char *prefix = "star.comp.assem";

enum {
  genProlog, genStar
} genMode = genProlog;

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "pc:")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 'c':
        genMode = genStar;
        prefix = optarg;
        break;
      default:;
    }
  }
  return optind;
}

int main(int argc, char **argv) {
  FILE *out = stdout;

  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else {
    if (narg < argc)
      out = fopen(argv[narg], "w");
  }

  fprintf(out, "/* Automatically generated, do not edit */\n\n");

  fprintf(out, ":- module(assemble,[assem/2]).\n");
  fprintf(out, ":- use_module(misc).\n");
  fprintf(out, ":- use_module(terms).\n");
  fprintf(out, ":- use_module(encode).\n\n");

  fprintf(out, "assem([method(Nm,Sig)|Ins],MTpl) :-\n");
  fprintf(out, "    genLblTbl(Ins,0,[],Lbs),\n");
  fprintf(out, "    mnem(Ins,Lbs,[],Lts,[],Lcs,0,Cde),\n");
  fprintf(out, "    mkInsTpl(Cde,Code),\n");
  fprintf(out, "    mkLitTpl(Lts,LtTpl),\n");
  fprintf(out, "    mkTpl(Lcs,LcsTpl),\n");
  fprintf(out, "    mkTpl([Nm,strg(Sig),Code,LtTpl,LcsTpl],MTpl).\n\n");

  fprintf(out, "mnem([],_,Lt,Lt,Lc,Lc,_,[]).\n");
  fprintf(out, "mnem([iLbl(_)|Ins],Lbs,Lt,Lts,Lc,Lcx,Pc,Code) :- mnem(Ins,Lbs,Lt,Lts,Lc,Lcx,Pc,Code).\n");

  fprintf(out, "mnem([iLocal(Nm,Frm,End,Off)|Ins],Lbs,Lt,Lts,Lc,Lcx,Pc,Code) :-\n");
  fprintf(out, "    findLbl(Frm,Lbs,F),\n");
  fprintf(out, "    findLbl(End,Lbs,T),\n");
  fprintf(out, "    mkTpl([strg(Nm),intgr(F),intgr(T),intgr(Off)],Entry),\n");
  fprintf(out, "    mnem(Ins,Lbs,Lt,Lts,[Entry|Lc],Lcx,Pc,Code).\n");

#include "instructions.h"

  fprintf(out, "\n");

  fprintf(out, "genLblTbl([],_,Lbls,Lbls).\n");
  fprintf(out, "genLblTbl([iLbl(Lbl)|Ins],Pc,Lbls,Lbx) :- genLblTbl(Ins,Pc,[(Lbl,Pc)|Lbls],Lbx).\n");
  fprintf(out, "genLblTbl([_|Ins],Pc,Lb,Lbx) :- Pc1 is Pc+1, genLblTbl(Ins,Pc1,Lb,Lbx).\n\n");

  fprintf(out, "findLbl(L,Lbs,Tgt) :- is_member((L,Tgt),Lbs),!.\n\n");
  fprintf(out, "pcGap(Pc,Tgt,Off) :- Off is Tgt-Pc-1.\n\n");

  fprintf(out, "findLit(Lits,V,LtNo,Lits) :- is_member((V,LtNo),Lits),!.\n");
  fprintf(out, "findLit(Lits,V,LtNo,[(V,LtNo)|Lits]) :- length(Lits,LtNo).\n\n");

  fprintf(out, "mkLitTpl(Lits,Tpl) :-\n");
  fprintf(out, "    reverse(Lits,RLit),\n");
  fprintf(out, "    project0(RLit,Els),\n");
  fprintf(out, "    mkTpl(Els,Tpl).\n\n");

  fprintf(out, "mkInsTpl(Is,Tpl) :-\n");
  fprintf(out, "    map(Is,assemble:mkIns,Ins),\n");
  fprintf(out, "    mkTpl(Ins,Tpl).\n\n");

  fprintf(out, "mkIns((O,A),Tpl) :-\n");
  fprintf(out, "    wrap(A,WA),\n");
  fprintf(out, "    mkTpl([intgr(O),WA],Tpl).\n");
  fprintf(out, "mkIns(O,intgr(O)) :- number(O).\n\n");
  fprintf(out, "wrap(O,intgr(O)) :- number(O).\n");
  fprintf(out, "wrap(S,strg(S)) :- string(S).\n\n");

  fclose(out);
  exit(0);
}

static char *genArg(FILE *out, char *sep, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
      return sep;
    case lit:
    case Es:
    case i32:
    case arg:
    case lcl:
    case off:
      fprintf(out, "%sV", sep);
      return ",";
    default:
      printf("Problem in generating opcode type\n");
      exit(11);
  }
}

char *tail = "      mnem(Ins,Lbls,Lt,Ltx,Lc,Lcx,Pc1,M).\n";

static void genCode(FILE *out, int op, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
      fprintf(out, "%d|M]) :- Pc1 is Pc+1,\n", op);
      break;
    case lit:
      fprintf(out, "%d,LtNo|M]) :- Pc1 is Pc+3,\n", op);
      fprintf(out, "      findLit(Lt,V,LtNo,Lt1),\n");
      fprintf(out, "      mnem(Ins,Lbls,Lt1,Ltx,Lc,Lcx,Pc1,M).\n");
      return;
    case i32:
    case arg:
    case lcl:
      fprintf(out, "%d,V|M]) :- Pc1 is Pc+3,\n", op);
      break;
    case Es:                              // escape code (0..65535)
      fprintf(out, "%d,V|M]) :- Pc1 is Pc+3,\n", op);
      break;
    case off:                            // program counter relative offset
      fprintf(out, "%d,Off|M]) :- Pc1 is Pc+3,\n", op);
      fprintf(out, "      findLbl(V,Lbls,Tgt),\n");
      fprintf(out, "      pcGap(Pc,Tgt,Off),\n");
      break;
    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
  fprintf(out, "%s", tail);
}

static char *capitalize(char *str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char) ('A' + (buffer[0] - 'a'));
  }
  return buffer;
}

static void genIns(FILE *out, char *mnem, int op, opAndSpec A1, char *cmt) {
  char *sep = "(";

  fprintf(out, "mnem([i%s", capitalize(mnem));

  sep = genArg(out, sep, A1);

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  fprintf(out, "%s|Ins],Lbls,Lt,Ltx,Lc,Lcx,Pc,[", sep);

  genCode(out, op, A1);
}
