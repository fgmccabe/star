
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "opcodes.h"

/* Generate an L&O package, that knows about instructions and their
   mnemonics */

#undef instruction
#define instruction(M, O, A1, A2, cmt) genIns(out,#M,O,A1,A2,cmt);

#define lastInstruction

static void genIns(FILE *out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt);

char *prefix = "lo.comp.code.asm";

int getOptions(int argc, char **argv) {
  int opt;
  extern char *optarg;
  extern int optind;

  while ((opt = getopt(argc, argv, "c:")) >= 0) {
    switch (opt) {
      case 'c':
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

  fprintf(out, "%s{\n", prefix);
  fprintf(out, "/* Automatically generated, do not edit */\n\n");

  fprintf(out, "  import lo.\n");
  fprintf(out, "  import lo.comp.escapes.\n");
  fprintf(out, "  import lo.comp.package.\n");
  fprintf(out, "  import lo.comp.term.\n");
  fprintf(out, "  import lo.comp.code.code.\n");
  fprintf(out, "  import lo.comp.code.instructions.\n");
  fprintf(out, "  import lo.comp.code.registers.\n\n");

  fprintf(out, "  public codeSeg ::= codeSeg(term,list[integer],list[term],list[term]).\n");
  fprintf(out, "  public codeMdl ::= codeMdl(pkgSpec,list[codeSeg]).\n\n");

  fprintf(out, "  public asm:(assem)=>codeSeg.\n");
  fprintf(out,
          "  asm(assem(Nm,Ins,Lits,SrcMap)) => codeSeg(Nm,mnem(Ins,Lbls,genLitTbl(Lits,0,[]),0),Lits//((litrl(_,T))=>T),genSrcMap(SrcMap,Lbls)) :-\n"
            "      Lbls = genLblTbl(Ins,0,[]).\n\n");

  fprintf(out, "  private mnem:(list[instruction],map[string,integer],map[string,integer],integer)=>list[integer].\n");
  fprintf(out, "  mnem([],_,_,_) => [].\n");
  fprintf(out, "  mnem([iLbl(_),..I],Lbls,Lits,Pc) => mnem(I,Lbls,Lits,Pc).\n");

#include "instructions.h"

  fprintf(out, "\n");

  fprintf(out, "  private genLblTbl:(list[instruction],integer,map[string,integer]) => map[string,integer].\n");
  fprintf(out, "  genLblTbl([],_,D) => D.\n");
  fprintf(out, "  genLblTbl([iLbl(Lbl),..I],Pc,D) => genLblTbl(I,Pc,D[Lbl->Pc]).\n");
  fprintf(out, "  genLblTbl([_,..I],Pc,D) => genLblTbl(I,Pc+1,D).\n\n");

  fprintf(out, "  private genLitTbl:(list[litrl],integer,map[string,integer]) => map[string,integer].\n");
  fprintf(out, "  genLitTbl([],_,D) => D.\n");
  fprintf(out, "  genLitTbl([litrl(Lbl,_),..I],Pc,D) => genLitTbl(I,Pc+1,D[Lbl->Pc]).\n\n");

  fprintf(out, "  private genSrcMap:(list[(string,string,tloc)],map[string,integer])=>list[term].\n");
  fprintf(out, "  genSrcMap([],_) => [].\n");
  fprintf(out,
          "  genSrcMap([(S,E,tloc(Line,Off,Col,Ln)),..L],M) => [cons(strct(\"()4\",4),[intgr(Sx),intgr(Ex),intgr(Off),intgr(Ln)]),..genSrcMap(L,M)] :-\n");
  fprintf(out, "    present(M,S,Sx),\n");
  fprintf(out, "    present(M,E,Ex).\n\n");

  fprintf(out, "  private ltOff:(string,map[string,integer]) => integer.\n");
  fprintf(out, "  ltOff(Lb,Lbls) => Tgt :-\n");
  fprintf(out, "    present(Lbls,Lb,Tgt).\n\n");

  fprintf(out, "  private pcGap:(integer,string,map[string,integer],integer) => integer.\n");
  fprintf(out, "  pcGap(pc,Lb,Lbls,mx) => Gap :-\n");
  fprintf(out, "    present(Lbls,Lb,Tgt),\n");
  fprintf(out, "    Gap = Tgt-pc-1,\n");
  fprintf(out, "    Gap=<mx.\n\n");

  fprintf(out, "}               -- end of generated module\n");
  fclose(out);
  exit(0);
}

static char *genArg(FILE *out, char *sep, int *V, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
      return sep;
    case iAh:                             // input argument register in upper slot (0..255)
    case oAh:                             // output argument register in upper slot (0..255)
    case iAm:                             // input argument register in middle slot (0..255)
    case oAm:                             // output argument register in middle slot (0..255)
    case iAl:                             // input argument register in lower slot (0..255)
    case oAl:                             // output argument register in lower slot (0..255)
    case iLh:        // input local variable offset (0..255)
    case iLm:        // input local variable offset (0..255)
    case iLl:        // input local variable offset (0..255)
    case iLc:             // input local variable offset (0..65535)
    case oLh:            /* output local variable, offset 0..255 */
    case oLm:            /* output local variable, offset 0..255 */
    case oLl:            /* output local variable, offset 0..255 */
    case oLc:           // output local variable offset  (0..65535)
      fprintf(out, "%sV%d", sep, (*V)++);
      return ",";
    case iSt:                             // input at current structure pointer
    case oSt:                             // output to current structure pointer
      return sep;
    case oAr:        /* Arity in upper slot */
    case uAr:                             // Arity in upper slot
    case uLt:                             // small literal in upper slot (-128..127)
    case Ltl:                              // 16bit literal (-32768..32767)
    case vSz:                             // Size of local variable vector
    case lSz:                             // Size of local variable vector
      fprintf(out, "%sV%d", sep, (*V)++);
      return ",";
    case Es:                              // escape code (0..65535)
      fprintf(out, "%sV%d", sep, (*V)++);
      return ",";
    case pcr:                             // program counter relative offset (-32768..32767)
    case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
    case ltl:                             // literal number (0..65535)
      fprintf(out, "%sV%d", sep, (*V)++);
      return ",";
    default:
      printf("Problem in generating opcode type\n");
      exit(11);
  }
}

static void genCode(FILE *out, int *V, opAndSpec A) {
  switch (A) {
    case nOp:                             // No operand
      return;
    case iAh:                             // input argument register in upper slot (0..255)
    case oAh:                             // output argument register in upper slot (0..255)
    case oAr:        /* Arity in upper slot */
    case uAr:                             // Arity in upper slot
    case uLt:                             // small literal in upper slot (-128..127)
    case iLh:        /* input local variable, offset 0..255 */
    case oLh:        /* output local variable, offset 0..255 */
      fprintf(out, ".|.(V%d.<<.24)", (*V)++);
      return;
    case iAm:                             // input argument register in middle slot (0..255)
    case oAm:                             // output argument register in middle slot (0..255)
    case iLm:        /* input local variable, offset 0..255 */
    case oLm:        /* output local variable, offset 0..255 */
      fprintf(out, ".|.(V%d.<<.16)", (*V)++);
      return;
    case iAl:                             // input argument register in lower slot (0..255)
    case oAl:                             // output argument register in lower slot (0..255)
    case iLl:        /* input local variable, offset 0..255 */
    case oLl:        /* output local variable, offset 0..255 */
      fprintf(out, ".|.(V%d.<<.8)", (*V)++);
      return;
    case iLc:                             // input local variable offset (0..65535)
    case oLc:                             // output local variable offset  (0..65535)
    case vSz:                             // Size of local variable vector
    case lSz:                             // Size of local variable vector
    case Ltl: {                              // 16bit literal (-32768..32767)
      int off = (*V)++;
      fprintf(out, ".|.(V%d.<<.8)", off);
      return;
    }
    case iSt:                             // input at current structure pointer
    case oSt:                             // output to current structure pointer
      return;
    case Es:                              // escape code (0..65535)
      fprintf(out, ".|.(escCode(V%d).<<.8)", (*V)++);
      return;
    case pcr:                             // program counter relative offset (-32768..32767)
      fprintf(out, ".|.(pcGap(pc,V%d,Lbls,65535).<<.8)", (*V)++);
      return;
    case pcl:                             // long pc relative offset (-0x80000000..0x7fffffff) (24bit)
      fprintf(out, ".|.(pcGap(pc,V%d,Lbls,16777215).<<.8)", (*V)++);
      return;
    case ltl:                             // literal number (0..65535)
      fprintf(out, ".|.(ltOff(V%d,Lits).<<.8)", (*V)++);
      return;
    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
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

static void genIns(FILE *out, char *mnem, int op, opAndSpec A1, opAndSpec A2, char *cmt) {
  char *sep = "(";
  int V = 0;

  fprintf(out, "  mnem([i%s", capitalize(mnem));

  sep = genArg(out, sep, &V, A1);
  sep = genArg(out, sep, &V, A2);

  if (strcmp(sep, ",") == 0)
    sep = ")";
  else
    sep = "";

  fprintf(out, "%s,..I],Lbls,Lits,pc) => [%d", sep, (unsigned char) (op));

  V = 0;

  genCode(out, &V, A1);
  genCode(out, &V, A2);

  fprintf(out, ",..mnem(I,Lbls,Lits,pc+1)].\n");
}
