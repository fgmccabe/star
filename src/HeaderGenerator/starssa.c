#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "hash.h"
#include "unistr.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include "formexts.h"
#include "ssaOps.h"

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
      default: ;
    }
  }
  return optind;
}

typedef struct {
  ioPo out;
  strBufferPo line;
  strBufferPo aux;
  int32 vNo;
  int32 ltNo;
  int32 pcNo;
} AsmInfoRecord, *asmInfoPo;

static void genAsm(hashPo vars);
static void showIns(ioPo out, char *mnem, int op, char *fmt);
static void ssaOpType(ioPo out, char *mnem, int op, char *fmt);

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

#undef instr
#define sym "s"
#define lcl "v"
#define lcls "V"
#define lit "l"
#define glb "g"
#define art "a"
#define i32 "i"
#define Es "e"
#define bLk "k"
#define lVl "b"
#define lVls "B"
#define none ""

#define instr(M, Fmt) ssaOpType(O_IO(typeBuff), #M, M, Fmt);

#include "ssaInstructions.h"

    integer tpLen;
    char *typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

    // Set up the display code
    strBufferPo showBuff = newStringBuffer();

#undef instr
#define instr(M, Fmt) showIns(O_IO(showBuff),#M, M, Fmt);

#include "ssaInstructions.h"

    integer showLen;
    char *showCode = getTextFromBuffer(showBuff, &showLen);
    hashPut(vars, "Show", showCode);

    genAsm(vars);

    static char hashBuff[64];
    strMsg(hashBuff, NumberOf(hashBuff), "%ld", OPCODE_SIGNATURE);
    hashPut(vars, "Hash", hashBuff);

    retCode ret = processTemplate(out, plate, vars, NULL, NULL);

    flushOut();
    closeIo(out);
    exit(0);
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

static void genArgs(ioPo out, char *fmt, int32 *cnt) {
  *cnt = 0;

  char *sep = "";

  while (*fmt++ != '\0') {
    outMsg(out, "%sV%d", sep, (*cnt)++);
    sep = ", ";
  }
}

static void genOp(asmInfoPo info, char **fmt) {
  switch (*(*fmt)++) {
    case Slcl:
      outMsg(O_IO(info->line), "findLocal(V%d,Lcs)", (info->vNo)++);
      return;
    case Slcls:
      outMsg(O_IO(info->line), "mkTpl(findLocals(V%d,Lcs))", (info->vNo)++);
      return;
    case Slit: {
      int32 lastLtno = info->ltNo++;
      outMsg(O_IO(info->aux), "(Lt%d, L%d) = findLit(Lt%d,V%d); ", info->ltNo, info->ltNo, lastLtno, (info->vNo)++);
      outMsg(O_IO(info->line), ".intgr(L%d)", info->ltNo);
      return;
    }
    case Sglb:
      outMsg(O_IO(info->line), ".strg(V%d)", (info->vNo)++);
      return;
    case Sart:
    case Si32:
      outMsg(O_IO(info->line), ".intgr(V%d)", (info->vNo)++);
      return;
    case SEs:
      outMsg(O_IO(info->line), ".strg(V%d)", (info->vNo)++);
      return;
    case SbLk: {
      int32 lastLtno = info->ltNo++;
      int32 NxtLt = info->ltNo;
      outMsg(O_IO(info->aux), "(A%d, _, Lt%d) = assemBlock(V%d,[],Pc+1,[.none,..Lbls],Lt%d,Lcs); ", NxtLt, NxtLt,
             (info->vNo)++, lastLtno);
      outMsg(O_IO(info->line), "mkTpl(A%d::cons[data])", NxtLt);
      return;
    }
    case SlVl:
      outMsg(O_IO(info->line), ".intgr(findLevel(Lbls,V%d))", (info->vNo)++);
      return;
    case Ssym: {
      int32 lastLtno = info->ltNo++;
      outMsg(O_IO(info->aux), "(Lt%d, L%d) = findLit(Lt%d,.symb(V%d)); ", info->ltNo, info->ltNo, lastLtno,
             (info->vNo)++);
      outMsg(O_IO(info->line), ".intgr(L%d)", info->ltNo);
      return;
    }

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
}

void genStarMnem(asmInfoPo info, char *mnem, int op, char *fmt) {
  outMsg(info->out, "  mnem(.i%s", capitalize(mnem));

  clearStrBuffer(info->line);
  clearStrBuffer(info->aux);
  info->ltNo = 0;
  info->pcNo = 1;

  if (uniStrLen(fmt) > 0) {
    int32 vCnt = 0;
    outMsg(info->out, "(");
    genArgs(info->out, fmt, &vCnt);
    outMsg(info->out, ")");
  }
  outMsg(info->out, ", Pc,Lbls,Lt%d,Lcs) => ", info->ltNo);

  char *opFmt = fmt;
  info->vNo = 0;
  while (*opFmt != '\0') {
    outMsg(O_IO(info->line), ",");
    genOp(info, &opFmt);
    info->pcNo++;
  }
  integer lineLen;
  char *line = getTextFromBuffer(info->line, &lineLen);

  if (strBufferLength(info->aux) > 0) {
    integer auxLen;
    char *aux = getTextFromBuffer(info->aux, &auxLen);
    outMsg(O_IO(info->out), "valof {\n    %S\n    valis ([.intgr(%d)%S],Pc+%d,Lt%d);\n  }\n", aux, (long) auxLen, op,
           line,
           (long) lineLen, info->pcNo, info->ltNo);
  } else {
    outMsg(O_IO(info->out), "([.intgr(%d)%S],Pc+%d,Lt%d).\n", op, line, (long) lineLen, info->pcNo, info->ltNo);
  }
}

static void genAsm(hashPo vars) {
  // Set up the assembler proper
  strBufferPo mnemBuff = newStringBuffer();
  strBufferPo auxBuff = newStringBuffer();
  strBufferPo lineBuff = newStringBuffer();

  AsmInfoRecord info = {.out = O_IO(mnemBuff), .line = lineBuff, .aux = auxBuff, .vNo = 0, .ltNo = 0};

#undef instr
#define instr(M, Fmt) genStarMnem(&info, #M, M, Fmt);

#include "ssaInstructions.h"

  integer insLen;
  char *allCode = getTextFromBuffer(mnemBuff, &insLen);
  hashPut(vars, "Mnem", allCode);
}

static char *opAndTp(char **f) {
  switch (*(*f)++) {
    case Slcl:
      return "varNm";
    case Slcls:
      return "cons[varNm]";
    case Slit:
      return "data";
    case Sglb:
      return "string";
    case Sart:
    case Si32:
      return "integer";
    case SEs:
      return "string";
    case SbLk:
      return "multi[insOp]";
    case SlVl:
      return "assemLbl";
    case Ssym:
      return "termLbl";

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
  }
}

void ssaOpType(ioPo out, char *mnem, int op, char *fmt) {
  outMsg(out, "    | .i%s", capitalize(mnem));

  if (uniStrLen(fmt) != 0) {
    char *sep = "(";
    while (*fmt != '\0') {
      outMsg(out, "%s%s", sep, opAndTp(&fmt));
      sep = ", ";
    }
    outMsg(out, ")\n");
  } else
    outMsg(out, "\n");
}

static void genDisp(ioPo out, char *fmt, int32 arity) {
  int32 ix = 0;
  while (ix < arity) {
    switch (*fmt++) {
      case Slcl:
        outMsg(out, " #(V%d)", ix++);
        continue;
      case Slcls:
      case Slit:
        outMsg(out, " $(V%d)", ix++);
        continue;
      case Sglb:
        outMsg(out, " #(V%d)", ix++);
        continue;
      case Sart:
      case Si32:
        outMsg(out, " $(V%d)", ix++);
        continue;
      case SEs:
        outMsg(out, " #(V%d)", ix++);
        continue;
      case SbLk:
        outMsg(out, " #(showBlock(V%d,[0,..Pc]))", ix++);
        continue;
      case SlVl:
      case Ssym:
        outMsg(out, " $(V%d)", ix++);
        continue;

      default:
        fprintf(stderr, "Unknown instruction type code\n");
        exit(1);
    }
  }
}

void showIns(ioPo out, char *mnem, int op, char *fmt) {
  int32 arity = 0;
  outMsg(out, "  showIns(.i%s", capitalize(mnem));
  if (*fmt != '\0') {
    outMsg(out, "(");
    genArgs(out, fmt, &arity);
    outMsg(out, ")");
  }
  outMsg(out, ", Pc) => \"%P", mnem);
  genDisp(out, fmt, arity);
  outMsg(out, "\".\n");
}
