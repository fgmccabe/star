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

/* Generate a Prolog module, that knows how to assemble a program */

char *templateFn = "pl.star.plate";
char date[MAXLINE] = "";

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
} AsmInfoRecord, *asmInfoPo;

static void genAsm(hashPo vars);
static void genShow(hashPo vars);

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

    genShow(vars);

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

static void genOps(asmInfoPo info, char *fmt) {
  while (*fmt != '\0') {
    switch (*fmt++) {
      case Slcl: {
        int32 vNo = info->vNo++;
        outMsg(O_IO(info->aux), ", Off%d", vNo);
        outMsg(O_IO(info->line), "  findLocal(V%d,LsMap,Off%d),\n", vNo, vNo);
        continue;
      }
      case Slcls: {
        int32 vNo = info->vNo++;
        outMsg(O_IO(info->aux), ", LL%d", vNo);
        outMsg(O_IO(info->line), "  findLocals(V%d,LsMap,LL%d),\n", vNo, vNo);
        continue;
      }
      case Sart:
      case Si32:
        outMsg(O_IO(info->aux), ", intgr(V%d)", (info->vNo)++);
        continue;
      case Sglb:
      case SEs:
        outMsg(O_IO(info->aux), ", strg(V%d)", (info->vNo)++);
        continue;
      case SbLk: {
        int32 lastLtno = info->ltNo++;
        int32 NxtLt = info->ltNo;
        int32 vNo = info->vNo++;
        outMsg(O_IO(info->aux), ", B%d", vNo);
        outMsg(O_IO(info->line), "  assemBlock(V%d,none,Lbls,Lt%d,Lt%d,LsMap,B%d),\n",
          vNo,lastLtno,NxtLt,vNo);
        continue;
      }
      case SlVl: {
        int32 vNo = info->vNo++;
        outMsg(O_IO(info->aux), ", intgr(L%d)", vNo);
        outMsg(O_IO(info->line), "  findLevel(Lbls,V%d,L%d),\n", vNo, vNo);
        continue;
      }
      case Slit:
      case Ssym: {
        int32 vNo = info->vNo++;
        int32 lastLtno = info->ltNo++;
        outMsg(O_IO(info->line), "  findLit(Lt%d,V%d,L%d,Lt%d),\n", lastLtno, vNo, vNo,
        info->ltNo);
        outMsg(O_IO(info->aux), ",intgr(L%d)", vNo);
        continue;
      }

      default:
        fprintf(stderr, "Unknown instruction type code\n");
        exit(1);
    }
  }
}

void genMnem(asmInfoPo info, char *mnem, int op, char *fmt) {
  outMsg(info->out, "mnem([i%s", capitalize(mnem));

  clearStrBuffer(info->line);
  clearStrBuffer(info->aux);
  info->ltNo = 0;

  if (uniStrLen(fmt) > 0) {
    int32 vCnt = 0;
    outMsg(info->out, "(");
    genArgs(info->out, fmt, &vCnt);
    outMsg(info->out, ")");
  }

  info->vNo = 0;

  genOps(info, fmt);

  integer auxLen;
  char *aux = getTextFromBuffer(info->aux, &auxLen);

  integer lineLen;
  char *line = getTextFromBuffer(info->line, &lineLen);

  outMsg(info->out, "|Ins],Lbls,Lt0,Ltx,LsMap,[intgr(%d)%S|Cd],Cdx) :-\n", op, aux, auxLen);
  outMsg(info->out, "%S", line, lineLen);
  outMsg(info->out, "  mnem(Ins,Lbls,Lt%d,Ltx,LsMap,Cd,Cdx).\n", info->ltNo);
}

static void genAsm(hashPo vars) {
  // Set up the assembler proper
  strBufferPo mnemBuff = newStringBuffer();
  strBufferPo auxBuff = newStringBuffer();
  strBufferPo lineBuff = newStringBuffer();

  AsmInfoRecord info = {.out = O_IO(mnemBuff), .line = lineBuff, .aux = auxBuff, .vNo = 0, .ltNo = 0};

#undef instr
#define instr(M, Fmt) genMnem(&info, #M, s##M, Fmt);

#include "ssaInstructions.h"

  integer insLen;
  char *allCode = getTextFromBuffer(mnemBuff, &insLen);
  hashPut(vars, "Mnem", allCode);

  // closeIo(O_IO(mnemBuff));
  closeIo(O_IO(auxBuff));
  closeIo(O_IO(lineBuff));
}

static void genDisp(asmInfoPo info, char *fmt, int32 arity) {
  int32 ix = 0;
  while (ix < arity) {
    switch (*fmt++) {
      case Slcl:
      case Sglb:
      case SEs:
      case SlVl:
        outMsg(O_IO(info->aux), ",ss(\" \"),ss(V%d)", ix++);
        continue;
      case Slit: {
        int32 vNo = ix++;
        outMsg(O_IO(info->aux), ",ss(\" \"),SS%d", vNo);
        outMsg(O_IO(info->line), "  ssTrm(V%d,0,SS%d),\n", vNo, vNo);
        continue;
      }
      case Slcls: {
        int32 vNo = ix++;
        outMsg(O_IO(info->aux), ",ss(\" \"),VV%d", vNo);
        outMsg(O_IO(info->line), "  showCallArgs(V%d,VV%d),\n", vNo, vNo);
        continue;
      }
      case Sart:
      case Si32:
        outMsg(O_IO(info->aux), ",ss(\" \"),ix(V%d)", ix++);
        continue;
      case SbLk: {
        int32 vNo = ix++;
        outMsg(O_IO(info->aux), ",ss(\" \"),sq([nl(Dp),iv(nl(Dp),SS%d)])", vNo);
        outMsg(O_IO(info->line), "  blockPc(Pc,SPc),\n");
        outMsg(O_IO(info->line), "  pcSpace(SPc,Dp),\n");
        outMsg(O_IO(info->line), "  showMnems(V%d,SPc,SS%d),\n", vNo, vNo);
        continue;
      }
      case Ssym: {
        int32 vNo = ix++;
        outMsg(O_IO(info->aux), ",ss(\" \"),SS%d", vNo);
        outMsg(O_IO(info->line), "  ssTrm(V%d,0,SS%d),\n", vNo, vNo);
        continue;
      }

      default:
        fprintf(stderr, "Unknown instruction type code\n");
        exit(1);
    }
  }
}

void showIns(asmInfoPo info, char *mnem, ssaOp op, char *fmt) {
  clearStrBuffer(info->line);
  clearStrBuffer(info->aux);

  int32 arity = 0;
  outMsg(info->out, "showMnem(i%s", capitalize(mnem));
  if (*fmt != '\0') {
    outMsg(info->out, "(");
    genArgs(info->out, fmt, &arity);
    outMsg(info->out, ")");
  }

  genDisp(info, fmt, arity);

  integer auxLen;
  char *aux = getTextFromBuffer(info->aux, &auxLen);

  outMsg(info->out, ",Pc,sq([PcDx,ss(\": \"),ss(\"%P\")%S])) :- !,\n", mnem, aux, auxLen);

  integer lineLen;
  char *line = getTextFromBuffer(info->line, &lineLen);
  outMsg(info->out, "%S", line, lineLen);

  outMsg(info->out, "  showPc(Pc,PcDx).\n");
}

static void genShow(hashPo vars) {
  // Set up the display code
  strBufferPo showBuff = newStringBuffer();
  strBufferPo lineBuff = newStringBuffer();
  strBufferPo auxBuff = newStringBuffer();

  AsmInfoRecord info = {.out = O_IO(showBuff), .line = lineBuff, .aux = auxBuff, .vNo = 0};

#undef instr
#define instr(M, Fmt) showIns(&info,#M, s##M, Fmt);

#include "ssaInstructions.h"

  integer showLen;
  char *showCode = getTextFromBuffer(showBuff, &showLen);
  hashPut(vars, "Show", showCode);

  // closeIo(O_IO(showBuff));
  closeIo(O_IO(auxBuff));
  closeIo(O_IO(lineBuff));
}
