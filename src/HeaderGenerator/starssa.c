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

char* prefix = "star.comp.assem";
char* templateFn = "ssa.star.plate";
char date[MAXLINE] = "";

int getOptions(int argc, char** argv) {
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
  ioPo outFl;
  strBufferPo line;
  strBufferPo aux;
  int32 vNo;
  int32 ltNo;
  int32 pcNo;
} AsmInfoRecord, *asmInfoPo;

static void genAsm(hashPo vars);
void showIns(asmInfoPo info, char* mnem, ssaOp op, char* fmt);
static void ssaOpType(ioPo out, char* mnem, int op, char* fmt);

static void generateDisplay(hashPo vars);
static void generateVerify(hashPo vars);

int main(int argc, char** argv) {
  initLogfile("-");
  installMsgProc('P', genQuotedStr);
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  }
  else {
    if (uniStrLen(date) == 0) {
      time_t rawtime;
      time(&rawtime);
      struct tm* timeinfo = localtime(&rawtime);

      strftime(date, NumberOf(date), "%c", timeinfo);
    }
    ioPo plate = openInFile(templateFn, utf8Encoding);

    if (plate == Null) {
      outMsg(logFile, "cannot find template file %s\n", templateFn);
      exit(1);
    }

    ioPo outFl;

    if (narg < argc)
      outFl = openOutFile(argv[narg], utf8Encoding);
    else
      outFl = Stdout();

    // Template variables
    hashPo vars = newHash(8, (hashFun)uniHash, (compFun)uniCmp, NULL);
    hashPut(vars, "Date", date);

    // Set up the opcodes for the type definition
    strBufferPo typeBuff = newStringBuffer();

#undef instr
#define sym "s"
#define lcl "v"
#define lcls "V"
#define oUt "o"
#define oUts "O"
#define pHi "p"
#define pHis "P"
#define lcm "m"
#define lit "l"
#define glb "g"
#define art "a"
#define i32 "i"
#define Es "e"
#define bLk "k"
#define lVl "b"
#define lVls "B"
#define none ""

#define instr(M, Fmt) ssaOpType(O_IO(typeBuff), #M, s##M, Fmt);

#include "ssaInstructions.h"

    integer tpLen;
    char* typeCode = getTextFromBuffer(typeBuff, &tpLen);
    hashPut(vars, "OpCodes", typeCode);

    generateDisplay(vars);
    generateVerify(vars);

    genAsm(vars);

    static char hashBuff[64];
    strMsg(hashBuff, NumberOf(hashBuff), "%ld", OPCODE_SIGNATURE);
    hashPut(vars, "Hash", hashBuff);

    retCode ret = processTemplate(outFl, plate, vars, NULL, NULL);

    flushOut();
    closeIo(outFl);
    exit(0);
  }
}

static char* capitalize(char* str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char)('A' + (buffer[0] - 'a'));
  }
  return buffer;
}

static void genArgs(ioPo outFl, char* fmt, int32* cnt) {
  *cnt = 0;

  char* sep = "";

  while (*fmt++ != '\0') {
    outMsg(outFl, "%sV%d", sep, (*cnt)++);
    sep = ", ";
  }
}

static void genOp(asmInfoPo info, char** fmt) {
  switch (*(*fmt)++) {
  case Slcl:
  case SoUt:
  case SpHi:
    outMsg(O_IO(info->line), "findLocal(V%d,Lcs)", (info->vNo)++);
    return;
  case Slcls:
  case SoUts:
  case SpHis:
    outMsg(O_IO(info->line), "mkTpl(findLocals(V%d,Lcs))", (info->vNo)++);
    return;
  case Slcm:
    outMsg(O_IO(info->line), ".intgr(size(V%d))", (info->vNo)++);
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

void genStarMnem(asmInfoPo info, char* mnem, int op, char* fmt) {
  outMsg(info->outFl, "  mnem(.i%s", capitalize(mnem));

  clearStrBuffer(info->line);
  clearStrBuffer(info->aux);
  info->ltNo = 0;
  info->pcNo = 1;

  if (uniStrLen(fmt) > 0) {
    int32 vCnt = 0;
    outMsg(info->outFl, "(");
    genArgs(info->outFl, fmt, &vCnt);
    outMsg(info->outFl, ")");
  }
  outMsg(info->outFl, ", Pc,Lbls,Lt%d,Lcs) => ", info->ltNo);

  char* opFmt = fmt;
  info->vNo = 0;
  while (*opFmt != '\0') {
    outMsg(O_IO(info->line), ",");
    genOp(info, &opFmt);
    info->pcNo++;
  }
  integer lineLen;
  char* line = getTextFromBuffer(info->line, &lineLen);

  if (strBufferLength(info->aux) > 0) {
    integer auxLen;
    char* aux = getTextFromBuffer(info->aux, &auxLen);
    outMsg(O_IO(info->outFl), "valof {\n    %S\n    valis ([.intgr(%d)%S],Pc+%d,Lt%d);\n  }\n", aux, (long)auxLen, op,
           line,
           (long)lineLen, info->pcNo, info->ltNo);
  }
  else {
    outMsg(O_IO(info->outFl), "([.intgr(%d)%S],Pc+%d,Lt%d).\n", op, line, (long)lineLen, info->pcNo, info->ltNo);
  }
}

static void genAsm(hashPo vars) {
  // Set up the assembler proper
  strBufferPo mnemBuff = newStringBuffer();
  strBufferPo auxBuff = newStringBuffer();
  strBufferPo lineBuff = newStringBuffer();

  AsmInfoRecord info = {.outFl = O_IO(mnemBuff), .line = lineBuff, .aux = auxBuff, .vNo = 0, .ltNo = 0};

#undef instr
#define instr(M, Fmt) genStarMnem(&info, #M, s##M, Fmt);

#include "ssaInstructions.h"

  integer insLen;
  char* allCode = getTextFromBuffer(mnemBuff, &insLen);
  hashPut(vars, "Mnem", allCode);
}

static char* opAndTp(char** f) {
  switch (*(*f)++) {
  case Slcl:
  case SoUt:
  case SpHi:
    return "varNm";
  case Slcls:
  case SoUts:
  case SpHis:
    return "cons[varNm]";
  case Slcm:
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
    fprintf(stderr, "Unknown instruction type code: %c\n", *((*f) - 1));
    exit(1);
  }
}

void ssaOpType(ioPo outFl, char* mnem, int op, char* fmt) {
  outMsg(outFl, "    | .i%s", capitalize(mnem));

  if (uniStrLen(fmt) != 0) {
    char* sep = "(";
    while (*fmt != '\0') {
      outMsg(outFl, "%s%s", sep, opAndTp(&fmt));
      sep = ", ";
    }
    outMsg(outFl, ")\n");
  }
  else
    outMsg(outFl, "\n");
}

static void generateDisplay(hashPo vars) {
  // Set up the display code
  strBufferPo showBuff = newStringBuffer();
  strBufferPo lineBuff = newStringBuffer();
  strBufferPo auxBuff = newStringBuffer();

  AsmInfoRecord info = {.outFl = O_IO(showBuff), .line = lineBuff, .aux = auxBuff, .vNo = 0};

#undef instr
#define instr(M, Fmt) showIns(&info, #M, s##M, Fmt);

#include "ssaInstructions.h"

  integer showLen;
  char* showCode = getTextFromBuffer(showBuff, &showLen);
  hashPut(vars, "Show", showCode);
}

static void genDisp(asmInfoPo info, char* fmt, int32 arity) {
  int32 ix = 0;
  char* sep = " ";
  while (ix < arity) {
    switch (*fmt++) {
    case Slcl:
    case SoUt:
    case SpHi:
    case Sglb:
    case SEs: {
      int32 vNo = ix++;
      outMsg(O_IO(info->line), "%s#(V%d)", sep, vNo);
      sep = ", ";
      outMsg(O_IO(info->aux), "    Pc%d = Pc%d+1;\n", info->pcNo + 1, info->pcNo);
      info->pcNo++;
      continue;
    }
    case Slcls:
    case SoUts:
    case SpHis: {
      int32 vNo = ix++;
      outMsg(O_IO(info->line), "%s#(showLocals(V%d))", sep, vNo);
      sep = ", ";
      outMsg(O_IO(info->aux), "    Pc%d = Pc%d+size(V%d)+1;\n", info->pcNo + 1, info->pcNo, vNo);
      info->pcNo++;
      continue;
    }
    case Slit:
    case Slcm: {
      int32 vNo = ix++;
      outMsg(O_IO(info->line), "%s$(V%d)", sep, vNo);
      sep = ", ";
      outMsg(O_IO(info->aux), "    Pc%d = Pc%d+1;\n", info->pcNo + 1, info->pcNo);
      info->pcNo++;
      continue;
    }
    case Sart:
    case Si32:
    case Ssym: {
      int32 vNo = ix++;
      outMsg(O_IO(info->line), "%s$(V%d)", sep, vNo);
      sep = ", ";
      outMsg(O_IO(info->aux), "    Pc%d = Pc%d+1;\n", info->pcNo + 1, info->pcNo);
      info->pcNo++;
      continue;
    }
    case SlVl: {
      int32 vNo = ix++;
      outMsg(O_IO(info->line), "%s#(V%d)", sep, vNo);
      sep = ", ";
      outMsg(O_IO(info->aux), "    Pc%d = Pc%d+1;\n", info->pcNo + 1, info->pcNo);
      info->pcNo++;
      continue;
    }
    case SbLk: {
      int32 vNo = ix++;
      outMsg(O_IO(info->line), "%s#(InsTxt)", sep, vNo);
      sep = ", ";
      outMsg(O_IO(info->aux), "    (InsTxt,Pc%d) = showBlock(V%d,Pc%d,Sps+2);\n", info->pcNo + 1, vNo, info->pcNo);
      info->pcNo++;
      continue;
    }

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
    }
  }
}

void showIns(asmInfoPo info, char* mnem, ssaOp op, char* fmt) {
  clearStrBuffer(info->line);
  clearStrBuffer(info->aux);
  info->vNo = 0;
  info->pcNo = 0;
  int32 arity = 0;
  outMsg(info->outFl, "  showIns(.i%s", capitalize(mnem));
  if (*fmt != '\0') {
    outMsg(info->outFl, "(");
    genArgs(info->outFl, fmt, &arity);
    outMsg(info->outFl, ")");
  }
  int32 basePc = info->pcNo;
  outMsg(info->outFl, ", Pc%d, Sps) => valof{\n", info->pcNo);

  genDisp(info, fmt, arity);

  integer auxLen;
  char* aux = getTextFromBuffer(info->aux, &auxLen);
  outMsg(info->outFl, "%S", aux, auxLen);

  integer lineLen;
  char* line = getTextFromBuffer(info->line, &lineLen);
  outMsg(info->outFl, "    valis (\"#(showPc(Pc%d,Sps))%s%S\",Pc%d+1)\n", basePc, capitalize(mnem), line, lineLen,
         info->pcNo);
  outMsg(info->outFl, "  }\n");
}

static void genArgValidation(asmInfoPo info, char* fmt, char* mnem, int32 arity) {
  int32 vx = 0;
  char* sep = " ";
  while (vx < arity) {
    switch (*fmt++) {
    case Slcl: {
      outMsg(O_IO(info->aux), "%s  if ~varInited(Lcls%d, V%d) then\n", sep, info->vNo, vx);
      outMsg(O_IO(info->aux), "%s    throw .exception(\"Var #(V%d) in '%s' not inited\");\n", sep, vx,mnem);
      vx++;
      continue;
    }
    case Sglb: {
      vx++;
      continue;
    }
    case SEs: {
      int32 vr = vx++;
      outMsg(O_IO(info->aux), "%sif ~isEscape(V%d) then", sep, info->vNo, vr);
      outMsg(O_IO(info->aux), "%s  throw .exception(\"Unknown escape #(V%d)\");\n", sep, vr);
      continue;
    }
    case SoUt: {
      outMsg(O_IO(info->aux), "%s  if varInited(Lcls%d, V%d) then\n", sep, info->vNo, vx);
      outMsg(O_IO(info->aux), "%s    throw .exception(\"Var #(V%d) in '%s' already inited\");\n", sep, vx,mnem);
      outMsg(O_IO(info->aux), "%s  Lcls%d = markInited(Lcls%d,V%d);\n", sep, info->vNo + 1, info->vNo, vx);
      vx++;
      info->vNo++;
      continue;
    }
    case SpHi: {
      outMsg(O_IO(info->aux), "%sif ~varPhi(Lcls%d, V%d) then", sep, info->vNo, vx);
      outMsg(O_IO(info->aux), "%s    throw .exception(\"Var #(V%d) in '%s' not phi var\");\n", sep, vx,mnem);
      vx++;
      continue;
    }
    case Slcls: {
      outMsg(O_IO(info->aux), "%s if ~ {? Vv in V%d *> varInited(Lcls%d,Vv) ?} then", sep, vx, info->vNo);
      outMsg(O_IO(info->aux), "%s    throw .exception(\"Var $(V%d) in '%s' not inited\");\n", sep, vx,mnem);
      vx++;
      continue;
    }
    case SoUts: {
      outMsg(O_IO(info->aux), "%sif ~ {? Vv in V%d *> varFresh(Lcls%d,Vv) ?} then", sep, vx, info->vNo);
      outMsg(O_IO(info->aux), "%s    throw .exception(\"Var #(V%d) in '%s' already inited\");\n", sep, vx,mnem);
      vx++;
      continue;
    }
    case SpHis: {
      outMsg(O_IO(info->aux), "%s  Lcls%d = foldRight(((V,Ls)=>Ls[V->.phiVar]),Lcls%d,V%d);\n",
             sep, info->vNo + 1, info->vNo, vx);
      outMsg(O_IO(info->aux), "%s  Lcls%d = foldRight(((V,Ls)=>Ls[V->.inited]),Lcls%d,V%d);\n",
             sep, info->vNo + 2, info->vNo, vx);

      vx++;
      info->vNo++;
      continue;
    }
    case Slit:
    case Si32:
    case Slcm:
    case Sart:
    case Ssym: {
      vx++;
      continue;
    }
    case SlVl: {
      outMsg(O_IO(info->aux), "%sif ~ V%d .<. Lbls then\n", sep, vx);
      outMsg(O_IO(info->aux), "%s  throw .exception(\"Label #(V%d) not in scope\");\n", sep, vx);
      vx++;
      continue;
    }
    case SbLk: {
      int32 vNo = vx++;
      outMsg(O_IO(info->aux), "    validBlock(V%d,Lcls%d,Lbls);\n", vNo, info->vNo);
      info->vNo++;
      continue;
    }

    default:
      fprintf(stderr, "Unknown instruction type code\n");
      exit(1);
    }
  }
}

void validIns(asmInfoPo info, char* mnem, ssaOp op, char* fmt) {
  clearStrBuffer(info->line);
  clearStrBuffer(info->aux);
  info->vNo = 0;
  int32 arity = 0;
  outMsg(info->outFl, "  validIns(.i%s", capitalize(mnem));
  if (*fmt != '\0') {
    outMsg(info->outFl, "(");
    genArgs(info->outFl, fmt, &arity);
    outMsg(info->outFl, ")");
  }
  outMsg(info->outFl, ", Lcls%d, Lbls) => valof{\n", info->vNo);

  genArgValidation(info, fmt, mnem, arity);

  integer auxLen;
  char* aux = getTextFromBuffer(info->aux, &auxLen);
  outMsg(info->outFl, "%S", aux, auxLen);

  integer lineLen;
  char* line = getTextFromBuffer(info->line, &lineLen);
  outMsg(info->outFl, "%s", line);
  outMsg(info->outFl, "    valis Lcls%d\n", info->vNo);
  outMsg(info->outFl, "  }\n");
}

static void generateVerify(hashPo vars) {
  // Set up the verification code
  strBufferPo showBuff = newStringBuffer();
  strBufferPo lineBuff = newStringBuffer();
  strBufferPo auxBuff = newStringBuffer();

  AsmInfoRecord info = {.outFl = O_IO(showBuff), .line = lineBuff, .aux = auxBuff, .vNo = 0};

#undef instr
#define instr(M, Fmt) validIns(&info, #M, s##M, Fmt);

#include "ssaInstructions.h"

  integer showLen;
  char* verifyCode = getTextFromBuffer(showBuff, &showLen);
  hashPut(vars, "Valid", verifyCode);
}
