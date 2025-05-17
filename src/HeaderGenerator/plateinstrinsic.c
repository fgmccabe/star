#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "signature.h"
#include "ooio.h"
#include "formioP.h"
#include "template.h"
#include <assert.h>
#include <ctype.h>
#include "formexts.h"

/* Generate a Prolog or Star module, that knows how to assemble a program */

enum {
  genProlog, genStar
} genMode = genProlog;

typedef enum {
  Last,
  NotLast
} TailEsc;

char *prefix = "star.comp.intrinsics";
char *templateFn = "intrinsics.star.plate";
char date[MAXLINE] = "";

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "pst:d:")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 's':
        genMode = genStar;
        break;
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

static void genPrologIntrinsic(ioPo out, char *name, char *tipe, char *op, char *cmt);
static void genStarIntrinsic(ioPo out, char *name, char *tipe, char *op, logical Alloc, TailEsc tailMode, char *cmt);

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
    strBufferPo mnemBuff = newStringBuffer();

    switch (genMode) {
      case genProlog:

#undef intrinsic
#define intrinsic(NM, Tp, Op, Alloc, Tail, cmt) genPrologIntrinsic(O_IO(mnemBuff),#NM,Tp,Op,cmt);

#include "intrinsics.h"
        break;
      case genStar:
#undef intrinsic
#define intrinsic(NM, Tp, Op, Alloc, Tail, cmt) genStarIntrinsic(O_IO(mnemBuff),#NM,Tp,Op,Alloc,Tail, cmt);

#include "intrinsics.h"
    }

    integer insLen;
    char *allCode = getTextFromBuffer(mnemBuff, &insLen);
    hashPut(vars, "Intrinsics", allCode);

    processTemplate(out, plate, vars, NULL, NULL);

    closeIo(out);
    exit(0);
  }
}

static char *dumpPrologSig(char *sig, ioPo out);
static char *capitalize(char *str);

static void genPrologIntrinsic(ioPo out, char *name, char *tipe, char *op, char *cmt) {
  outMsg(out, "isIntrinsic(\"%P\",", name);
  dumpPrologSig(tipe, out);
  if (tipe[0] == throwSig)
    outMsg(out, ",i%s,throwing).  %% %s\n", capitalize(op), cmt);
  else
    outMsg(out, ",i%s,noThrow).  %% %s\n", capitalize(op), cmt);
}

static char *dumpStarSig(char *sig, ioPo out);

static void genStarIntrinsic(ioPo out, char *name, char *tipe, char *op, logical Alloc, TailEsc tailMode, char *cmt) {
  outMsg(out, "    | \"%s\" => .some((", name);
  dumpStarSig(tipe, out);
  if (tipe[0] == throwSig)
    outMsg(out, ",(Lb)=>.i%s(Lb), %s, %s))  -- %s\n", capitalize(op), (Alloc ? ".true" : ".false"),
           (tailMode == Last ? ".noMore" : ".notLast"), cmt);
  else
    outMsg(out, ",(_)=>.i%s, %s, %s))  -- %s\n", capitalize(op), (Alloc ? ".true" : ".false"),
           (tailMode == Last ? ".noMore" : ".notLast"), cmt);
}

static char *dName(char *sig, ioPo out);
static char *dInt(char *sig,
int *len);
static char *dStarSequence(char *sig, ioPo out);
static char *dStarFields(char *sig, ioPo out);
static char *dumpStarConstraint(char *sig, ioPo out);

static char *dumpStarSig(char *sig, ioPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case intSig:
      outMsg(out, "intType");
      break;
    case bigSig:
      outMsg(out, "bigintType");
      break;
    case fltSig:
      outMsg(out, "fltType");
      break;
    case chrSig:
      outMsg(out, "chrType");
      break;
    case strSig:
      outMsg(out, "strType");
      break;
    case logSig:
      outMsg(out, "boolType");
      break;
    case kvrSig:
      outStr(out, ".nomnal(");
      sig = dName(sig, out);
      outStr(out, ")");
      break;
    case kfnSig: {
      outStr(out, ".kFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(out, ",");
      outInt(out, ar);
      outStr(out, ")");
      break;
    }
    case voidSig:
      outStr(out, ".voidType");
      break;
    case thisSig:
      outStr(out, "thisType");
      break;
    case tpeSig:
      outMsg(out, ".nomnal(");
      sig = dName(sig, out);
      outMsg(out, ")");
      break;
    case refSig: {
      outStr(out, ".tpExp(.tpFun(\"ref\",1),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    }
    case tpfnSig: {
      outStr(out, ".tpFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(out, ",");
      outInt(out, ar);
      outStr(out, ")");
      break;
    }
    case tpeExpSig:
      outStr(out, ".tpExp(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case tplSig:
      outStr(out, ".tupleType(");
      sig = dStarSequence(sig, out);
      outStr(out, ")");
      break;
    case funSig:
      outStr(out, ".tpExp(.tpExp(.tpFun(\"=>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case throwSig:
      outStr(out, ".tpExp(.tpExp(.tpExp(.tpFun(\"=>\",3),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case conSig:
      outStr(out, ".tpExp(.tpExp(.tpFun(\"<=>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case contSig:
      outStr(out, ".tpExp(.tpExp(.tpFun(\"=>>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case faceSig:
      outStr(out, ".faceType(");
      sig = dStarFields(sig, out);
      outStr(out, ")");
      break;
    case lstSig:
      outStr(out, ".tpExp(.tpFun(\"cons\",1),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case vctSig:
      outStr(out, ".tpExp(.tpFun(\"star.vector*vect\",1),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case allSig:
      outStr(out, ".allType(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case xstSig:
      outStr(out, ".existType(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      break;
    case constrainedSig:
      outStr(out, ".constrainedType(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarConstraint(sig, out);
      outStr(out, ")");
      break;
    default:
      fprintf(stderr, "illegal signature %s\n", sig);
      exit(99);
  }
  return sig;
}

char *dumpStarConstraint(char *sig, ioPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case contractCon: {
      outStr(out, ".conTract(");
      sig = dName(sig, out);
      outStr(out, ",[");
      sig = dStarSequence(sig, out);
      outStr(out, "],[");
      sig = dStarSequence(sig, out);
      outStr(out, "])");
      return sig;
    }
    case hasFieldCon: {
      outStr(out, ".hasField(");
      sig = dumpStarSig(sig, out);
      outStr(out, ", ");
      int ln;
      sig = dInt(sig, &ln);
      assert(ln == 1);
      sig = dName(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      sig = dInt(sig, &ln);
      assert(ln == 0);
      outStr(out, ")");
      return sig;
    }
    case implicitCon: {
      outStr(out, ".implicit(");
      sig = dName(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    default:
      fprintf(stderr, "illegal constraint signature %s\n", sig);
      exit(99);
  }
}

static char *dStarSequence(char *sig, ioPo out) {
  char *sep = "";
  outStr(out, "[");
  while (*sig != '\0' && *sig != ')') {
    outStr(out, sep);
    sig = dumpStarSig(sig, out);
    sep = ",";
  }
  outStr(out, "]");
  assert(*sig == ')');
  return ++sig;
}

static char *dStarFields(char *sig, ioPo out) {
  int ar;
  sig = dInt(sig, &ar);
  char *sep = "";
  outStr(out, "[");
  while (ar-- > 0) {
    outStr(out, sep);
    outStr(out, "(");
    sig = dName(sig, out);
    outStr(out, ",");
    sig = dumpStarSig(sig, out);
    outStr(out, ")");
    sep = ",";
  }
  outStr(out, "]");
  return sig;
}

static void dumpPrologStdType(char *name, ioPo out);
static char *dPrologTple(char *sig, ioPo out);
static char *dPrologSequence(char *sig, ioPo out);
static char *dPrologFields(char *sig, ioPo out);
static void dumpStr(char *str, ioPo out);

static char *dumpPrologConstraint(char *sig, ioPo out);

static char *dumpPrologSig(char *sig, ioPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case intSig:
      dumpPrologStdType("integer", out);
      break;
    case bigSig:
      dumpPrologStdType("bigint", out);
      break;
    case fltSig:
      dumpPrologStdType("float", out);
      break;
    case chrSig:
      dumpPrologStdType("char", out);
      break;
    case strSig:
      dumpPrologStdType("string", out);
      break;
    case logSig:
      dumpPrologStdType("boolean", out);
      break;
    case kvrSig:
      outStr(out, "kVar(");
      sig = dName(sig, out);
      outStr(out, ")");
      break;
    case kfnSig: {
      outStr(out, "kFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(out, ",");
      outInt(out, ar);
      outStr(out, ")");
      break;
    }
    case voidSig:
      outStr(out, "voidType");
      break;
    case thisSig:
      outStr(out, "thisType");
      break;
    case tpeSig:
      outMsg(out, "type(");
      sig = dName(sig, out);
      outMsg(out, ")");
      return sig;

    case refSig: {
      outMsg(out, "tpExp(tpFun(\"ref\",1),");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case tpfnSig: {
      outStr(out, "tpFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(out, ",");
      outInt(out, ar);
      outStr(out, ")");
      break;
    }
    case tpeExpSig:
      outStr(out, "tpExp(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      break;
    case tplSig:
      outStr(out, "tplType(");
      sig = dPrologSequence(sig, out);
      outStr(out, ")");
      break;
    case funSig:
      outStr(out, "funType(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    case throwSig:
      outStr(out, "funType(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    case conSig:
      outStr(out, "consType(");
      sig = dPrologTple(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    case contSig:
      outStr(out, "continType(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    case faceSig:
      outStr(out, "faceType(");
      sig = dPrologFields(sig, out);
      outStr(out, ")");
      break;
    case lstSig:
      outStr(out, "tpExp(");
      outStr(out, "tpFun(");
      dumpStr("cons", out);
      outStr(out, ",1),");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      break;
    case vctSig:
      outStr(out, "tpExp(");
      outStr(out, "tpFun(");
      dumpStr("star.vector*vect", out);
      outStr(out, ",1),");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      break;
    case allSig:
      outStr(out, "allType(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      break;
    case xstSig:
      outStr(out, "existType(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      break;
    case constrainedSig:
      outStr(out, "constrained(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ",");
      sig = dumpPrologConstraint(sig, out);
      outStr(out, ")");
      break;
    default:
      fprintf(stderr, "illegal signature %s\n", sig);
      exit(99);
  }
  return sig;
}

char *dumpPrologConstraint(char *sig, ioPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case contractCon: {
      outStr(out, "conTract(");
      sig = dName(sig, out);
      outStr(out, ",[");
      sig = dPrologSequence(sig, out);
      outStr(out, "],[");
      sig = dPrologSequence(sig, out);
      outStr(out, "])");
      return sig;
    }
    case hasFieldCon: {
      outStr(out, "implementsFace(");
      sig = dumpPrologSig(sig, out);
      outStr(out, ", ");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case implicitCon: {
      outStr(out, "implicit(");
      sig = dName(sig, out);
      outStr(out, ",");
      sig = dumpPrologSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    default:
      fprintf(stderr, "illegal constraint signature %s\n", sig);
      exit(99);
  }
}

static void dumpPrologStdType(char *name, ioPo out) {
  outMsg(out, "type(");
  dumpStr(name, out);
  outMsg(out, ")");
}

static int

digitVal(char D) {
  assert(D >= '0' && D <= '9');
  return D - '0';
}

static char *dInt(char *sig,
int *len) {
char K = *sig;
int
Ln = 0;
while (
isdigit(K)
) {
Ln = Ln * 10 + digitVal(K);
K = *++sig;
}
*
len = Ln;
return
sig;
}

static char *dPrologTple(char *sig, ioPo out) {
  assert(*sig == tplSig);
  return dPrologSequence(++sig, out);
}

static char *dPrologSequence(char *sig, ioPo out) {
  char *sep = "";
  outStr(out, "[");
  while (*sig != '\0' && *sig != ')') {
    outStr(out, sep);
    sig = dumpPrologSig(sig, out);
    sep = ",";
  }
  outStr(out, "]");
  assert(*sig == ')');
  return ++sig;
}

static char *dPrologFields(char *sig, ioPo out) {
  int ar;
  sig = dInt(sig, &ar);
  char *sep = "";
  outStr(out, "[");
  while (ar-- > 0) {
    outStr(out, sep);
    outStr(out, "(");
    sig = dName(sig, out);
    outStr(out, ",");
    sig = dumpPrologSig(sig, out);
    outStr(out, ")");
    sep = ",";
  }
  outStr(out, "]");
  return sig;
}

static void dumpStr(char *str, ioPo out) {
  outByte(out, '"');
  while (*str != '\0') {
    char c = *str++;
    switch (c) {
      case '\'':
      case '"':
      case '\\':
        outByte(out, '\\');
        outByte(out, (byte) c);
        break;
      default:
        outByte(out, (byte) c);
    }
  }
  outByte(out, '"');
}

static char *dName(char *sig, ioPo out) {
  char delim = *sig++;
  outByte(out, '"');
  while (*sig != delim && *sig != '\0') {
    outByte(out, (byte) *sig++);
  }
  outByte(out, '"');
  return sig + 1;
}

static char *capitalize(char *str) {
  static char buffer[128];
  strcpy(buffer, str);
  if (buffer[0] >= 'a' && buffer[0] <= 'z') {
    buffer[0] = (char) ('A' + (buffer[0] - 'a'));
  }
  return buffer;
}
