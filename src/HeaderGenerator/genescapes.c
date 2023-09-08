/* Generate a module, in either prolog or Star, that knows about escape codes */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <getopt.h>
#include "signature.h"
#include "formio.h"
#include "escodes.h"
#include <ctype.h>

enum {
  genProlog, genStar
} genMode = genProlog;
char *prefix = NULL;

int getOptions(int argc, char **argv) {
  int opt;

  while ((opt = getopt(argc, argv, "ps:")) >= 0) {
    switch (opt) {
      case 'p':
        genMode = genProlog;
        break;
      case 's':
        genMode = genStar;
        prefix = optarg;
        break;
      default:;
    }
  }
  return optind;
}

static void prologEscapeTypes(FILE *);

static void prologIsEscape(FILE *);

static void starEscapeTypes(FILE *);

static void starIsEscape(FILE *);

int main(int argc, char **argv) {
  int narg = getOptions(argc, argv);

  if (narg < 0) {
    fprintf(stdout, "bad args");
    exit(1);
  } else {
    FILE *out = stdout;

    if (narg < argc)
      out = fopen(argv[narg], "w");

    fprintf(out, "/* Automatically generated, do not edit */\n\n");

    switch (genMode) {
      case genProlog:
        fprintf(out, ":-module(escapes,[isEscape/2,escapeType/2]).\n\n");
        prologEscapeTypes(out);
        prologIsEscape(out);
        break;
      case genStar:
        fprintf(out, "%s{\n", prefix);
        fprintf(out, "  import star.\n");
        fprintf(out, "  import star.compiler.types.\n\n");
        starEscapeTypes(out);
        starIsEscape(out);
        fprintf(out, "}.\n");
        break;
    }
  }
}

typedef char *(*sigDump)(char *sig, strBufferPo out);

static void dumpStdType(char *name, strBufferPo out);

static void dumpStr(char *str, strBufferPo out);

static char *dInt(char *sig, int *len);

static char *dName(char *sig, strBufferPo out);

static char *dSequence(char *sig, sigDump dump, strBufferPo out);

static char *dTple(char *sig, sigDump dump, strBufferPo out);

static char *dFields(char *sig, sigDump dump, strBufferPo out);

static char *dumpPrologConstraint(char *sig, strBufferPo out);

char *dumpPrologSig(char *sig, strBufferPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case intSig:
      dumpStdType("star.core*integer", out);
      break;
    case bigSig:
      dumpStdType("star.core*bigint", out);
      break;
    case fltSig:
      dumpStdType("star.core*float", out);
      break;
    case chrSig:
      dumpStdType("star.core*char", out);
      break;
    case strSig:
      dumpStdType("star.core*string", out);
      break;
    case logSig:
      dumpStdType("star.core*boolean", out);
      break;
    case kvrSig:
      outStr(O_IO(out), "kVar(");
      sig = dName(sig, out);
      outStr(O_IO(out), ")");
      break;
    case kfnSig: {
      outStr(O_IO(out), "kFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      outInt(O_IO(out), ar);
      outStr(O_IO(out), ")");
      break;
    }
    case anySig:
      outStr(O_IO(out), "_");
      break;
    case voidSig: {
      outStr(O_IO(out), "voidType");
      break;
    }

    case tpeSig: {
      outMsg(O_IO(out), "type(");
      sig = dName(sig, out);
      outMsg(O_IO(out), ")");
      return sig;
    }

    case refSig: {
      outMsg(O_IO(out), "tpExp(tpFun(\"ref\",1),");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case tpfnSig: {
      outStr(O_IO(out), "tpFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      outInt(O_IO(out), ar);
      outStr(O_IO(out), ")");
      break;
    }
    case tpeExpSig: {
      outStr(O_IO(out), "tpExp(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      break;
    }

    case tplSig: {
      outStr(O_IO(out), "tplType(");
      sig = dSequence(sig, dumpPrologSig, out);
      outStr(O_IO(out), ")");
      break;
    }
    case funSig:
      outStr(O_IO(out), "funType(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    case conSig:
      outStr(O_IO(out), "consType(");
      sig = dTple(sig, dumpPrologSig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    case contSig:
      outStr(O_IO(out), "continType(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    case faceSig: {
      outStr(O_IO(out), "faceType(");
      sig = dFields(sig, dumpPrologSig, out);
      outStr(O_IO(out), ")");
      break;
    }
    case lstSig: {
      outStr(O_IO(out), "tpExp(");
      outStr(O_IO(out), "tpFun(");
      dumpStr("star.core*cons", out);
      outStr(O_IO(out), ",1),");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case allSig: {
      outStr(O_IO(out), "allType(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case xstSig: {
      outStr(O_IO(out), "existType(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case constrainedSig: {
      outStr(O_IO(out), "constrained(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologConstraint(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    default:
      fprintf(stderr, "illegal signature %s\n", sig);
      exit(99);
  }

  return sig;
}

char *dumpPrologConstraint(char *sig, strBufferPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case contractCon: {
      outStr(O_IO(out), "conTract(");
      sig = dName(sig, out);
      outStr(O_IO(out), ",[");
      sig = dSequence(sig, dumpPrologSig, out);
      outStr(O_IO(out), "],[");
      sig = dSequence(sig, dumpPrologSig, out);
      outStr(O_IO(out), "])");
      return sig;
    }
    case hasFieldCon: {
      outStr(O_IO(out), "implementsFace(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ", ");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case implicitCon: {
      outStr(O_IO(out), "implicit(");
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case raisesCon: {
      outStr(O_IO(out), "raises(");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    default:
      fprintf(stderr, "illegal constraint signature %s\n", sig);
      exit(99);
  }
}

static char *dumpStarConstraint(char *sig, strBufferPo out);

static char *dumpStarSig(char *sig, strBufferPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case intSig:
      dumpStdType("star.core*integer", out);
      break;
    case bigSig:
      dumpStdType("star.core*bigint", out);
      break;
    case fltSig:
      dumpStdType("star.core*float", out);
      break;
    case chrSig:
      dumpStdType("star.core*char", out);
      break;
    case strSig:
      dumpStdType("star.core*string", out);
      break;
    case logSig:
      dumpStdType("star.core*boolean", out);
      break;
    case kvrSig: {
      outStr(O_IO(out), ".nomnal(");
      sig = dName(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case kfnSig: {
      outStr(O_IO(out), ".kFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      outInt(O_IO(out), ar);
      outStr(O_IO(out), ")");
      return sig;
    }
    case anySig:
      outStr(O_IO(out), "_");
      break;
    case voidSig: {
      outStr(O_IO(out), ".voidType");
      return sig;
    }

    case tpeSig: {
      outMsg(O_IO(out), ".nomnal(");
      sig = dName(sig, out);
      outMsg(O_IO(out), ")");
      return sig;
    }

    case refSig: {
      outStr(O_IO(out), ".tpExp(.tpFun(\"ref\",1),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case tpfnSig: {
      outStr(O_IO(out), ".tpFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      outInt(O_IO(out), ar);
      outStr(O_IO(out), ")");
      return sig;
    }
    case tpeExpSig: {
      outStr(O_IO(out), ".tpExp(");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case tplSig: {
      outStr(O_IO(out), ".tupleType(");
      sig = dSequence(sig, dumpStarSig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case funSig: {
      outStr(O_IO(out), ".tpExp(.tpExp(.tpFun(\"=>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), "),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case conSig: {
      outStr(O_IO(out), ".tpExp(.tpExp(.tpFun(\"<=>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), "),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case contSig: {
      outStr(O_IO(out), ".tpExp(.tpExp(.tpFun(\"=>>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), "),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case faceSig: {
      outStr(O_IO(out), ".faceType(");
      sig = dFields(sig, dumpStarSig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case lstSig: {
      outStr(O_IO(out), ".tpExp(");
      outStr(O_IO(out), ".tpFun(");
      dumpStr("star.core*cons", out);
      outStr(O_IO(out), ",1),");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case allSig: {
      outStr(O_IO(out), ".allType(");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case xstSig: {
      outStr(O_IO(out), ".existType(");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    case constrainedSig: {
      outStr(O_IO(out), ".constrainedType(");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpStarConstraint(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }

    default:
      fprintf(stderr, "illegal signature %s\n", sig);
      exit(99);
  }

  return sig;
}

char *dumpStarConstraint(char *sig, strBufferPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case contractCon: {
      outStr(O_IO(out), ".conTract(");
      sig = dName(sig, out);
      outStr(O_IO(out), ",[");
      sig = dSequence(sig, dumpStarSig, out);
      outStr(O_IO(out), "],[");
      sig = dSequence(sig, dumpStarSig, out);
      outStr(O_IO(out), "])");
      return sig;
    }
    case hasFieldCon: {
      outStr(O_IO(out), ".hasField(");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ", ");
      int ln;
      sig = dInt(sig, &ln);
      assert(ln == 1);
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpStarSig(sig, out);
      sig = dInt(sig, &ln);
      assert(ln == 0);
      outStr(O_IO(out), ")");
      return sig;
    }
    case implicitCon: {
      outStr(O_IO(out), ".implicit(");
      sig = dName(sig, out);
      outStr(O_IO(out), ",");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case raisesCon: {
      outStr(O_IO(out), ".raisEs(");
      sig = dumpStarSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    default:
      fprintf(stderr, "illegal constraint signature %s\n", sig);
      exit(99);
  }
}

static void dumpStdType(char *name, strBufferPo out) {
  switch (genMode) {
    case genProlog:
      outMsg(O_IO(out), "type(");
      dumpStr(name, out);
      outMsg(O_IO(out), ")");
      return;
    case genStar:
      outMsg(O_IO(out), ".nomnal(");
      dumpStr(name, out);
      outMsg(O_IO(out), ")");
  }
}

static int digitVal(char D) {
  assert(D >= '0' && D <= '9');
  return D - '0';
}

static char *dInt(char *sig, int *len) {
  char K = *sig;
  int Ln = 0;
  while (isdigit(K)) {
    Ln = Ln * 10 + digitVal(K);
    K = *++sig;
  }
  *len = Ln;
  return sig;
}

static char *dTple(char *sig, sigDump dump, strBufferPo out) {
  assert(*sig == tplSig);
  return dSequence(++sig, dump, out);
}

static char *dSequence(char *sig, sigDump dump, strBufferPo out) {
  char *sep = "";
  outStr(O_IO(out), "[");
  while (*sig != '\0' && *sig != ')') {
    outStr(O_IO(out), sep);
    sig = dump(sig, out);
    sep = ",";
  }
  outStr(O_IO(out), "]");
  assert(*sig == ')');
  return ++sig;
}

static char *dFields(char *sig, sigDump dump, strBufferPo out) {
  int ar;
  sig = dInt(sig, &ar);
  char *sep = "";
  outStr(O_IO(out), "[");
  while (ar-- > 0) {
    outStr(O_IO(out), sep);
    outStr(O_IO(out), "(");
    sig = dName(sig, out);
    outStr(O_IO(out), ",");
    sig = dump(sig, out);
    outStr(O_IO(out), ")");
    sep = ",";
  }
  outStr(O_IO(out), "]");
  return sig;
}

static void dumpStr(char *str, strBufferPo out) {
  outByte(O_IO(out), '"');
  while (*str != '\0') {
    char c = *str++;
    switch (c) {
      case '\'':
      case '"':
      case '\\':
        outByte(O_IO(out), '\\');
        outByte(O_IO(out), (byte) c);
        break;
      default:
        outByte(O_IO(out), (byte) c);
    }
  }
  outByte(O_IO(out), '"');
}

static char *dName(char *sig, strBufferPo out) {
  char delim = *sig++;
  outByte(O_IO(out), '"');
  while (*sig != delim && *sig != '\0') {
    outByte(O_IO(out), (byte) *sig++);
  }
  outByte(O_IO(out), '"');
  return sig + 1;
}

#undef escape
#define escape(name, type, cmt) genStarEsc(out,buffer,#name,type,cmt);

static void genStarEsc(FILE *out, strBufferPo buffer, char *name, char *sig, char *cmt) {
  outStr(O_IO(buffer), "    ");
  dumpStr(name, buffer);
  outStr(O_IO(buffer), " => .some(");
  dumpStarSig(sig, buffer);
  outStr(O_IO(buffer), ").\n");

  integer len;
  char *text = (char *) getTextFromBuffer(buffer, &len);
  fprintf(out, "%s", text);
  clearStrBuffer(buffer);
}

static void starEscapeTypes(FILE *out) {
  strBufferPo buffer = newStringBuffer();

  fprintf(out, "  public escapeType:(string)=>option[tipe].\n");
  fprintf(out, "  escapeType(Es) => case Es in {\n");

#include "escapes.h"

  fprintf(out, "    _ default => .none.\n");
  fprintf(out, "  }\n");

  closeFile(O_IO(buffer));
}

#undef escape
#define escape(name, type, cmt) genPrIsEsc(out,buffer,#name,Esc##name);

static void genPrIsEsc(FILE *out, strBufferPo buffer, char *name, EscapeCode code) {
  outStr(O_IO(buffer), "isEscape(");
  dumpStr(name, buffer);
  outMsg(O_IO(buffer), ",%d).\n", code);

  integer len;
  char *text = (char *) getTextFromBuffer(buffer, &len);
  fprintf(out, "%s", text);
  clearStrBuffer(buffer);
}

static void prologIsEscape(FILE *out) {
  strBufferPo buffer = newStringBuffer();

#include "escapes.h"

  closeFile(O_IO(buffer));
}

#undef escape
#define escape(name, type, cmt) genStarIsEsc(out,buffer,#name,Esc##name);

static void genStarIsEsc(FILE *out, strBufferPo buffer, char *name, EscapeCode code) {
  outStr(O_IO(buffer), "    ");
  dumpStr(name, buffer);
  outMsg(O_IO(buffer), " => .some(%d).\n", code);

  integer len;
  char *text = (char *) getTextFromBuffer(buffer, &len);
  fprintf(out, "%s", text);
  clearStrBuffer(buffer);
}

static void starIsEscape(FILE *out) {
  strBufferPo buffer = newStringBuffer();

  fprintf(out, "\n  public isEscape:(string)=>option[integer].\n");
  fprintf(out, "  isEscape(Es) => case Es in {\n");

#include "escapes.h"

  fprintf(out, "    _ default => .none.\n");
  fprintf(out, "  }\n");

  closeFile(O_IO(buffer));
}

#undef escape
#define escape(name, type, cmt) genPrologEsc(out,buffer,#name,type,cmt);

static void genPrologEsc(FILE *out, strBufferPo buffer, char *name, char *sig, char *cmt) {
  outStr(O_IO(buffer), "escapeType(");
  dumpStr(name, buffer);
  outStr(O_IO(buffer), ",");
  dumpPrologSig(sig, buffer);
  outStr(O_IO(buffer), ").\n");

  integer len;
  char *text = (char *) getTextFromBuffer(buffer, &len);
  fprintf(out, "%s", text);
  clearStrBuffer(buffer);
}

static void prologEscapeTypes(FILE *out) {
  strBufferPo buffer = newStringBuffer();

#include "escapes.h"

  closeFile(O_IO(buffer));
}
