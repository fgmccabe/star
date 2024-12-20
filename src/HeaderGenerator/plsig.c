//
// Created by Francis McCabe on 12/20/24.
//

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "signature.h"
#include "formio.h"
#include <ctype.h>
#include "plsig.h"

static char *dumpPrologConstraint(char *sig, ioPo out);
static char *dName(char *sig, ioPo out);
static char *dInt(char *sig, int *len);
static void dumpStdType(char *name, ioPo out);
static char *dSequence(char *sig, ioPo out);
static char *dFields(char *sig, ioPo out);

char *dumpPrologSig(char *sig, ioPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case intSig:
      dumpStdType("integer", out);
      break;
    case bigSig:
      dumpStdType("bigint", out);
      break;
    case fltSig:
      dumpStdType("float", out);
      break;
    case chrSig:
      dumpStdType("char", out);
      break;
    case strSig:
      dumpStdType("string", out);
      break;
    case logSig:
      dumpStdType("boolean", out);
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
      sig = dSequence(sig, out);
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

      assert(*sig == tplSig);
      dSequence(++sig, out);

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
      sig = dFields(sig, out);
      outStr(O_IO(out), ")");
      break;
    }
    case lstSig: {
      outStr(O_IO(out), "tpExp(");
      outStr(O_IO(out), "tpFun(");
      dumpStr("cons", out);
      outStr(O_IO(out), ",1),");
      sig = dumpPrologSig(sig, out);
      outStr(O_IO(out), ")");
      return sig;
    }
    case vctSig: {
      outStr(O_IO(out), "tpExp(");
      outStr(O_IO(out), "tpFun(");
      dumpStr("star.vector*vect", out);
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

char *dumpPrologConstraint(char *sig, ioPo out) {
  assert(sig != NULL && *sig != '\0');

  switch (*sig++) {
    case contractCon: {
      outStr(O_IO(out), "conTract(");
      sig = dName(sig, out);
      outStr(O_IO(out), ",[");
      sig = dSequence(sig, out);
      outStr(O_IO(out), "],[");
      sig = dSequence(sig, out);
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

static void dumpStdType(char *name, ioPo out) {
  outMsg(O_IO(out), "type(");
  dumpStr(name, out);
  outMsg(O_IO(out), ")");
  return;
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

void dumpStr(char *str, ioPo out) {
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

static int digitVal(char D) {
  assert(D >= '0' && D <= '9');
  return D - '0';
}

char *dInt(char *sig, int *len) {
  char K = *sig;
  int Ln = 0;
  while (isdigit(K)) {
    Ln = Ln * 10 + digitVal(K);
    K = *++sig;
  }
  *len = Ln;
  return sig;
}

static char *dSequence(char *sig, ioPo out) {
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

static char *dFields(char *sig, ioPo out) {
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


