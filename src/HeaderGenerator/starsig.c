//
// Created by Francis McCabe on 12/20/24.
//
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "signature.h"
#include "formio.h"
#include <ctype.h>
#include "starsig.h"

static char *dumpStarConstraint(char *sig, ioPo out);
static char *dName(char *sig, ioPo out);
static char *dInt(char *sig, int *len);
static void dumpStdType(char *name, ioPo out);
static char *dSequence(char *sig, ioPo out);
static char *dFields(char *sig, ioPo out);

char *dumpStarSig(char *sig, ioPo out) {
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
    case kvrSig: {
      outStr(out, ".kVar(");
      sig = dName(sig, out);
      outStr(out, ")");
      return sig;
    }
    case kfnSig: {
      outStr(out, ".kFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(out, ",");
      outInt(out, ar);
      outStr(out, ")");
      return sig;
    }
    case anySig:
      outStr(out, "_");
      break;
    case voidSig: {
      outStr(out, ".voidType");
      return sig;
    }

    case tpeSig: {
      outMsg(out, ".nomnal(");
      sig = dName(sig, out);
      outMsg(out, ")");
      return sig;
    }

    case refSig: {
      outStr(out, ".tpExp(.tpFun(\"ref\",1),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case tpfnSig: {
      outStr(out, ".tpFun(");
      int ar;
      sig = dInt(sig, &ar);
      sig = dName(sig, out);
      outStr(out, ",");
      outInt(out, ar);
      outStr(out, ")");
      return sig;
    }
    case tpeExpSig: {
      outStr(out, ".tpExp(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }

    case tplSig: {
      outStr(out, ".tupleType(");
      sig = dSequence(sig, out);
      outStr(out, ")");
      return sig;
    }
    case funSig: {
      outStr(out, ".tpExp(.tpExp(.tpFun(\"=>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case conSig: {
      outStr(out, ".tpExp(.tpExp(.tpFun(\"<=>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case throwSig: {
      outStr(out, ".tpExp(.tpExp(.tpExp(.tpFun(\"=>\",3),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case contSig: {
      outStr(out, ".tpExp(.tpExp(.tpFun(\"=>>\",2),");
      sig = dumpStarSig(sig, out);
      outStr(out, "),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case faceSig: {
      outStr(out, ".faceType(");
      sig = dFields(sig, out);
      outStr(out, ")");
      return sig;
    }
    case lstSig: {
      outStr(out, ".tpExp(");
      outStr(out, ".tpFun(");
      dumpStr("cons", out);
      outStr(out, ",1),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case vctSig: {
      outStr(out, ".tpExp(");
      outStr(out, ".tpFun(");
      dumpStr("star.vector*vect", out);
      outStr(out, ",1),");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    case allSig: {
      outStr(out, ".allType(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }

    case xstSig: {
      outStr(out, ".existType(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }

    case constrainedSig: {
      outStr(out, ".constrainedType(");
      sig = dumpStarSig(sig, out);
      outStr(out, ",");
      sig = dumpStarConstraint(sig, out);
      outStr(out, ")");
      return sig;
    }

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
      sig = dSequence(sig, out);
      outStr(out, "],[");
      sig = dSequence(sig, out);
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
    case raisesCon: {
      outStr(out, ".raisEs(");
      sig = dumpStarSig(sig, out);
      outStr(out, ")");
      return sig;
    }
    default:
      fprintf(stderr, "illegal constraint signature %s\n", sig);
      exit(99);
  }
}

void dumpStdType(char *name, ioPo out) {
  outMsg(out, ".nomnal(");
  dumpStr(name, out);
  outMsg(out, ")");
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
    sig = dumpStarSig(sig, out);
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
    sig = dumpStarSig(sig, out);
    outStr(out, ")");
    sep = ",";
  }
  outStr(out, "]");
  return sig;
}
