//
// Created by Francis McCabe on 3/26/25.
//

#include <stdlib.h>
#include "eitherP.h"
#include "constantsP.h"
#include "globals.h"

static hashPo constantKeys = Null;
ptrPo constAnts = Null;
static int32 nextConstant = 0;
static int32 constantPoolSize = 0;

static integer constantHash(void *n);
static comparison cmpConstants(void *n1, void *n2);

void initConstants() {
  constantKeys = newHash(4096, constantHash, cmpConstants, Null);
  constAnts = (ptrPo)malloc(sizeof(termPo) * 4096);
  constantPoolSize = 4096;
  constAnts[nextConstant++] = voidEnum; // First entry cannot be used
}

termPo getConstant(int32 key) {
  assert(key >= 0 && key < nextConstant);
  return constAnts[key];
}

int32 lookupConstant(termPo t) {
  constAnts[0] = t;
  int32 constNo = (int32)(integer)hashGet(constantKeys, (void*)(integer)0);
  constAnts[0] = Null;
  return constNo;
}

int32 defineConstantLiteral(termPo t) {
  int32 key = lookupConstant(t);
  if (key == 0) {
    if (nextConstant == constantPoolSize) {
      constantPoolSize *= 2;
      constAnts = (termPo*)realloc(constAnts, sizeof(termPo) * constantPoolSize);
      if (constAnts == Null) {
        syserr("Could not reallocate constants vector");
      }
    }
    int32 cx = nextConstant++;
    constAnts[cx] = t;

    hashPut(constantKeys, (void*)(integer)cx, (void*)(integer)cx);

    return cx;
  }

  return key;
}

logical isDefinedConstant(int32 key) {
  if (key >= 0 && key < nextConstant) {
    termPo constant = getConstant(key);
    return constant != Null && lookupConstant(constant) == key;
  }
  else
    return False;
}

void markConstants(gcSupportPo G) {
  for (int32 ix = 0; ix < nextConstant; ix++) {
    constAnts[ix] = markPtr(G, &constAnts[ix]);
  }
}

retCode scanConstants(termHelper helper, void *cl) {
  retCode ret = Ok;
  for (int32 ix = 0; ret==Ok && ix < nextConstant; ix++) {
    ret = helper(&constAnts[ix], cl);
  }
  return ret;
}

void dumpConstants() {
  for (int32 ix = 0; ix < nextConstant; ix++) {
    if (constAnts[ix] != Null)
      outMsg(logFile, "constant %d: %T\n", ix, constAnts[ix]);
  }
  flushOut();
}

static integer constantHash(void *n) {
  return termHash(constAnts[(integer)n]);
}

static comparison cmpConstants(void *n1, void *n2) {
  return compTerm(constAnts[(integer)n1], constAnts[(integer)n2]);
}
