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

void initConstants() {
  constantKeys = newHash(4096, (hashFun) termHash, (compFun) compTerm, Null);
  constAnts = (ptrPo) malloc(sizeof(termPo) * 4096);
  constantPoolSize = 4096;
  constAnts[nextConstant++] = voidEnum; // First entry cannot be used
}

termPo getConstant(int32 key) {
  assert(key >= 0 && key < nextConstant);
  return constAnts[key];
}

int32 defineConstantLiteral(termPo t) {
  integer tx = (integer) hashGet(constantKeys, t);
  if (tx == (integer) Null) {
    if (nextConstant == constantPoolSize) {
      constantPoolSize *= 2;
      constAnts = (termPo *) realloc(constAnts, sizeof(termPo) * constantPoolSize);
      if (constAnts == Null) {
        syserr("Could not reallocate constants vector");
      }
    }
    int32 cx = nextConstant++;
    constAnts[cx] = t;

    hashPut(constantKeys, t, (void *) (integer) cx);

    return cx;
  }

  return (int32) tx;
}

logical isDefinedConstant(int32 key) {
  if (key >= 0 && key < nextConstant) {
    termPo constant = getConstant(key);
    return constant != Null && (integer) hashGet(constantKeys, constant) == (integer) key;
  } else
    return False;
}

void markConstants(gcSupportPo G) {
  for (int32 ix = 0; ix < nextConstant; ix++)
    constAnts[ix] = markPtr(G, &constAnts[ix]);
}

void dumpConstants() {
  for (int32 ix = 0; ix < nextConstant; ix++) {
    if (constAnts[ix] != Null)
      outMsg(logFile, "constant %d: %T\n", ix, constAnts[ix]);
  }
  flushOut();
}
