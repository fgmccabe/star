//
// Created by Francis McCabe on 1/6/18.
//


#include <assert.h>
#include "ooio.h"
#include "engine.h"
#include "arithP.h"

const int64 integerVal(termPo o){
  assert(isInteger(o));
  intPo ix = (intPo)o;
  return ix->ix;
}

integer integerHash(intPo ix){
  return ix->ix;
}

retCode g_int_plus(processPo p,ptrPo tos) {
  termPo Tx = *tos;
  return outMsg(logFile, "%ld\n", (int64) Tx);
}

