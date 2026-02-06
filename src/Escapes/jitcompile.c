//
// Created by Francis McCabe on 9/6/24.
//
// Give direct access to jit compilation

#include "jit.h"
#include "globals.h"
#include "labelsP.h"
#include "errorCodes.h"
#include "arith.h"
#include "escape.h"

#ifndef NOJIT
ValueReturn s__jit_compile(enginePo P, termPo m) {
  stringPo mtdName = C_STR(m);
  integer mLen = strLength(mtdName) + 1;
  char buff[mLen];

  copyChars2Buff(mtdName, buff, mLen);

  labelPo lbl = findLbl(buff, (int32) integerVal(popVal(P)));

  if (lbl == Null) {
    return abnormalReturn(eNOTFND);
  }

  methodPo mtd = labelMtd(lbl);
  if (mtd == Null) {
    return abnormalReturn(eNOTFND);
  }

  char errMsg[MAXLINE];
  retCode ret = jitMethod(mtd, errMsg, NumberOf(errMsg));
  if (ret != Ok) {
    logMsg(logFile, "%s\n", errMsg);
    return abnormalReturn(eINVAL);
  } else {
    return normalReturn(unitEnum);
  }
}

ReturnStatus g__jit_compile(enginePo P) {
  termPo m = popVal(P);
  ValueReturn ret = s__jit_compile(P, m);
  pshVal(P, ret.value);
  return ret.status;
}
#else
ValueReturn s__jit_compile(enginePo P, termPo m) {
  return abnormalReturn(eNOPERM);
}

ReturnStatus g__jit_compile(enginePo P) {
  popVal(P); // Drop arity & method name
  popVal(P);
  pshVal(P, eNOPERM);
  return Abnormal;
}
#endif
