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
ValueReturn s__jit_compile(enginePo P, termPo m, termPo ar) {
  stringPo mtdName = C_STR(m);
  integer mLen = strLength(mtdName) + 1;
  char buff[mLen];

  copyChars2Buff(mtdName, buff, mLen);

  labelPo lbl = findLbl(buff, (int32) integerVal(ar));

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
#else
ValueReturn s__jit_compile(enginePo P, termPo m) {
  return abnormalReturn(eNOPERM);
}
#endif
