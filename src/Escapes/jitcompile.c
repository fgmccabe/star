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

ReturnStatus g__jit_compile(processPo P) {
  stringPo mtdName = C_STR(popVal(P));
  integer mLen = strLength(mtdName) + 1;
  char buff[mLen];

  copyChars2Buff(mtdName, buff, mLen);

  labelPo lbl = findLbl(buff, (int32) integerVal(popVal(P)));

  if (lbl == Null) {
    pshVal(P, eNOTFND);
    return Abnormal;
  }

  methodPo mtd = labelCode(lbl);
  if (mtd == Null) {
    pshVal(P, eNOTFND);
    return Abnormal;
  }

  char errMsg[MAXLINE];
  retCode ret = jitMethod(mtd, errMsg, NumberOf(errMsg));
  if (ret != Ok) {
    logMsg(logFile, "%s\n", errMsg);
    pshVal(P, eINVAL);
    return Abnormal;
  } else {
    pshVal(P, unitEnum);
    return Normal;
  }
}
