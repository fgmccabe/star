//
// Created by Francis McCabe on 9/6/24.
//
// Give direct access to jit compilation

#include "jit.h"
#include "globals.h"
#include "labelsP.h"
#include "errorCodes.h"
#include "arith.h"

ReturnStatus g__jit_compile(heapPo h, termPo a1, termPo a2) {
  stringPo mtdName = C_STR(a1);
  integer mLen = strLength(mtdName) + 1;
  char buff[mLen];

  copyChars2Buff(mtdName, buff, mLen);

  labelPo lbl = findLbl(buff, (int32) integerVal(a2));

  if (lbl == Null)
    return (ReturnStatus) {.ret=Abnormal, .result=eNOTFND};

  methodPo mtd = labelCode(lbl);
  if (mtd == Null)
    return (ReturnStatus) {.ret=Abnormal, .result=eNOTFND};

  char errMsg[MAXLINE];
  retCode ret = jitMethod(mtd, errMsg, NumberOf(errMsg));
  if (ret != Ok) {
    logMsg(logFile, "%s\n", errMsg);
    return (ReturnStatus) {.ret=Abnormal, .result=eINVAL};
  } else
    return (ReturnStatus) {.ret=Normal, .result=unitEnum};
}
