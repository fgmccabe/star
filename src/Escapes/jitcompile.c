//
// Created by Francis McCabe on 9/6/24.
//
// Give direct access to jit compilation

#include "jit.h"
#include "globals.h"
#include "labelsP.h"
#include "errorCodes.h"
#include "closure.h"

ReturnStatus g__jit_compile(heapPo h, termPo xc, termPo a1) {
  closurePo cl = C_CLOSURE(a1);
  methodPo mtd = labelCode(closureLabel(cl));

  if (mtd == Null)
    return (ReturnStatus) {.ret=Abnormal, .result=eNOTFND};
  else {
    char errMsg[MAXLINE];
    retCode ret = jitMethod(mtd, errMsg, NumberOf(errMsg));
    if (ret != Ok) {
      logMsg(logFile, "%s\n", errMsg);
      return (ReturnStatus) {.ret=Abnormal, .cont=xc, .result=eINVAL};
    } else
      return (ReturnStatus) {.ret=Normal, .result=unitEnum};
  }
}
