//
// Created by Francis McCabe on 1/9/24.
//
#include "futureP.h"
#include "globals.h"
#include "errorCodes.h"
#include "cell.h"
#include "consP.h"

ReturnStatus g__futureIsResolved(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Normal, .result=(termPo) (futureIsResolved(C_FUTURE(a1), h) ? trueEnum : falseEnum)};
}

ReturnStatus g__futureIsAccepted(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Normal, .result=(termPo) (futureIsAccepted(C_FUTURE(a1)) ? trueEnum : falseEnum)};
}

ReturnStatus g__futureIsRejected(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Normal, .result=(termPo) (futureIsRejected(C_FUTURE(a1)) ? trueEnum : falseEnum)};
}

ReturnStatus g__resolveFuture(heapPo h, termPo xc, termPo a1, termPo a2) {
  futurePo ft = C_FUTURE(a1);
  switch (resolveFuture(ft, a2)) {
    case Ok:{
      return (ReturnStatus) {.ret=Normal, .result=unitEnum};
    }
    default:
      return (ReturnStatus) {.ret=Abnormal, .result=hasValue};
  }
}

ReturnStatus g__rejectFuture(heapPo h, termPo xc, termPo a1, termPo a2) {
  futurePo ft = C_FUTURE(a1);
  switch (rejectFuture(ft, a2)) {
    case Ok: {
      return (ReturnStatus) {.ret=Normal, .result=unitEnum};
    }
    default:
      return (ReturnStatus) {.ret=Abnormal, .result=hasValue};
  }
}

ReturnStatus g__futureVal(heapPo h, termPo xc, termPo a1) {
  futurePo ft = C_FUTURE(a1);

  if (futureIsAccepted(ft))
    return (ReturnStatus) {.ret=Normal, .result=futureValue(ft)};
  else
    return (ReturnStatus) {.ret=Abnormal, .result=futureValue(ft)};
}
