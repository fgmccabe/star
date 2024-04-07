//
// Created by Francis McCabe on 1/9/24.
//
#include <assert.h>
#include "futureP.h"
#include "globals.h"
#include "errorCodes.h"
#include "cell.h"
#include "either.h"

static retCode pollCellFuture(futurePo ft, heapPo h, void *cl, void *cl2) {
  termPo f = futureValue(ft);

  assert(isCell(f));

  termPo fv = getCell(C_CELL(f));
  if (isNeither(fv))
    return Fail;
  else if (isEither(fv))
    return resolveFuture(ft, eitherValue(fv));
  else
    return rejectFuture(ft, orValue(fv));
}

ReturnStatus g__cell_future(heapPo h, termPo a1) {
  assert(isCell(a1));
  return (ReturnStatus) {.ret=Normal, .result=(termPo) makeFuture(h, a1, pollCellFuture, Null, Null)};
}

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
    case Ok: {
      return (ReturnStatus) {.ret=Normal, .result=unitEnum};
    }
    default:
      return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=hasValue};
  }
}

ReturnStatus g__rejectFuture(heapPo h, termPo xc, termPo a1, termPo a2) {
  futurePo ft = C_FUTURE(a1);
  switch (rejectFuture(ft, a2)) {
    case Ok: {
      return (ReturnStatus) {.ret=Normal, .result=unitEnum};
    }
    default:
      return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=hasValue};
  }
}

ReturnStatus g__futureVal(heapPo h, termPo xc, termPo a1) {
  futurePo ft = C_FUTURE(a1);

  if (futureIsAccepted(ft))
    return (ReturnStatus) {.ret=Normal, .result=futureValue(ft)};
  else
    return (ReturnStatus) {.ret=Abnormal, .cont = xc, .result=futureValue(ft)};
}
