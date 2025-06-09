//
// Created by Francis McCabe on 1/9/24.
//
#include <assert.h>
#include "futureP.h"
#include "globals.h"
#include "errorCodes.h"
#include "cell.h"
#include "either.h"
#include "escape.h"

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

ReturnStatus g__cell_future(processPo P) {
  termPo a1 = popVal(P);
  assert(isCell(a1));
  pshVal(P, (termPo) makeFuture(processHeap(P), a1, pollCellFuture, Null, Null));
  return Normal;
}

ReturnStatus g__futureIsResolved(processPo P) {
  pshVal(P, futureIsResolved(C_FUTURE(popVal(P)), processHeap(P)) ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__futureIsAccepted(processPo P) {
  pshVal(P, futureIsAccepted(C_FUTURE(popVal(P))) ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__futureIsRejected(processPo P) {
  pshVal(P, futureIsRejected(C_FUTURE(popVal(P))) ? trueEnum : falseEnum);
  return Normal;
}

ReturnStatus g__resolveFuture(processPo P) {
  futurePo ft = C_FUTURE(popVal(P));
  switch (resolveFuture(ft, popVal(P))) {
    case Ok: {
      pshVal(P, unitEnum);
      return Normal;
    }
    default:
      pshVal(P, hasValue);
      return Abnormal;
  }
}

ReturnStatus g__rejectFuture(processPo P) {
  futurePo ft = C_FUTURE(popVal(P));
  switch (rejectFuture(ft, popVal(P))) {
    case Ok: {
      pshVal(P, unitEnum);
      return Normal;
    }
    default:
      pshVal(P, hasValue);
      return Abnormal;
  }
}

ReturnStatus g__futureVal(processPo P) {
  futurePo ft = C_FUTURE(popVal(P));

  pshVal(P, futureValue(ft));

  if (futureIsAccepted(ft))
    return Normal;

  else
    return Abnormal;
}
