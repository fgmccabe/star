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

ValueReturn s__cell_future(enginePo P, termPo f){
  assert(isCell(f));
  return normalReturn((termPo) makeFuture(processHeap(P), f, pollCellFuture, Null, Null));
}

ValueReturn s__futureIsResolved(enginePo P, termPo f){
  return normalReturn(futureIsResolved(C_FUTURE(f),processHeap(P)) ? trueEnum : falseEnum);
}

ValueReturn s__futureIsAccepted(enginePo P, termPo f){
  return normalReturn(futureIsAccepted(C_FUTURE(f)) ? trueEnum : falseEnum);
}

ValueReturn s__futureIsRejected(enginePo P, termPo f){
  return normalReturn(futureIsRejected(C_FUTURE(f)) ? trueEnum : falseEnum);
}

ValueReturn s__resolveFuture(enginePo P, termPo f, termPo vl){
  switch (resolveFuture(C_FUTURE(f), vl)) {
    case Ok: {
      return normalReturn(unitEnum);
    }
    default:
      return abnormalReturn(hasValue);
  }
}

ValueReturn s__rejectFuture(enginePo P, termPo f, termPo vl){
  switch (rejectFuture(C_FUTURE(f), vl)) {
    case Ok: {
      return normalReturn(unitEnum);
    }
    default:
      return abnormalReturn(hasValue);
  }
}

ValueReturn s__futureVal(enginePo P, termPo f){
  futurePo ft = C_FUTURE(f);

  if (futureIsAccepted(ft))
    return normalReturn(futureValue(ft));
  else
    return abnormalReturn(futureValue(ft));
}
