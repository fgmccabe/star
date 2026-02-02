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

ReturnStatus g__cell_future(enginePo P) {
  termPo a1 = popVal(P);
  ValueReturn ret = s__cell_future(P, a1);

  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__futureIsResolved(enginePo P, termPo f){
  return normalReturn(futureIsResolved(C_FUTURE(f),processHeap(P)) ? trueEnum : falseEnum);
}

ReturnStatus g__futureIsResolved(enginePo P) {
  termPo f = popVal(P);
  ValueReturn ret = s__futureIsResolved(P,f);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__futureIsAccepted(enginePo P, termPo f){
  return normalReturn(futureIsAccepted(C_FUTURE(f)) ? trueEnum : falseEnum);
}

ReturnStatus g__futureIsAccepted(enginePo P) {
  termPo f = popVal(P);
  ValueReturn ret = s__futureIsAccepted(P,f);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__futureIsRejected(enginePo P, termPo f){
  return normalReturn(futureIsRejected(C_FUTURE(f)) ? trueEnum : falseEnum);
}

ReturnStatus g__futureIsRejected(enginePo P) {
  termPo f = popVal(P);
  ValueReturn ret = s__futureIsRejected(P,f);
  pshVal(P,ret.value);
  return ret.status;
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

ReturnStatus g__resolveFuture(enginePo P) {
  termPo f = popVal(P);
  termPo v = popVal(P);
  ValueReturn ret = s__resolveFuture(P,f,v);
  pshVal(P,ret.value);
  return ret.status;
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

ReturnStatus g__rejectFuture(enginePo P) {
  termPo f = popVal(P);
  termPo v = popVal(P);
  ValueReturn ret = s__rejectFuture(P,f,v);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__futureVal(enginePo P, termPo f){
  futurePo ft = C_FUTURE(f);

  if (futureIsAccepted(ft))
    return normalReturn(futureValue(ft));
  else
    return abnormalReturn(futureValue(ft));
}

ReturnStatus g__futureVal(enginePo P) {
  termPo f = popVal(P);

  ValueReturn ret = s__futureVal(P,f);

  pshVal(P,ret.value);
  return ret.status;
}

