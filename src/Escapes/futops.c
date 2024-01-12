//
// Created by Francis McCabe on 1/9/24.
//
#include "futureP.h"
#include "globals.h"
#include "errorCodes.h"
#include "cell.h"
#include "consP.h"

ReturnStatus g__futureIsResolved(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) (futureIsResolved(C_FUTURE(a1)) ? trueEnum : falseEnum)};
}

ReturnStatus g__futureIsAccepted(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) (futureIsAccepted(C_FUTURE(a1)) ? trueEnum : falseEnum)};
}

ReturnStatus g__futureIsRejected(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) (futureIsRejected(C_FUTURE(a1)) ? trueEnum : falseEnum)};
}

ReturnStatus g__resolveFuture(heapPo h, termPo xc, termPo a1, termPo a2) {
  futurePo ft = C_FUTURE(a1);
  switch (resolveFuture(ft, a2)) {
    case Ok:{
      int root = gcAddRoot(h, &a1);
      cellPo cell = C_CELL(futureQueue(ft));
      gcAddRoot(h, (ptrPo) &cell);

      termPo pair = (termPo) allocateCons(h, a1, getCell(cell));
      setCell(cell, pair);
      gcReleaseRoot(h,root);
      return (ReturnStatus) {.ret=Ok, .result=unitEnum};
    }
    default:
      return (ReturnStatus) {.ret=Error, .result=hasValue};
  }
}

ReturnStatus g__rejectFuture(heapPo h, termPo xc, termPo a1, termPo a2) {
  futurePo ft = C_FUTURE(a1);
  switch (rejectFuture(ft, a2)) {
    case Ok: {
      int root = gcAddRoot(h, &a1);
      cellPo cell = C_CELL(futureQueue(ft));
      gcAddRoot(h, (ptrPo) &cell);

      termPo pair = (termPo)allocateCons(h, a1, getCell(cell));
      setCell(cell, pair);
      gcReleaseRoot(h,root);
      return (ReturnStatus) {.ret=Ok, .result=unitEnum};
    }
    default:
      return (ReturnStatus) {.ret=Error, .result=hasValue};
  }
}

ReturnStatus g__futureVal(heapPo h, termPo xc, termPo a1) {
  futurePo ft = C_FUTURE(a1);

  if (futureIsResolved(ft))
    return (ReturnStatus) {.ret=Ok, .result=futureValue(ft)};
  else
    return (ReturnStatus) {.ret=Error, .result=noValue};
}
