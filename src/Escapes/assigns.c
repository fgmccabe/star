//
// Created by Francis McCabe on 3/5/18.
//

#include <arith.h>
#include <strings.h>
#include <globals.h>
#include "assigns.h"
#include "cellP.h"
#include "single.h"
#include "errorCodes.h"

ReturnStatus g__cell(heapPo h, termPo a1) {
  cellPo cell = newCell(h, a1);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) cell};
}

ReturnStatus g__get(heapPo h, termPo a1) {
  cellPo cell = C_CELL(a1);
  return (ReturnStatus) {.ret=Ok, .result=getCell(cell)};
}

ReturnStatus g__assign(heapPo h, termPo a1, termPo a2) {
  cellPo Cell = C_CELL(a1);

  setCell(Cell, a2);

  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

ReturnStatus g__single(heapPo h) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) makeSingle(h)};
}

ReturnStatus g__singleIsSet(heapPo h, termPo a1) {
  return (ReturnStatus) {.ret=Ok, .result=(termPo) (singleHasValue(C_SINGLE(a1)) ? trueEnum : falseEnum)};
}

ReturnStatus g__singleVal(heapPo h, termPo a1) {
  singlePo ft = C_SINGLE(a1);
  if (singleHasValue(ft)) {
    return (ReturnStatus) {.ret=Ok, .result=getSingle(ft)};
  } else {
    return (ReturnStatus) {.ret=Error, .result=noValue};
  }
}

ReturnStatus g__singleSet(heapPo h, termPo a1, termPo a2) {
  singlePo ft = C_SINGLE(a1);
  if (setSingle(h, ft, a2) == Ok) {
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  } else {
    return (ReturnStatus) {.ret=Error, .result=hasValue};
  }
}
