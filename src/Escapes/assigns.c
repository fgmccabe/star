//
// Created by Francis McCabe on 3/5/18.
//

#include <arith.h>
#include <strings.h>
#include <globals.h>
#include "assigns.h"
#include "cellP.h"

ReturnStatus g__cell(enginePo P) {
  pshVal(P, (termPo) newCell(processHeap(P), popVal(P)));
  return Normal;
}

ReturnStatus g__get(enginePo P) {
  pshVal(P, getCell(C_CELL(popVal(P))));
  return Normal;
}

ReturnStatus g__assign(enginePo P) {
  cellPo Cell = C_CELL(popVal(P));
  setCell(Cell, popVal(P));
  return Normal;
}
