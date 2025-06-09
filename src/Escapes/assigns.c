//
// Created by Francis McCabe on 3/5/18.
//

#include <arith.h>
#include <strings.h>
#include <globals.h>
#include "assigns.h"
#include "cellP.h"

ReturnStatus g__cell(processPo P) {
  pshVal(P, (termPo) newCell(currentHeap, popVal(P)));
  return Normal;
}

ReturnStatus g__get(processPo P) {
  pshVal(P, getCell(C_CELL(popVal(P))));
  return Normal;
}

ReturnStatus g__assign(processPo P) {
  cellPo Cell = C_CELL(popVal(P));
  setCell(Cell, popVal(P));
  return Normal;
}
