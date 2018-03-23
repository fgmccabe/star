//
// Created by Francis McCabe on 3/5/18.
//

#include "assigns.h"
#include "cellP.h"

ReturnStatus g__cell(processPo p, ptrPo tos) {
  termPo content = tos[0];
  cellPo cell = newCell(processHeap(p), content);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) cell};

  return ret;
}

ReturnStatus g__get(processPo p, ptrPo tos) {
  cellPo cell = C_CELL(tos[0]);
  ReturnStatus ret = {.ret=Ok, .rslt=getCell(cell)};
  return ret;
}

ReturnStatus g__assign(processPo p, ptrPo tos) {
  termPo Content = tos[1];
  cellPo Cell = C_CELL(tos[0]);

  setCell(Cell, Content);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) Cell};

  return ret;
}
