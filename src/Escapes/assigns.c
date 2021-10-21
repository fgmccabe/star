//
// Created by Francis McCabe on 3/5/18.
//

#include <arith.h>
#include <strings.h>
#include <globals.h>
#include "assigns.h"
#include "cellP.h"

ReturnStatus g__cell(processPo p, ptrPo tos) {
  termPo content = tos[0];
  cellPo cell = newCell(processHeap(p), content);

  return (ReturnStatus){.ret=Ok, .result=(termPo) cell};
}

ReturnStatus g__get(processPo p, ptrPo tos) {
  cellPo cell = C_CELL(tos[0]);
  return (ReturnStatus){.ret=Ok, .result=getCell(cell)};
}

ReturnStatus g__assign(processPo p, ptrPo tos) {
  termPo Content = tos[1];
  cellPo Cell = C_CELL(tos[0]);

  setCell(Cell, Content);

  return (ReturnStatus){.ret=Ok, .result=unitEnum};
}

ReturnStatus g__overwrite(processPo p, ptrPo tos) {
  normalPo orig = C_NORMAL(tos[0]);
  normalPo newval = C_NORMAL(tos[1]);

  if (termArity(orig) == termArity(newval)) {
    orig->lbl = newval->lbl;
    for (integer ix = 0; ix < termArity(newval); ix++)
      orig->args[ix] = newval->args[ix];

    return (ReturnStatus){.ret=Ok, .result=(termPo)orig};
  } else {
    return (ReturnStatus){.ret=Error, .result= voidEnum};
  }
}
