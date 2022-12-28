//
// Created by Francis McCabe on 3/5/18.
//

#include <arith.h>
#include <strings.h>
#include <globals.h>
#include "assigns.h"
#include "cellP.h"

ReturnStatus g__cell(heapPo h, termPo a1) {
  cellPo cell = newCell(h, a1);

  return (ReturnStatus){.ret=Ok, .result=(termPo) cell};
}

ReturnStatus g__get(heapPo h, termPo a1) {
  cellPo cell = C_CELL(a1);
  return (ReturnStatus){.ret=Ok, .result=getCell(cell)};
}

ReturnStatus g__assign(heapPo h, termPo a1,termPo a2) {
  cellPo Cell = C_CELL(a1);

  setCell(Cell, a2);

  return (ReturnStatus){.ret=Ok, .result=unitEnum};
}

ReturnStatus g__overwrite(heapPo h, termPo a1,termPo a2) {
  normalPo orig = C_NORMAL(a1);
  normalPo newval = C_NORMAL(a2);

  if (termArity(orig) == termArity(newval)) {
    orig->lbl = newval->lbl;
    for (integer ix = 0; ix < termArity(newval); ix++)
      orig->args[ix] = newval->args[ix];

    return (ReturnStatus){.ret=Ok, .result=(termPo)orig};
  } else {
    return (ReturnStatus){.ret=Error, .result= unitEnum};
  }
}
