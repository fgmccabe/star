//
// Created by Francis McCabe on 3/5/18.
//

#include <globals.h>
#include "assigns.h"
#include "cellP.h"

ValueReturn s__cell(enginePo P, termPo vl)
{
  return normalReturn((termPo)newCell(processHeap(P),vl));
}

ReturnStatus g__cell(enginePo P)
{
  ValueReturn ret = s__cell(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__get(enginePo P, termPo cl)
{
  return normalReturn(getCell(C_CELL(cl)));
}

ReturnStatus g__get(enginePo P)
{
  ValueReturn ret = s__get(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__assign(enginePo P, termPo c, termPo v)
{
  cellPo Cell = C_CELL(c);
  setCell(Cell, v);
  return normalReturn(voidEnum);
}

ReturnStatus g__assign(enginePo P)
{
  termPo cell = popVal(P);
  termPo vl = popVal(P);
  ValueReturn ret = s__assign(P, cell, vl);
  pshVal(P, ret.value);
  return ret.status;
}
