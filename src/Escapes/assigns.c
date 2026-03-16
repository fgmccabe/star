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

ValueReturn s__get(enginePo P, termPo cl)
{
  return normalReturn(getCell(C_CELL(cl)));
}

ValueReturn s__assign(enginePo P, termPo c, termPo v)
{
  cellPo Cell = C_CELL(c);
  setCell(Cell, v);
  return normalReturn(voidEnum);
}
