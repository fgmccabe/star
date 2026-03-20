//
// Created by Francis McCabe on 3/5/18.
//

#include "cellP.h"
#include <assert.h>
#include "heapP.h"
#include "labelsP.h"

static long cellSize(builtinClassPo cl, termPo o);
static termPo cellCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo cellScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical cellCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer hashCell(builtinClassPo cl, termPo o);
static retCode cellDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo cellFinalizer(builtinClassPo class, termPo o);

BuiltinTerm CellClass = {
  .sizeFun = cellSize,
  .copyFun = cellCopy,
  .scanFun = cellScan,
  .finalizer = cellFinalizer,
  .compFun = cellCmp,
  .hashFun = hashCell,
  .dispFun = cellDisp
};

builtinClassPo cellClass = &CellClass;
int32 cellIndex;

long cellSize(builtinClassPo cl, termPo o) {
  return CellCellCount;
}

termPo cellCopy(builtinClassPo cl, termPo dst, termPo src) {
  cellPo si = C_CELL(src);
  cellPo di = (cellPo) dst;
  *di = *si;

  return (termPo) di + CellCellCount;
}

termPo cellScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o) {
  cellPo cell = C_CELL(o);

  if(cell->content!=Null)
    helper(&cell->content, c);

  return o + CellCellCount;
}

termPo cellFinalizer(builtinClassPo class, termPo o) {
  return o + CellCellCount;
}

logical cellCmp(builtinClassPo cl, termPo o1, termPo o2) {
  return o1==o2;
}

integer hashCell(builtinClassPo cl, termPo o) {
  syserr("not permitted to take hash of assignment cell");
  return 0; // unreachable
}

retCode cellDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  cellPo cell = C_CELL(t);

  retCode ret = outStr(out, "{");
  if (ret == Ok) {
    if (depth > 0)
      ret = dispTerm(out, cell->content, precision, depth - 1, alt);
    else
      ret = outStr(out, "..");
  }
  if (ret == Ok)
    ret = outStr(out, "}");
  return ret;
}

void initCell() {
  CellClass.special.lblIndex = specialIndex;
  cellIndex = standardIndex(cellClass);
}

logical isCell(termPo t){
  return hasIndex(t,cellIndex);
}

cellPo C_CELL(termPo t) {
  assert(isCell(t));
  return (cellPo) t;
}

cellPo newCell(heapPo H, termPo content) {
  int root = gcAddRoot(H, (ptrPo) (&content));
  cellPo cell = (cellPo) allocateObject(H, cellIndex, CellCellCount);
  cell->content = content;
  gcReleaseRoot(H, root);
  return cell;
}

termPo getCell(cellPo cell) {
  return cell->content;
}

termPo setCell(cellPo cell, termPo e) {
  cell->content = e;
  return (termPo) cell;
}
