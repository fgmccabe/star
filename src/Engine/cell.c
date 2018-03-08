//
// Created by Francis McCabe on 3/5/18.
//

#include "cellP.h"
#include <assert.h>
#include <globals.h>
#include "heapP.h"

static long cellSize(specialClassPo cl, termPo o);
static termPo cellCopy(specialClassPo cl, termPo dst, termPo src);
static termPo cellScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static retCode cellDisp(ioPo out, termPo t, long depth, logical alt);

SpecialClass CellClass = {
  .clss = Null,
  .sizeFun = cellSize,
  .copyFun = cellCopy,
  .scanFun = cellScan,
  .dispFun = cellDisp
};

clssPo cellClass = (clssPo) &CellClass;

long cellSize(specialClassPo cl, termPo o) {
  return CellCellCount;
}

termPo cellCopy(specialClassPo cl, termPo dst, termPo src) {
  cellPo si = C_CELL(src);
  cellPo di = (cellPo) dst;
  *di = *si;

  return (termPo) di + CellCellCount;
}

termPo cellScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  cellPo list = C_CELL(o);

  helper(list->content, c);

  return o + CellCellCount;
}

retCode cellDisp(ioPo out, termPo t, long depth, logical alt) {
  cellPo cell = C_CELL(t);

  retCode ret = outStr(out, "{");
  if (ret == Ok) {
    if (depth > 0)
      ret = dispTerm(out, cell->content, depth - 1, alt);
    else
      ret = outStr(out, "..");
  }
  if (ret == Ok)
    ret = outStr(out, "}");
  return ret;
}

void initCell() {
  CellClass.clss = specialClass;
}

cellPo C_CELL(termPo t) {
  assert(hasClass(t, cellClass));
  return (cellPo) t;
}

cellPo newCell(heapPo H, termPo content) {
  int root = gcAddRoot((ptrPo) (&content));
  cellPo cell = (cellPo) allocateObject(H, cellClass, CellCellCount);
  cell->content = content;
  gcReleaseRoot(root);
  return cell;
}

extern termPo getCell(cellPo cell) {
  return cell->content;
}

extern termPo setCell(cellPo cell, termPo e) {
  cell->content = e;
  return (termPo) cell;
}
