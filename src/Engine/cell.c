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
static logical cellCmp(specialClassPo cl, termPo o1, termPo o2);
static integer cellHash(specialClassPo cl, termPo o);
static retCode cellDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo cellFinalizer(specialClassPo class, termPo o);

SpecialClass CellClass = {
  .clss = Null,
  .sizeFun = cellSize,
  .copyFun = cellCopy,
  .scanFun = cellScan,
  .finalizer = cellFinalizer,
  .compFun = cellCmp,
  .hashFun = cellHash,
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

  helper(&list->content, c);

  return o + CellCellCount;
}

termPo cellFinalizer(specialClassPo class, termPo o) {
  cellPo list = C_CELL(o);

  return o + CellCellCount;
}

logical cellCmp(specialClassPo cl, termPo o1, termPo o2) {
  cellPo c1 = C_CELL(o1);
  cellPo c2 = C_CELL(o2);
  return sameTerm(c1->content, c2->content);
}

integer cellHash(specialClassPo cl, termPo o) {
  return hash64(uniHash("cell") * 37 + termHash(C_CELL(o)->content));
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
  CellClass.clss = specialClass;
}

cellPo C_CELL(termPo t) {
  assert(hasClass(t, cellClass));
  return (cellPo) t;
}

cellPo newCell(heapPo H, termPo content) {
  int root = gcAddRoot(H, (ptrPo) (&content));
  cellPo cell = (cellPo) allocateObject(H, cellClass, CellCellCount);
  cell->content = content;
  gcReleaseRoot(H, root);
  return cell;
}

extern termPo getCell(cellPo cell) {
  return cell->content;
}

extern termPo setCell(cellPo cell, termPo e) {
  cell->content = e;
  return (termPo) cell;
}
