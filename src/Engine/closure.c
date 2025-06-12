//
// Created by Francis McCabe on 4/11/23.
//
#include <assert.h>
#include "closureP.h"

static long closureSize(specialClassPo cl, termPo o);
static termPo closureCopy(specialClassPo cl, termPo dst, termPo src);
static termPo closureScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical closureCmp(specialClassPo cl, termPo o1, termPo o2);
static integer closureHash(specialClassPo cl, termPo o);
static retCode closureDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo closureFinalizer(specialClassPo class, termPo o);

SpecialClass ClosureClass = {
  .clss = Null,
  .sizeFun = closureSize,
  .copyFun = closureCopy,
  .scanFun = closureScan,
  .finalizer = closureFinalizer,
  .compFun = closureCmp,
  .hashFun = closureHash,
  .dispFun = closureDisp
};

clssPo closureClass = (clssPo) &ClosureClass;

long closureSize(specialClassPo cl, termPo o) {
  return ClosureCellCount;
}

termPo closureCopy(specialClassPo cl, termPo dst, termPo src) {
  closurePo si = C_CLOSURE(src);
  closurePo di = (closurePo) dst;
  *di = *si;

  return (termPo) di + ClosureCellCount;
}

termPo closureScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  closurePo list = C_CLOSURE(o);

  helper(&list->free, c);

  return o + ClosureCellCount;
}

termPo closureFinalizer(specialClassPo class, termPo o) {
  return o + ClosureCellCount;
}

logical closureCmp(specialClassPo cl, termPo o1, termPo o2) {
  closurePo c1 = C_CLOSURE(o1);
  closurePo c2 = C_CLOSURE(o2);
  return c1->lbl == c2->lbl && sameTerm(c1->free, c2->free);
}

integer closureHash(specialClassPo cl, termPo o) {
  closurePo clo = C_CLOSURE(o);
  return hash61(((uniHash)("closure") * 37 + termHash(clo->free)) * 37 + labelHash(clo->lbl));
}

retCode closureDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  closurePo closure = C_CLOSURE(t);

  retCode ret = outStr(out, "<");
  if (ret == Ok) {
    if (depth > 0) {
      ret = showLbl(out, closure->lbl, 0, depth - 1, alt);

      if (ret == Ok) {
        ret = outStr(out, ":");
      }

      if (ret == Ok)
        ret = dispTerm(out, closure->free, precision, depth - 1, alt);
    } else
      ret = outStr(out, "..");
  }
  if (ret == Ok)
    ret = outStr(out, ">");
  return ret;
}

void initClosure() {
  ClosureClass.clss.clss = specialClass;
}

closurePo newClosure(heapPo H, labelPo lbl, termPo free) {
  int root = gcAddRoot(H, (ptrPo) (&free));
  closurePo closure = (closurePo) allocateObject(H, closureClass, ClosureCellCount);
  closure->free = free;
  closure->lbl = lbl;
  gcReleaseRoot(H, root);
  return closure;
}

labelPo closureLabel(closurePo cl) {
  return cl->lbl;
}

termPo closureFree(closurePo cl) {
  return cl->free;
}
