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

  helper(&list->content, c);

  return o + ClosureCellCount;
}

termPo closureFinalizer(specialClassPo class, termPo o) {
  return o + ClosureCellCount;
}

logical closureCmp(specialClassPo cl, termPo o1, termPo o2) {
  closurePo c1 = C_CLOSURE(o1);
  closurePo c2 = C_CLOSURE(o2);
  return c1->lbl == c2->lbl && sameTerm(c1->content, c2->content);
}

integer closureHash(specialClassPo cl, termPo o) {
  closurePo clo = C_CLOSURE(o);
  return hash61(((uniHash)("closure") * 37 + termHash(clo->content)) * 37 + labelHash(clo->lbl));
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
        ret = dispTerm(out, closure->content, precision, depth - 1, alt);
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

closurePo C_CLOSURE(termPo t) {
  assert(hasClass(t, closureClass));
  return (closurePo) t;
}

logical isClosure(termPo t) {
  return hasClass(t, closureClass);
}

closurePo newClosure(heapPo H, labelPo lbl, termPo content) {
  int root = gcAddRoot(H, (ptrPo) (&content));
  closurePo closure = (closurePo) allocateObject(H, closureClass, ClosureCellCount);
  closure->content = content;
  closure->lbl = lbl;
  gcReleaseRoot(H, root);
  return closure;
}

labelPo closureLabel(closurePo cl) {
  return cl->lbl;
}

termPo closureFree(closurePo cl) {
  return cl->content;
}
