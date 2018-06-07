//
// Created by Francis McCabe on 6/7/18.
//

#include "futureP.h"
#include <assert.h>
#include <globals.h>
#include "heapP.h"

static long futureSize(specialClassPo cl, termPo o);
static termPo futureCopy(specialClassPo cl, termPo dst, termPo src);
static termPo futureScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static comparison futureCmp(specialClassPo cl, termPo o1, termPo o2);
static integer futureHash(specialClassPo cl, termPo o);
static retCode futureDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass FutureClass = {
  .clss = Null,
  .sizeFun = futureSize,
  .copyFun = futureCopy,
  .scanFun = futureScan,
  .compFun = futureCmp,
  .hashFun = futureHash,
  .dispFun = futureDisp
};

clssPo futureClass = (clssPo) &FutureClass;

long futureSize(specialClassPo cl, termPo o) {
  return FutureCellCount;
}

termPo futureCopy(specialClassPo cl, termPo dst, termPo src) {
  futurePo si = C_FUTURE(src);
  futurePo di = (futurePo) dst;
  *di = *si;

  return (termPo) di + FutureCellCount;
}

termPo futureScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  futurePo list = C_FUTURE(o);

  helper(&list->content, c);
  helper(&list->generator,c);

  return o + FutureCellCount;
}

comparison futureCmp(specialClassPo cl, termPo o1, termPo o2) {
  futurePo c1 = C_FUTURE(o1);
  futurePo c2 = C_FUTURE(o2);
  return compareTerm(c1->generator, c2->generator);
}

integer futureHash(specialClassPo cl, termPo o) {
  // The content is unreliable, so dont hash on it
  return hash64(uniHash("future") * 37 + termHash(C_FUTURE(o)->generator));
}

retCode futureDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  futurePo future = C_FUTURE(t);

  retCode ret = outStr(out, "{");
  if (ret == Ok) {
    if (depth > 0){
      ret = dispTerm(out, future->generator, precision, depth - 1, alt);
      if(ret==Ok)
        ret = outStr(out,"->");
      if(ret==Ok)
        ret = dispTerm(out, future->content, precision, depth - 1, alt);
    }
    else
      ret = outStr(out, "..");
  }
  if (ret == Ok)
    ret = outStr(out, "}");
  return ret;
}

void initFuture() {
  FutureClass.clss = specialClass;
}

futurePo C_FUTURE(termPo t) {
  assert(hasClass(t, futureClass));
  return (futurePo) t;
}

futurePo newFuture(heapPo H, termPo generator) {
  int root = gcAddRoot(H, (ptrPo) (&generator));
  futurePo future = (futurePo) allocateObject(H, futureClass, FutureCellCount);
  future->generator = generator;
  future->content = voidEnum;
  gcReleaseRoot(H, root);
  return future;
}

termPo getFutureValue(futurePo cell) {
  return cell->content;
}

termPo getFutureGenerator(futurePo cell) {
  return cell->generator;
}

termPo setFutureValue(futurePo cell, termPo e) {
  cell->content = e;
  return (termPo) cell;
}

logical futureHasValue(futurePo ft){
  return (logical)(ft->content!=voidEnum);
}
