//
// Created by Francis McCabe on 12/5/24
//

#include <assert.h>
#include "singleP.h"

static long singleSize(specialClassPo cl, termPo o);
static termPo singleCopy(specialClassPo cl, termPo dst, termPo src);
static termPo singleScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical singleCmp(specialClassPo cl, termPo o1, termPo o2);
static integer singleHash(specialClassPo cl, termPo o);
static retCode singleDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo singleFinalizer(specialClassPo class, termPo o);

SpecialClass SingleClass = {
    .clss = Null,
    .sizeFun = singleSize,
    .copyFun = singleCopy,
    .scanFun = singleScan,
    .finalizer = singleFinalizer,
    .compFun = singleCmp,
    .hashFun = singleHash,
    .dispFun = singleDisp
};

clssPo singleClass = (clssPo) &SingleClass;

void initSingle() {
  SingleClass.clss.clss = specialClass;
}

singlePo C_SINGLE(termPo t) {
  assert(hasClass(t, singleClass));

  return (singlePo) t;
}

singlePo singleVar(heapPo H) {
  singlePo single = (singlePo) allocateObject(H, singleClass, SingleCellCount);

  single->content = Null;
  single->clss.clss = singleClass;

  return single;
}

long singleSize(specialClassPo cl, termPo o) {
  return SingleCellCount;
}

logical singleCmp(specialClassPo cl, termPo o1, termPo o2) {
  singlePo i1 = C_SINGLE(o1);
  singlePo i2 = C_SINGLE(o2);

  return (logical) (i1==i2);
}

static integer singleHash(specialClassPo cl, termPo o) {
  logMsg(logFile,"not permitted to take hash of single assignment var");
  star_exit(99);
  return 0;
}

termPo singleCopy(specialClassPo cl, termPo dst, termPo src) {
  *((singlePo) dst) = *((singlePo) src);
  return dst + SingleCellCount;
}

termPo singleScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  singlePo single = C_SINGLE(o);

  if (single->content != Null)
    helper((ptrPo) (&single->content), c);

  return o + SingleCellCount;
}

termPo singleFinalizer(specialClassPo class, termPo o) {
  return o + SingleCellCount;
}

retCode singleDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  singlePo single = C_SINGLE(t);

  if (singleVal(single) != Null)
    return outMsg(out, "<!%,*T!>", depth, single->content);
  else
    return outMsg(out, "<!(undef)!>");
}

termPo singleVal(singlePo v) {
  return v->content;
}

termPo setSingle(singlePo v, termPo e) {
  assert(e != Null);

  termPo prev = v->content;
  v->content = e;
  return prev;
}

logical singleIsSet(singlePo single) {
  return (logical) (single->content != Null);
}


