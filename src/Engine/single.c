//
// Created by Francis McCabe on 12/5/24
//

#include <assert.h>

#include "labelsP.h"
#include "singleP.h"

static long singleSize(builtinClassPo cl, termPo o);
static termPo singleCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo singleScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical singleCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer singleHash(builtinClassPo cl, termPo o);
static retCode singleDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo singleFinalizer(builtinClassPo class, termPo o);

BuiltinTerm SingleClass = {
    .special = {},
    .sizeFun = singleSize,
    .copyFun = singleCopy,
    .scanFun = singleScan,
    .finalizer = singleFinalizer,
    .compFun = singleCmp,
    .hashFun = singleHash,
    .dispFun = singleDisp
};

builtinClassPo singleClass = &SingleClass;
int32 singleIndex;

void initSingle() {
  SingleClass.special.lblIndex = specialIndex;
  singleIndex = standardIndex(singleClass);
}

singlePo C_SINGLE(termPo t) {
  assert(hasIndex(t, singleIndex));

  return (singlePo) t;
}

singlePo singleVar(heapPo H) {
  singlePo single = (singlePo) allocateObject(H, singleIndex, SingleCellCount);

  single->content = Null;
  return single;
}

long singleSize(builtinClassPo cl, termPo o) {
  return SingleCellCount;
}

logical singleCmp(builtinClassPo cl, termPo o1, termPo o2) {
  singlePo i1 = C_SINGLE(o1);
  singlePo i2 = C_SINGLE(o2);

  return (logical) (i1==i2);
}

static integer singleHash(builtinClassPo cl, termPo o) {
  syserr("not permitted to take hash of single assignment var");
  return 0;
}

termPo singleCopy(builtinClassPo cl, termPo dst, termPo src) {
  *((singlePo) dst) = *((singlePo) src);
  return dst + SingleCellCount;
}

termPo singleScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o) {
  singlePo single = C_SINGLE(o);

  if (single->content != Null)
    helper((ptrPo) (&single->content), c);

  return o + SingleCellCount;
}

termPo singleFinalizer(builtinClassPo class, termPo o) {
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


