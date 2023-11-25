//
// Created by Francis McCabe on 9/10/21.
//


#include <labels.h>
#include "singleP.h"
#include "assert.h"
#include "option.h"
#include "globals.h"

static long futSize(specialClassPo cl, termPo o);
static termPo futCopy(specialClassPo cl, termPo dst, termPo src);
static termPo futScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical futCmp(specialClassPo cl, termPo o1, termPo o2);
static integer futHash(specialClassPo cl, termPo o);
static retCode futDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo futFinalizer(specialClassPo class, termPo o);

SpecialClass SingleClass = {
  .clss = Null,
  .sizeFun = futSize,
  .copyFun = futCopy,
  .scanFun = futScan,
  .finalizer = futFinalizer,
  .compFun = futCmp,
  .hashFun = futHash,
  .dispFun = futDisp
};

clssPo singleClass = (clssPo) &SingleClass;

void initSingle() {
  SingleClass.clss = specialClass;
}

singlePo C_SINGLE(termPo t) {
  assert(hasClass(t, singleClass));
  return (singlePo) t;
}

static integer singleHash = 0;

singlePo makeSingle(heapPo H) {
  singlePo ft = (singlePo) allocateObject(H, singleClass, SingleCellCount);
  ft->val = voidEnum;
  ft->hash = hash61(singleHash++);
  return ft;
}

long futSize(specialClassPo cl, termPo o) {
  return SingleCellCount;
}

termPo futCopy(specialClassPo cl, termPo dst, termPo src) {
  singlePo si = C_SINGLE(src);
  singlePo di = (singlePo) (dst);
  *di = *si;
  return (termPo) di + SingleCellCount;
}

termPo futScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  singlePo ft = C_SINGLE(o);

  helper(&ft->val, c);
  return (termPo) (o + SingleCellCount);
}

termPo futFinalizer(specialClassPo class, termPo o) {
  singlePo ft = C_SINGLE(o);

  ft->val = voidEnum;

  return (termPo) (o + SingleCellCount);
}

logical futCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer futHash(specialClassPo cl, termPo o) {
  singlePo ft = C_SINGLE(o);
  return (integer) ft->hash;
}

static retCode futDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  singlePo ft = C_SINGLE(t);
  return outMsg(out, "<<single:0x%x>>", ft->hash);
}

logical singleHasValue(singlePo t) {
  return t->val != voidEnum;
}

retCode setSingle(heapPo H, singlePo ft, termPo val) {
  if(singleHasValue(ft))
    return Error;
  else {
    ft->val = val;
    return Ok;
  }
}

termPo getSingle(singlePo f) {
  return f->val;
}


