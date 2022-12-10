//
// Created by Francis McCabe on 9/10/21.
//


#include <labels.h>
#include <stack.h>
#include "futureP.h"
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

SpecialClass FutureClass = {
  .clss = Null,
  .sizeFun = futSize,
  .copyFun = futCopy,
  .scanFun = futScan,
  .finalizer = futFinalizer,
  .compFun = futCmp,
  .hashFun = futHash,
  .dispFun = futDisp
};

clssPo futureClass = (clssPo) &FutureClass;

void initFuture() {
  FutureClass.clss = specialClass;
}

futurePo C_FUTURE(termPo t) {
  assert(hasClass(t, futureClass));
  return (futurePo) t;
}

static integer futureHash = 0;

futurePo makeFuture(heapPo H, futureSetProc fut, void *cl) {
  futurePo ft = (futurePo) allocateObject(H, futureClass, FutureCellCount);
  ft->cont = Null;
  ft->val = voidEnum;
  ft->set = fut;
  ft->cl = cl;
  ft->hash = hash61(futureHash++);
  return ft;
}

long futSize(specialClassPo cl, termPo o) {
  return FutureCellCount;
}

termPo futCopy(specialClassPo cl, termPo dst, termPo src) {
  futurePo si = C_FUTURE(src);
  futurePo di = (futurePo) (dst);
  *di = *si;
  return (termPo) di + FutureCellCount;
}

termPo futScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  futurePo ft = C_FUTURE(o);

  helper((ptrPo) &ft->cont, c);
  helper(&ft->val, c);
  return (termPo) (o + FutureCellCount);
}

termPo futFinalizer(specialClassPo class, termPo o) {
  futurePo ft = C_FUTURE(o);

  ft->cont = Null;
  ft->val = voidEnum;

  return (termPo) (o + FutureCellCount);
}

logical futCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer futHash(specialClassPo cl, termPo o) {
  futurePo ft = C_FUTURE(o);
  return (integer) ft->hash;
}

static retCode futDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  futurePo ft = C_FUTURE(t);
  return outMsg(out, "<<future:0x%x>>", ft->hash);
}

logical futureIsSet(futurePo ft) {
  return isSome(ft->val);
}

retCode setFuture(heapPo H, futurePo ft, termPo val) {
  int root = gcAddRoot(H, &val);
  ft->val = (termPo) wrapSome(H, val);

  return Ok;
}


