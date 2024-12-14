//
// Created by Francis McCabe on 1/7/24.
//
#include "futureP.h"
#include "assert.h"
#include "globals.h"

static long futureSize(specialClassPo cl, termPo o);
static termPo futureCopy(specialClassPo cl, termPo dst, termPo src);
static termPo futureScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical futureCmp(specialClassPo cl, termPo o1, termPo o2);
static integer futureHash(specialClassPo cl, termPo o);
static retCode futureDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo futureFinalizer(specialClassPo class, termPo o);

SpecialClass FutureClass = {
  .clss = Null,
  .sizeFun = futureSize,
  .copyFun = futureCopy,
  .scanFun = futureScan,
  .finalizer = futureFinalizer,
  .compFun = futureCmp,
  .hashFun = futureHash,
  .dispFun = futureDisp
};

clssPo futureClass = (clssPo) &FutureClass;

void initFuture() {
  FutureClass.clss = specialClass;
}

futurePo C_FUTURE(termPo t) {
  assert(hasClass(t, futureClass));
  return (futurePo) t;
}

static integer prHash = 0;

futurePo makeFuture(heapPo H, termPo vl, futurePoll poll, void *cl, void *cl2) {
  int root = gcAddRoot(H, &vl);
  futurePo pr = (futurePo) allocateObject(H, futureClass, FutureCellCount);
  pr->val = vl;
  pr->state = notResolved;
  pr->poller = poll;
  pr->cl = cl;
  pr->cl2 = cl2;

  pr->hash = hash61(prHash++);
  gcReleaseRoot(H, root);
  return pr;
}

futurePo makeResolvedFuture(heapPo h, termPo val, futureState state) {
  int root = gcAddRoot(h, &val);
  futurePo ft = (futurePo) allocateObject(h, futureClass, FutureCellCount);
  ft->state = state;
  ft->val = val;
  ft->poller = Null;
  ft->cl = ft->cl2 = Null;
  gcReleaseRoot(h, root);
  return ft;
}

long futureSize(specialClassPo cl, termPo o) {
  return FutureCellCount;
}

termPo futureCopy(specialClassPo cl, termPo dst, termPo src) {
  futurePo si = C_FUTURE(src);
  futurePo di = (futurePo) (dst);
  *di = *si;
  return (termPo) di + FutureCellCount;
}

termPo futureScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  futurePo ft = C_FUTURE(o);

  helper(&ft->val, c);
  return (termPo) (o + FutureCellCount);
}

termPo futureFinalizer(specialClassPo class, termPo o) {
  futurePo ft = C_FUTURE(o);

  ft->val = voidEnum;

  return (termPo) (o + FutureCellCount);
}

logical futureCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer futureHash(specialClassPo cl, termPo o) {
  futurePo ft = C_FUTURE(o);
  return (integer) ft->hash;
}

static char *stateName(futureState st) {
  switch (st) {
    case notResolved:
      return "unset";
    case isAccepted:
      return "set";
    case isRejected:
      return "rejected";
  }
}

static retCode futureDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  futurePo ft = C_FUTURE(t);
  return outMsg(out, "<<future:0x%x[%s]%T>>", ft->hash, stateName(ft->state), ft->val);
}

logical futureIsResolved(futurePo t, heapPo h) {
  if (t->state == notResolved && t->poller != Null)
    t->poller(t, h, t->cl, t->cl2);
  return t->state != notResolved;
}

logical futureIsAccepted(futurePo t) {
  return t->state == isAccepted;
}

logical futureIsRejected(futurePo t) {
  return t->state == isRejected;
}

retCode resolveFuture(futurePo p, termPo vl) {
  if (p->state == notResolved) {
    p->state = isAccepted;
    p->val = vl;
    return Ok;
  } else
    return Error;
}

retCode rejectFuture(futurePo p, termPo ex) {
  if (p->state == notResolved) {
    p->state = isRejected;
    p->val = ex;
    return Ok;
  } else
    return Error;
}

termPo futureValue(futurePo p) {
  return p->val;
}
