//
// Created by Francis McCabe on 10/2/21.
//

#include <assert.h>
#include "thunkP.h"

static long thnkSize(specialClassPo cl, termPo o);
static termPo thnkCopy(specialClassPo cl, termPo dst, termPo src);
static termPo thnkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical thnkCmp(specialClassPo cl, termPo o1, termPo o2);
static integer thnkHash(specialClassPo cl, termPo o);
static retCode thnkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo thnkFinalizer(specialClassPo class, termPo o);

static integer thunkHash = 0;

SpecialClass ThunkClass = {
  .clss = Null,
  .sizeFun = thnkSize,
  .copyFun = thnkCopy,
  .scanFun = thnkScan,
  .finalizer = thnkFinalizer,
  .compFun = thnkCmp,
  .hashFun = thnkHash,
  .dispFun = thnkDisp
};

clssPo thunkClass = (clssPo) &ThunkClass;

void initThunk() {
  ThunkClass.clss = specialClass;
}

thunkPo C_THUNK(termPo t) {
  assert(hasClass(t, thunkClass));

  return (thunkPo) t;
}

thunkPo thunkVar(heapPo H, normalPo lam) {
  int root = gcAddRoot(H, (ptrPo) &lam);

  thunkPo thnk = (thunkPo) allocateObject(H, thunkClass, ThunkCellCount);

  thnk->content = Null;
  thnk->clss = thunkClass;
  thnk->lam = lam;
  thnk->hash = hash61(thunkHash++);

  gcReleaseRoot(H, root);

  return thnk;
}

long thnkSize(specialClassPo cl, termPo o) {
  return ThunkCellCount;
}

comparison thunkCmp(thunkPo lb1, thunkPo lb2) {
  return lb1 == lb2;
}

logical thnkCmp(specialClassPo cl, termPo o1, termPo o2) {
  thunkPo i1 = C_THUNK(o1);
  thunkPo i2 = C_THUNK(o2);

  return (logical) (thunkCmp(i1, i2) == same);
}

static integer thnkHash(specialClassPo cl, termPo o) {
  thunkPo thnk = C_THUNK(o);
  return thnk->hash;
}

termPo thnkCopy(specialClassPo cl, termPo dst, termPo src) {
  *((thunkPo) dst) = *((thunkPo) src);
  return dst + ThunkCellCount;
}

termPo thnkScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  thunkPo thnk = C_THUNK(o);

  if (thnk->content != Null)
    helper((ptrPo) (&thnk->content), c);

  helper((ptrPo)&thnk->lam, c);

  return o + ThunkCellCount;
}

termPo thnkFinalizer(specialClassPo class, termPo o) {
  return o + ThunkCellCount;
}

retCode thnkDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  thunkPo thnk = C_THUNK(t);

  if (thunkVal(thnk) != Null)
    return outMsg(out, "thunk: %,*T", depth, thnk->content);
  else
    return outMsg(out, "thunk: (undef)");
}

termPo thunkVal(thunkPo v) {
  return v->content;
}

termPo setThunk(thunkPo v, termPo e) {
  assert(e != Null);

  termPo prev = v->content;
  v->content = e;
  return prev;
}

logical thunkIsSet(thunkPo thnk) {
  return (logical) (thnk->content != Null);
}

normalPo thunkLam(thunkPo thunk) {
  return thunk->lam;
}
