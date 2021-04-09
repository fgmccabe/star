//
// Created by Francis McCabe on 2/21/21.
//

#include <assert.h>
#include "continuationP.h"

static long cntSize(specialClassPo cl, termPo o);
static termPo cntCopy(specialClassPo cl, termPo dst, termPo src);
static termPo cntScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical cntCmp(specialClassPo cl, termPo o1, termPo o2);
static integer cntHash(specialClassPo cl, termPo o);
static retCode cntDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass ContinuationClass = {
  .clss = Null,
  .sizeFun = cntSize,
  .copyFun = cntCopy,
  .scanFun = cntScan,
  .compFun = cntCmp,
  .hashFun = cntHash,
  .dispFun = cntDisp
};

clssPo continuationClass = (clssPo) &ContinuationClass;

static integer stackCount = 0;

void initContinuations() {
  ContinuationClass.clss = specialClass;
}

continuationPo C_CONTINUATION(termPo t) {
  assert(hasClass(t, continuationClass));
  return (continuationPo) t;
}

continuationPo allocateContinuation(heapPo H, stackPo stk) {
  continuationPo cnt = (continuationPo) allocateObject(H, stackClass, CONTINUATION_CELLCOUNT);
  cnt->stk = stk;
  cnt->state = continuationEmpty;
  return cnt;
}

long cntSize(specialClassPo cl, termPo o) {
  return CONTINUATION_CELLCOUNT;
}

termPo cntCopy(specialClassPo cl, termPo dst, termPo src) {
  continuationPo cc = C_CONTINUATION(src);
  continuationPo ds = (continuationPo) dst;
  *ds = *cc;                  // Copy the structural part

  return ((termPo) ds) + CONTINUATION_CELLCOUNT;
}

termPo cntScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  continuationPo cnt = C_CONTINUATION(o);

  helper((ptrPo) &cnt->stk, c);

  return o + CONTINUATION_CELLCOUNT;
}

logical cntCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer cntHash(specialClassPo cl, termPo o) {
  return 0;
}

static char *continuationState(ContinuationState st) {
  switch (st) {
    case continuationEmpty:
      return "empty";
    case continuationCaptured:
      return "captured";
    case continuationEntered:
      return "entered";
  }
}

retCode cntDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  continuationPo cnt = C_CONTINUATION(t);

  return outMsg(out, "<continuation (%s) %T>", continuationState(cnt->state), cnt->stk);
}

ContinuationState markContinuation(continuationPo cnt, ContinuationState state) {
  ContinuationState inuse = cnt->state;
  cnt->state = state;
  return inuse;
}
