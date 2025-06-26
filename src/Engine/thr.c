//
// Created by Francis McCabe on 3/11/18.
//


#include <assert.h>
#include "thrP.h"

static long thrSize(specialClassPo cl, termPo o);
static termPo thrCopy(specialClassPo cl, termPo dst, termPo src);
static termPo thrScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical thrCmp(specialClassPo cl, termPo o1, termPo o2);
static integer thrHash(specialClassPo cl, termPo o);
static retCode thrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo thrFinalizer(specialClassPo class, termPo o);

SpecialClass ThredClass = {
  .clss = Null,
  .sizeFun = thrSize,
  .copyFun = thrCopy,
  .scanFun = thrScan,
  .finalizer = thrFinalizer,
  .compFun = thrCmp,
  .hashFun = thrHash,
  .dispFun = thrDisp
};

clssPo threadClass = (clssPo) &ThredClass;

pthread_key_t processKey;              /* Special key for thread specific */
integer live_processes = 0;

void initThr() {
  ThredClass.clss.clss = specialClass;
  pthread_key_create(&processKey, NULL);  /* create the processKey */
}

threadPo C_THREAD(termPo t) {
  assert(hasClass(t, threadClass));
  return (threadPo) t;
}

long thrSize(specialClassPo cl, termPo o) {
  return ThreadCellCount;
}

termPo thrCopy(specialClassPo cl, termPo dst, termPo src) {
  threadPo si = C_THREAD(src);
  threadPo di = (threadPo) dst;
  *di = *si;

  return (termPo) di + ThreadCellCount;
}

termPo thrScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return o + ThreadCellCount;
}

termPo thrFinalizer(specialClassPo class, termPo o) {
  return o + ThreadCellCount;
}

logical thrCmp(specialClassPo cl, termPo o1, termPo o2) {
  threadPo l1 = C_THREAD(o1);
  threadPo l2 = C_THREAD(o2);

  return (logical) (l1 == l2);
}

integer thrHash(specialClassPo cl, termPo o) {
  threadPo t = C_THREAD(o);
  return hash61((integer) t->process);
}

static retCode thrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  return outMsg(out, "<<thread@0x%x>>", t);
}

enginePo getThreadProcess(threadPo t) {
  return t->process;
}

threadPo allocateThread(heapPo H, enginePo pr) {
  threadPo thr = (threadPo) allocateObject(H, threadClass, ThreadCellCount);

  thr->clss.clss = threadClass;
  thr->process = pr;

  return thr;
}

threadPo newThread(enginePo p, heapPo h) {
  return allocateThread(h, p);
}
