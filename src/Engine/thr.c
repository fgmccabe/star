//
// Created by Francis McCabe on 3/11/18.
//


#include <assert.h>

#include "labelsP.h"
#include "thrP.h"

static long thrSize(builtinClassPo cl, termPo o);
static termPo thrCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo thrScan(builtinClassPo cl, specialHelperFun helper, void* c, termPo o);
static logical thrCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer thrHash(builtinClassPo cl, termPo o);
static retCode thrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo thrFinalizer(builtinClassPo class, termPo o);

BuiltinTerm ThredClass = {
  .special = {},
  .sizeFun = thrSize,
  .copyFun = thrCopy,
  .scanFun = thrScan,
  .finalizer = thrFinalizer,
  .compFun = thrCmp,
  .hashFun = thrHash,
  .dispFun = thrDisp
};

builtinClassPo threadClass = &ThredClass;
int32 threadIndex;

pthread_key_t processKey; /* Special key for thread specific */
integer live_processes = 0;

void initThr() {
  ThredClass.special.lblIndex = specialIndex;
  threadIndex = standardIndex(threadClass);
  pthread_key_create(&processKey, NULL); /* create the processKey */
}

threadPo C_THREAD(termPo t) {
  assert(hasIndex(t, threadIndex));
  return (threadPo)t;
}

long thrSize(builtinClassPo cl, termPo o) {
  return ThreadCellCount;
}

termPo thrCopy(builtinClassPo cl, termPo dst, termPo src) {
  threadPo si = C_THREAD(src);
  threadPo di = (threadPo)dst;
  *di = *si;

  return (termPo)di + ThreadCellCount;
}

termPo thrScan(builtinClassPo cl, specialHelperFun helper, void* c, termPo o) {
  return o + ThreadCellCount;
}

termPo thrFinalizer(builtinClassPo class, termPo o) {
  return o + ThreadCellCount;
}

logical thrCmp(builtinClassPo cl, termPo o1, termPo o2) {
  threadPo l1 = C_THREAD(o1);
  threadPo l2 = C_THREAD(o2);

  return (logical)(l1 == l2);
}

integer thrHash(builtinClassPo cl, termPo o) {
  threadPo t = C_THREAD(o);
  return hash61((integer)t->process);
}

static retCode thrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  return outMsg(out, "<<thread@0x%x>>", t);
}

enginePo getThreadProcess(threadPo t) {
  return t->process;
}

threadPo allocateThread(heapPo H, enginePo pr) {
  threadPo thr = (threadPo)allocateObject(H, threadIndex, ThreadCellCount);
  thr->process = pr;

  return thr;
}

threadPo newThread(enginePo p, heapPo h) {
  return allocateThread(h, p);
}
