//
// Created by Francis McCabe on 11/21/22.
//

#include "continuationP.h"
#include "termP.h"
#include "stackP.h"
#include "globalsP.h"
#include <assert.h>

logical traceContinuations = False;      // continuation tracing

continuationPo C_CONTINUATION(termPo t) {
  assert(isContinuation(t));
  return (continuationPo) t;
}

static long cntSize(specialClassPo cl, termPo o);
static termPo cntCopy(specialClassPo cl, termPo dst, termPo src);
static termPo cntScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical cntCmp(specialClassPo cl, termPo o1, termPo o2);
static integer cntHash(specialClassPo cl, termPo o);
static retCode cntDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo cntFinalizer(specialClassPo class, termPo o);

SpecialClass ContinuationClass = {
  .clss = Null,
  .sizeFun = cntSize,
  .copyFun = cntCopy,
  .scanFun = cntScan,
  .finalizer = cntFinalizer,
  .compFun = cntCmp,
  .hashFun = cntHash,
  .dispFun = cntDisp
};

clssPo contClass = ((clssPo) &ContinuationClass);

void initContinuations() {
  ContinuationClass.clss = specialClass;
}

long cntSize(specialClassPo cl, termPo o) {
  return ContinuationCellCount;
}

termPo cntCopy(specialClassPo cl, termPo dst, termPo src) {
  continuationPo ss = C_CONTINUATION(src);
  continuationPo ds = (continuationPo) dst; // Dest not yet a valid structure
  *ds = *ss;                  // Copy the structural part

  return ((termPo) ds) + ContinuationCellCount;
}

termPo cntScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  continuationPo cont = C_CONTINUATION(o);

  assert(cont != Null);

  if (cont->stack != Null) {
#ifdef TRACEMEM
    if (traceContinuations) {
      outMsg(logFile, "scan continuation %d\n%_", cont);
    }
#endif
    helper((ptrPo) &cont->stack, c);
  }
  return o + ContinuationCellCount;
}

logical cntCmp(specialClassPo cl, termPo o1, termPo o2) {
  return o1 == o2;
}

integer cntHash(specialClassPo cl, termPo o) {
  continuationPo cont = C_CONTINUATION(o);

  return (cont->stack->hash) * 37 + cont->counter;
}

retCode cntDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  continuationPo cont = C_CONTINUATION(t);

  return outMsg(out, "(.continuation %d:[%d].)", cont->stack->hash, cont->pc);
}

termPo cntFinalizer(specialClassPo class, termPo o) {
  return o + ContinuationCellCount;
}

continuationPo allocateContinuation(heapPo H, stackPo stack, framePo fp, insPo pc) {
  int root = gcAddRoot(H, (ptrPo) &stack);

  continuationPo cont = (continuationPo) allocateObject(H, contClass, ContinuationCellCount);

  cont->stack = stack;
  cont->fp = fp;
  cont->pc = pc;
  cont->counter = ++stack->counter;

  assert(validFP(stack, fp));

  gcReleaseRoot(H, root);
  return cont;
}

stackPo contStack(continuationPo cont) {
  return cont->stack;
}

logical continIsValid(continuationPo cont) {
  stackPo stk = cont->stack;
  return validFP(stk, cont->fp);
}

framePo contFP(continuationPo cont) {
  return cont->fp;
}

insPo contPC(continuationPo cont){
  return cont->pc;
}
