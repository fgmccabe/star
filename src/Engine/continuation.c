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
    helper((ptrPo) &cont->stack, c);
  }
  return o + ContinuationCellCount;
}

logical cntCmp(specialClassPo cl, termPo o1, termPo o2) {
  return o1 == o2;
}

integer cntHash(specialClassPo cl, termPo o) {
  continuationPo cont = C_CONTINUATION(o);

  return hash61((cont->stack->hash) * 37 + cont->hashCounter);
}

retCode cntDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  continuationPo cont = C_CONTINUATION(t);

  if (continIsValid(cont)) {
    framePo fp = contFP(cont);
    termPo loc = findPcLocation(fp->prog, insOffset(fp->prog, contPC(cont)));

    if (loc == Null)
      return outMsg(out, "(.continuation %d:[unknown].)", cont->stack->hash, displayDepth);
    else
      return outMsg(out, "(.continuation %d:[%,*T].)", cont->stack->hash, displayDepth, loc);
  } else
    return outMsg(out, "(.continuation %d:invalid.)",cont->stack->hash);
}

termPo cntFinalizer(specialClassPo class, termPo o) {
  return o + ContinuationCellCount;
}

continuationPo allocateContinuation(heapPo H, stackPo stack, ptrPo sp, framePo fp, insPo pc) {
  int root = gcAddRoot(H, (ptrPo) &stack);

  integer fpo = fp - baseFrame(stack);
  integer spo = stackLimit(stack) - sp;
  integer pcOff = insOffset(fp->prog, pc);

  continuationPo cont = (continuationPo) allocateObject(H, contClass, ContinuationCellCount);

  cont->stack = stack;
  cont->fpNo = fpo;
  cont->pcOff = pcOff;
  cont->spOff = spo;
  cont->hashCounter = ++stack->counter;

  gcReleaseRoot(H, root);
  return cont;
}

stackPo contStack(continuationPo cont) {
  return cont->stack;
}

logical continIsValid(continuationPo cont) {
  stackPo stk = cont->stack;
  return stk != Null && stackState(stk) != moribund && validFP(stk, contFP(cont));
}

void invalidateCont(continuationPo cont) {
  assert(continIsValid(cont));
  cont->stack = Null;
}

framePo contFP(continuationPo cont) {
  return baseFrame(cont->stack) + cont->fpNo;
}

insPo contPC(continuationPo cont) {
  return pcAddr(contFP(cont)->prog, cont->pcOff);
}

ptrPo contSP(continuationPo cont) {
  return stackLimit(cont->stack) - cont->spOff;
}
