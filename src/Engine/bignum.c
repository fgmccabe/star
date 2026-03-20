//
// Created by Francis McCabe on 11/10/21.
//
// Implementation of Bignum arithmetic functions

#include "bignumP.h"
#include "assert.h"
#include "labelsP.h"

static long bigSize(builtinClassPo cl, termPo o);
static termPo bigCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo bigScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical bigCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer bigHash(builtinClassPo cl, termPo o);
static retCode bigDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo bigFinalizer(builtinClassPo class, termPo o);

BuiltinTerm BignumClass = {
  .sizeFun = bigSize,
  .copyFun = bigCopy,
  .scanFun = bigScan,
  .finalizer = bigFinalizer,
  .compFun = bigCmp,
  .hashFun = bigHash,
  .dispFun = bigDisp
};

int32 bignumIndex;

void initBignum() {
  BignumClass.special.lblIndex = specialIndex;
  bignumIndex = standardIndex(&BignumClass);
}

logical isBignum(termPo t) {
  return (logical) hasIndex(t, bignumIndex);
}

bignumPo C_BIGNUM(termPo t) {
  assert(isBignum(t));
  return (bignumPo) t;
}

uint32 bigCount(bignumPo b) {
  return b->count;
}

uint32 *bigDigits(bignumPo b) {
  return b->data;
}

termPo allocateBignum(heapPo H, uint32 count, uint32 data[]) {
  bignumPo big = (bignumPo) allocateObject(H, bignumIndex, BignumCellCount(count));
  big->count = count;

  wordMove(big->data, count, data, count);

  return (termPo) big;
}

long bigSize(builtinClassPo cl, termPo o) {
  return BignumCellCount(C_BIGNUM(o)->count);
}

termPo bigCopy(builtinClassPo cl, termPo dst, termPo src) {
  bignumPo si = C_BIGNUM(src);
  bignumPo di = (bignumPo) dst;
  *di = *si;

  uint32 bCount = si->count;
  wordMove(di->data, bCount, si->data, bCount);

  return ((termPo) di) + BignumCellCount(si->count);
}

termPo bigScan(builtinClassPo cl, specialHelperFun helper, void *c, termPo o) {
  bignumPo big = C_BIGNUM(o);

  return o + BignumCellCount(big->count);
}

termPo bigFinalizer(builtinClassPo class, termPo o) {
  bignumPo big = C_BIGNUM(o);

  return o + BignumCellCount(big->count);
}

logical bigCmp(builtinClassPo cl, termPo o1, termPo o2) {
  bignumPo b1 = C_BIGNUM(o1);
  bignumPo b2 = C_BIGNUM(o2);
  return sameWords(b1->data, b1->count, b2->data, b2->count);
}

static integer bigHash(builtinClassPo cl, termPo o) {
  bignumPo b = C_BIGNUM(o);
  return (integer)longHash(b->data, b->count);
}

static retCode bigDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  bignumPo big = C_BIGNUM(t);

  return showLong(out, big->data, big->count);
}

termPo bignumFromString(heapPo h, char *text, integer tLen) {
  integer dS = tLen * 2 + 1;
  uint32 data[dS];
  integer dLen = longFromText(text, tLen, data, dS);

  return allocateBignum(h, dLen, data);
}

integer bignumHash(bignumPo bg) {
  return wordHash(bg->data,bg->count);
}
