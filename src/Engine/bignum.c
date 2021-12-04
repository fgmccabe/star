//
// Created by Francis McCabe on 11/10/21.
//
// Implementation of Bignum arithmetic functions

#include "bignumP.h"
#include "assert.h"

static long bigSize(specialClassPo cl, termPo o);
static termPo bigCopy(specialClassPo cl, termPo dst, termPo src);
static termPo bigScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical bigCmp(specialClassPo cl, termPo o1, termPo o2);
static integer bigHash(specialClassPo cl, termPo o);
static retCode bigDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo bigFinalizer(specialClassPo class, termPo o);

SpecialClass BignumClass = {
  .clss = Null,
  .sizeFun = bigSize,
  .copyFun = bigCopy,
  .scanFun = bigScan,
  .finalizer = bigFinalizer,
  .compFun = bigCmp,
  .hashFun = bigHash,
  .dispFun = bigDisp
};

clssPo bignumClass = (clssPo) &bignumClass;

void initBignum() {
  BignumClass.clss = specialClass;
}

logical isBignum(termPo t) {
  return (logical) hasClass(t, bignumClass);
}

bignumPo C_BIGNUM(termPo t) {
  assert(hasClass(t, bignumClass));
  return (bignumPo) t;
}

integer bigCount(bignumPo b) {
  return b->count;
}

uint32 *bigDigits(bignumPo b) {
  return b->data;
}

termPo allocateBignum(heapPo H, integer count, uint32 data[]) {
  bignumPo big = (bignumPo) allocateObject(H, bignumClass, BignumCellCount(count));

  big->clss = bignumClass;
  big->hash = 0;
  big->count = count;

  wordMove(big->data, count, data, count);

  return (termPo) big;
}

long bigSize(specialClassPo cl, termPo o) {
  return BignumCellCount(C_BIGNUM(o)->count);
}

termPo bigCopy(specialClassPo cl, termPo dst, termPo src) {
  bignumPo si = C_BIGNUM(src);
  bignumPo di = (bignumPo) dst;
  *di = *si;

  integer bCount = si->count;
  wordMove(di->data, bCount, si->data, bCount);

  return ((termPo) di) + BignumCellCount(si->count);
}

termPo bigScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  bignumPo big = C_BIGNUM(o);

  return o + BignumCellCount(big->count);
}

termPo bigFinalizer(specialClassPo class, termPo o) {
  bignumPo big = C_BIGNUM(o);

  return o + BignumCellCount(big->count);
}

logical bigCmp(specialClassPo cl, termPo o1, termPo o2) {
  bignumPo b1 = C_BIGNUM(o1);
  bignumPo b2 = C_BIGNUM(o2);
  return sameWords(b1->data, b1->count, b2->data, b2->count);
}

static integer bigHash(specialClassPo cl, termPo o) {
  bignumPo b = C_BIGNUM(o);
  if (b->hash == 0) {
    b->hash = wordHash(b->data, b->count);
  }
  return b->hash;
}

static retCode bigDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  bignumPo big = C_BIGNUM(t);
  uint32 *digits = big->data;

  return showLong(out, big->data, big->count);
}

termPo bignumFromString(heapPo h, char *text, integer tLen) {
  integer dS = tLen * 2 + 1;
  uint32 data[dS];
  integer dLen = longFromText(text, tLen, data, dS);

  return allocateBignum(h, dLen, data);
}

integer bignumHash(bignumPo bg){
  if (bg->hash == 0) {
    bg->hash = wordHash(bg->data, bg->count);
  }
  return bg->hash;
}
