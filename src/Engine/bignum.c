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

integer bigLen(bignumPo b) {
  return b->count;
}

byte *bigData(bignumPo b) {
  return b->data;
}

termPo allocateBignum(heapPo H, integer count, byte data[]) {
  bignumPo big = (bignumPo) allocateObject(H, bignumClass, BignumCellCount(count));

  big->clss = bignumClass;
  big->hash = 0;
  big->count = count;

  byteMove(big->data, count, data, count);

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
  byteMove(di->data, bCount, si->data, bCount);

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
  return sameBytes(b1->data, b1->count, b2->data, b2->count);
}

static integer bigHash(specialClassPo cl, termPo o) {
  bignumPo b = C_BIGNUM(o);
  if (b->hash == 0) {
    b->hash = byteHash(b->data, b->count);
  }
  return b->hash;
}

static retCode bigDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  bignumPo big = C_BIGNUM(t);
  byte *data = big->data;

  integer buffLen = big->count + 5;
  char buff[buffLen];
  integer txtLen = textOfBignum(buff, buffLen, big->data, big->length);

  return outMsg(out, "%*Sd", txtLen, buff);
}

termPo cbdFromString(heapPo h, char *text, integer tlen) {
  byte data[tlen * 2];
  integer dLen = bigFromText(text, tlen, data, tlen * 2);

  return allocateBignum(h, dLen, data);
}

