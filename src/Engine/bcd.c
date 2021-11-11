//
// Created by Francis McCabe on 11/10/21.
//
// Implementation of BCD arithmetic functions

#include "bcdP.h"
#include "assert.h"

static long bcdSize(specialClassPo cl, termPo o);
static termPo bcdCopy(specialClassPo cl, termPo dst, termPo src);
static termPo bcdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical bcdCmp(specialClassPo cl, termPo o1, termPo o2);
static integer bcdHash(specialClassPo cl, termPo o);
static retCode bcdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo bcdFinalizer(specialClassPo class, termPo o);

SpecialClass BCDClass = {
  .clss = Null,
  .sizeFun = bcdSize,
  .copyFun = bcdCopy,
  .scanFun = bcdScan,
  .finalizer = bcdFinalizer,
  .compFun = bcdCmp,
  .hashFun = bcdHash,
  .dispFun = bcdDisp
};

clssPo bcdClass = (clssPo) &bcdClass;

void initBCD() {
  BCDClass.clss = specialClass;
}

logical isBcd(termPo t) {
  return (logical) hasClass(t, bcdClass);
}

bcdPo C_BCD(termPo t) {
  assert(hasClass(t, bcdClass));
  return (bcdPo) t;
}

integer bcdLen(bcdPo b) {
  return b->length;
}

byte *bcdData(bcdPo b) {
  return b->data;
}

static byte *bcdVal(termPo t, integer *length) {
  bcdPo bcd = C_BCD(t);
  *length = bcd->length;
  return bcd->data;
}

termPo allocateBCD(heapPo H, integer count, byte data[]) {
  bcdPo bcd = (bcdPo) allocateObject(H, bcdClass, BCDCellCount(count));

  bcd->clss = bcdClass;
  bcd->hash = 0;
  bcd->length = count;

  byteMove(bcd->data, count, data, count);

  return (termPo) bcd;
}

long bcdSize(specialClassPo cl, termPo o) {
  return BCDCellCount(C_BCD(o)->length);
}

termPo bcdCopy(specialClassPo cl, termPo dst, termPo src) {
  bcdPo si = C_BCD(src);
  bcdPo di = (bcdPo) dst;
  *di = *si;

  byteMove(di->data, si->length, si->data, si->length);

  return ((termPo) di) + BCDCellCount(si->length);
}

termPo bcdScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  bcdPo bcd = C_BCD(o);

  return o + BCDCellCount(bcd->length);
}

termPo bcdFinalizer(specialClassPo class, termPo o) {
  bcdPo bcd = C_BCD(o);

  return o + BCDCellCount(bcd->length);
}

logical bcdCmp(specialClassPo cl, termPo o1, termPo o2) {
  integer l1, l2;
  const byte *tx1 = bcdVal(o1, &l1);
  const byte *tx2 = bcdVal(o2, &l2);

  return sameBytes(tx1, l1, tx2, l2);
}

static integer bcdHash(specialClassPo cl, termPo o) {
  bcdPo b = C_BCD(o);
  if (b->hash == 0) {
    b->hash = byteHash(b->data, b->length);
  }
  return b->hash;
}

static retCode bcdDisp(ioPo out, termPo t, integer precision, integer depth, logical alt){
  return Error;
}
