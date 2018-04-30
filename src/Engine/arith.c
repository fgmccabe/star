//
// Created by Francis McCabe on 3/1/18.
//

#include "arithP.h"
#include "assert.h"

static long intSize(specialClassPo cl, termPo o);
static termPo intCopy(specialClassPo cl, termPo dst, termPo src);
static termPo intScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static retCode intDisp(ioPo out, termPo t, long depth, logical alt);
static integer intHash(specialClassPo cl, termPo o);
static comparison intCmp(specialClassPo cl, termPo t1, termPo t2);

SpecialClass IntegerClass = {
  .clss = Null,
  .sizeFun = intSize,
  .copyFun = intCopy,
  .scanFun = intScan,
  .compFun = intCmp,
  .hashFun = intHash,
  .dispFun = intDisp
};

clssPo integerClass = (clssPo) &IntegerClass;

static long fltSize(specialClassPo cl, termPo o);
static termPo fltCopy(specialClassPo cl, termPo dst, termPo src);
static termPo fltScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static retCode fltDisp(ioPo out, termPo t, long depth, logical alt);
static integer fltHash(specialClassPo cl, termPo o);
static comparison fltCmp(specialClassPo cl, termPo t1, termPo t2);

SpecialClass FloatClass = {
  .clss = Null,
  .sizeFun = fltSize,
  .copyFun = fltCopy,
  .scanFun = fltScan,
  .compFun = fltCmp,
  .hashFun = fltHash,
  .dispFun = fltDisp
};

clssPo floatClass = (clssPo) &FloatClass;

void initArith() {
  IntegerClass.clss = specialClass;
  FloatClass.clss = specialClass;
}

intPo allocateInteger(heapPo H, integer ix) {
  intPo t = (intPo) allocateObject(H, integerClass, CellCount(sizeof(IntegerRecord)));
  t->ix = ix;
  return t;
}

long intSize(specialClassPo cl, termPo o) {
  return CellCount(sizeof(IntegerRecord));
}

termPo intCopy(specialClassPo cl, termPo dst, termPo src) {
  intPo si = C_INT(src);
  intPo di = (intPo) (dst);
  *di = *si;
  return (termPo) di + IntegerCellCount;
}

termPo intScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return (termPo) (o + IntegerCellCount);
}

comparison intCmp(specialClassPo cl, termPo t1, termPo t2) {
  integer ix1 = integerVal(t1);
  integer ix2 = integerVal(t2);

  if (ix1 == ix2)
    return same;
  else if (ix1 < ix2)
    return smaller;
  else
    return bigger;
}

integer intHash(specialClassPo cl, termPo o) {
  return integerHash(C_INT(o));
}

static retCode intDisp(ioPo out, termPo t, long depth, logical alt) {
  intPo ix = C_INT(t);
  return outInteger(out, ix->ix, 10, 0, 0, 0, False, "", alt);
}

extern intPo C_INT(termPo t) {
  assert(hasClass(t, integerClass));
  return (intPo) t;
}

const int64 integerVal(termPo o) {
  intPo ix = C_INT(o);
  return ix->ix;
}

integer integerHash(intPo ix) {
  return ix->ix;
}

fltPo allocateFloat(heapPo H, double dx) {
  fltPo t = (fltPo) allocateObject(H, integerClass, CellCount(sizeof(FloatRecord)));
  t->dx = dx;
  return t;
}

long fltSize(specialClassPo cl, termPo o) {
  return CellCount(sizeof(FloatRecord));
}

termPo fltCopy(specialClassPo cl, termPo dst, termPo src) {
  fltPo si = C_FLT(src);
  fltPo di = (fltPo) (dst);
  *di = *si;
  return (termPo) di + FloatCellCount;
}

termPo fltScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return (termPo) (o + FloatCellCount);
}

static retCode fltDisp(ioPo out, termPo t, long depth, logical alt) {
  fltPo dx = C_FLT(t);
  return outFloat(out, dx->dx);
}

comparison fltCmp(specialClassPo cl, termPo t1, termPo t2) {
  double ix1 = floatVal(t1);
  double ix2 = floatVal(t2);

  if (ix1 == ix2)
    return same;
  else if (ix1 < ix2)
    return smaller;
  else
    return bigger;
}

integer fltHash(specialClassPo cl, termPo o) {
  return floatHash(floatVal(o));
}

integer floatHash(double dx){
  union {
    double n;
    integer i;
  } c;
  c.n = dx;
  return c.i;
}

extern fltPo C_FLT(termPo t) {
  assert(hasClass(t, floatClass));
  return (fltPo) t;
}

const double floatVal(termPo o) {
  assert(isInteger(o));
  intPo ix = (intPo) o;
  return ix->ix;
}
