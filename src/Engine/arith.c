//
// Created by Francis McCabe on 3/1/18.
//

#include "arithP.h"

static retCode intDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static integer intHash(specialClassPo cl, termPo o);
static logical intCmp(specialClassPo cl, termPo t1, termPo t2);

SpecialClass IntegerClass = {
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null,
  .finalizer = Null,
  .compFun = intCmp,
  .hashFun = intHash,
  .dispFun = intDisp
};

clssPo integerClass =  (clssPo) &IntegerClass;


logical intCmp(specialClassPo cl, termPo t1, termPo t2) {
  integer ix1 = integerVal(t1);
  integer ix2 = integerVal(t2);

  return (logical) (ix1 == ix2);
}

integer intHash(specialClassPo cl, termPo o) {
  return hash61(integerVal(o));
}

static retCode intDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  return outInteger(out, integerVal(t), 10, 0, precision, 0, False, "", alt);
}

static long floatSize(specialClassPo cl, termPo o);
static termPo floatCopy(specialClassPo cl, termPo dst, termPo src);
static termPo floatScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical floatCmp(specialClassPo cl, termPo o1, termPo o2);
static integer fltHash(specialClassPo cl, termPo o);
static retCode floatDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo floatFinalizer(specialClassPo class, termPo o);

SpecialClass FloatClass = {
  .sizeFun = floatSize,
  .copyFun = floatCopy,
  .scanFun = floatScan,
  .finalizer = floatFinalizer,
  .compFun = floatCmp,
  .hashFun = fltHash,
  .dispFun = floatDisp
};

clssPo floatClass = (clssPo) &FloatClass;

long floatSize(specialClassPo cl, termPo o) {
  return FloatCellCount;
}

termPo floatCopy(specialClassPo cl, termPo dst, termPo src) {
  floatPo si = C_FLOAT(src);
  floatPo di = (floatPo) dst;
  *di = *si;

  return (termPo) di + FloatCellCount;
}

termPo floatScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return o + FloatCellCount;
}

termPo floatFinalizer(specialClassPo class, termPo o) {
  return o + FloatCellCount;
}

logical floatCmp(specialClassPo cl, termPo o1, termPo o2) {
  return o1==o2;
}

logical fltCmp(specialClassPo cl, termPo t1, termPo t2) {
  double dx1 = floatVal(t1);
  double dx2 = floatVal(t2);

  if (dx1==dx2)
    return True;
  else
    return False;
}

integer floatHash(floatPo f) {
  return hash61((integer) float_bits(f->dx));
}

integer fltHash(specialClassPo cl, termPo o) {
  return hash61((integer) float_bits(floatVal((o))));
}

retCode floatDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  floatPo flt = C_FLOAT(t);

  return outDouble(out, flt->dx, 'g', 0, (int) precision, ' ', True, False);
}

floatPo C_FLOAT(termPo t) {
  assert(hasClass(t, floatClass));
  return (floatPo) t;
}

logical isFloat(termPo t) {
  return hasClass(t, floatClass);
}

termPo makeFloat(heapPo H, double dx) {
  floatPo flt = (floatPo) allocateObject(H, floatClass, FloatCellCount);
  flt->dx = dx;
  return (termPo)flt;
}

double floatVal(termPo t) {
  return C_FLOAT(t)->dx;
}

void initArith() {
  IntegerClass.clss.clss = specialClass;
  FloatClass.clss.clss = specialClass;
}
