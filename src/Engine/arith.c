//
// Created by Francis McCabe on 3/1/18.
//

#include "arithP.h"
#include "labelsP.h"

static retCode intDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static integer intHash(builtinClassPo cl, termPo o);
static logical intCmp(builtinClassPo cl, termPo t1, termPo t2);

BuiltinTerm IntegerClass = {
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null,
  .finalizer = Null,
  .compFun = intCmp,
  .hashFun = intHash,
  .dispFun = intDisp
};

builtinClassPo integerClass = &IntegerClass;
int32 integerIndex;


logical intCmp(builtinClassPo cl, termPo t1, termPo t2) {
  integer ix1 = integerVal(t1);
  integer ix2 = integerVal(t2);

  return (logical)(ix1 == ix2);
}

integer intHash(builtinClassPo cl, termPo o) {
  return hash61(integerVal(o));
}

static retCode intDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  return outInteger(out, integerVal(t), 10, 0, precision, 0, False, "", False);
}

static long floatSize(builtinClassPo cl, termPo o);
static termPo floatCopy(builtinClassPo cl, termPo dst, termPo src);
static termPo floatScan(builtinClassPo cl, specialHelperFun helper, void* c, termPo o);
static logical floatCmp(builtinClassPo cl, termPo o1, termPo o2);
static integer fltHash(builtinClassPo cl, termPo o);
static retCode floatDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo floatFinalizer(builtinClassPo class, termPo o);

BuiltinTerm FloatClass = {
  .sizeFun = floatSize,
  .copyFun = floatCopy,
  .scanFun = floatScan,
  .finalizer = floatFinalizer,
  .compFun = floatCmp,
  .hashFun = fltHash,
  .dispFun = floatDisp
};

builtinClassPo floatClass = &FloatClass;
int32 floatIndex;

long floatSize(builtinClassPo cl, termPo o) {
  return FloatCellCount;
}

termPo floatCopy(builtinClassPo cl, termPo dst, termPo src) {
  floatPo si = C_FLOAT(src);
  floatPo di = (floatPo)dst;
  *di = *si;

  return (termPo)di + FloatCellCount;
}

termPo floatScan(builtinClassPo cl, specialHelperFun helper, void* c, termPo o) {
  return o + FloatCellCount;
}

termPo floatFinalizer(builtinClassPo class, termPo o) {
  return o + FloatCellCount;
}

logical floatCmp(builtinClassPo cl, termPo o1, termPo o2) {
  return o1 == o2;
}

logical fltCmp(builtinClassPo cl, termPo t1, termPo t2) {
  double dx1 = floatVal(t1);
  double dx2 = floatVal(t2);

  if (dx1 == dx2)
    return True;
  else
    return False;
}

integer floatHash(floatPo f) {
  return hash61((integer)float_bits(f->dx));
}

integer fltHash(builtinClassPo cl, termPo o) {
  return hash61((integer)float_bits(floatVal((o))));
}

retCode floatDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  floatPo flt = C_FLOAT(t);

  return outDouble(out, flt->dx, 'g', 0, (int)precision, ' ', True, False);
}

floatPo C_FLOAT(termPo t) {
  assert(hasIndex(t, floatIndex));
  return (floatPo)t;
}

logical isFloat(termPo t) {
  return hasIndex(t, floatIndex);
}

termPo makeFloat(heapPo H, double dx) {
  floatPo flt = (floatPo)allocateObject(H, floatIndex, FloatCellCount);
  flt->dx = dx;
  return (termPo)flt;
}

double floatVal(termPo t) {
  return C_FLOAT(t)->dx;
}

void initArith() {
  IntegerClass.special.lblIndex = specialIndex;
  integerIndex = standardIndex(integerClass);
  FloatClass.special.lblIndex = specialIndex;
  floatIndex = standardIndex(floatClass);
}
