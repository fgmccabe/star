//
// Created by Francis McCabe on 3/1/18.
//

#include <math.h>
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

static retCode fltDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static logical fltCmp(specialClassPo cl, termPo t1, termPo t2);
static integer fltHash(specialClassPo cl, termPo o);

SpecialClass FloatClass = {
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null,
  .finalizer = Null,
  .compFun = fltCmp,
  .hashFun = fltHash,
  .dispFun = fltDisp
};

clssPo floatClass = (clssPo)&FloatClass;

void initArith() {
  IntegerClass.clss.clss = specialClass;
  FloatClass.clss.clss = specialClass;
}

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

static retCode fltDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  return outDouble(out, floatVal(t), 'g', 0, (int) precision, ' ', True, False);
}

logical fltCmp(specialClassPo cl, termPo t1, termPo t2) {
  double dx1 = floatVal(t1);
  double dx2 = floatVal(t2);

  if (dx1==dx2)
    return True;
  else
    return False;
}

integer fltHash(specialClassPo cl, termPo o) {
  return floatHash(floatVal(o));
}