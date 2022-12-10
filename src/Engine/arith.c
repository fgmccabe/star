//
// Created by Francis McCabe on 3/1/18.
//

#include <math.h>
#include "arithP.h"
#include "assert.h"
#include "bignumP.h"
#include "heapP.h"

#ifdef TRACEMEM
integer allocatedInts = 0;
integer allocatedFloats = 0;
#endif

static retCode intDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static integer intHash(specialClassPo cl, termPo o);
static logical intCmp(specialClassPo cl, termPo t1, termPo t2);

SpecialClass IntegerClass = {
  .clss = Null,
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null,
  .finalizer = Null,
  .compFun = intCmp,
  .hashFun = intHash,
  .dispFun = intDisp
};

clssPo integerClass = (clssPo) &IntegerClass;

static long fltSize(specialClassPo cl, termPo o);
static termPo fltCopy(specialClassPo cl, termPo dst, termPo src);
static termPo fltScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static termPo fltFinalizer(specialClassPo class, termPo o);
static retCode fltDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static integer fltHash(specialClassPo cl, termPo o);
static logical fltCmp(specialClassPo cl, termPo t1, termPo t2);

SpecialClass FloatClass = {
  .clss = Null,
  .sizeFun = fltSize,
  .copyFun = fltCopy,
  .scanFun = fltScan,
  .finalizer = fltFinalizer,
  .compFun = fltCmp,
  .hashFun = fltHash,
  .dispFun = fltDisp
};

clssPo floatClass = (clssPo) &FloatClass;

void initArith() {
  IntegerClass.clss = specialClass;
  FloatClass.clss = specialClass;
  initBignum();
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

typedef struct float_term *fltPo;

extern fltPo C_FLT(termPo t) {
  assert(hasClass(t, floatClass));
  return (fltPo) t;
}

termPo allocateFloat(heapPo H, double dx) {
  fltPo t = (fltPo) allocateObject(H, floatClass, CellCount(sizeof(FloatRecord)));
  t->dx = dx;
#ifdef TRACEMEM
  if (traceAllocs)
    allocatedFloats++;
#endif
  return (termPo) t;
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

termPo fltFinalizer(specialClassPo class, termPo o) {
  return (termPo) (o + FloatCellCount);
}

static retCode fltDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  fltPo dx = C_FLT(t);
  return outDouble(out, dx->dx, 'g', 0, (int) precision, ' ', True, False);
}

logical fltCmp(specialClassPo cl, termPo t1, termPo t2) {
  double ix1 = floatVal(t1);
  double ix2 = floatVal(t2);

  if (nearlyEqual(ix1, ix2, EPSILON))
    return True;
  else
    return False;
}

logical nearlyEqual(double dx1, double dx2, double eps) {
  if (dx1 == dx2)
    return True;
  else {
    double abs1 = fabs(dx1);
    double abs2 = fabs(dx2);
    double diff = fabs(abs1 - abs2);
    if (dx1 == 0 || dx2 == 0 || diff < MIN_NORMAL) {
      if (diff < (eps * MIN_NORMAL))
        return True;
    } else if (diff / (abs1 + abs2) < eps)
      return True;
  }
  return False;
}

integer fltHash(specialClassPo cl, termPo o) {
  return floatHash(floatVal(o));
}

integer floatHash(double dx) {
  return hash61((integer) dx);
}

double floatVal(termPo o) {
  assert(isFloat(o));
  fltPo dx = (fltPo) o;
  return dx->dx;
}

