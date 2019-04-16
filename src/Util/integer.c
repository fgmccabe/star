//
// Created by Francis McCabe on 8/5/18.
//
#include "integerP.h"

static integer ixHash(objectPo o);
static logical ixEquality(objectPo o1, objectPo o2);
static void ixInit(objectPo o, va_list *args);

IxClassRec IxClass = {
  {
    (classPo) &ObjectClass,
    "integer",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    ixInit,
    sizeof(IxObjRecord),
    ixHash,
    ixEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo ixClass = (classPo) &IxClass;

void ixInit(objectPo o, va_list *args) {
  ixPo i = O_IX(o);
  integer ix = va_arg(*args,integer);

  i->ix.ix = ix;
}

static integer ixHash(objectPo o) {
  ixPo ix = O_IX(o);
  return ix->ix.ix;
}

static logical ixEquality(objectPo o1, objectPo o2) {
  ixPo v1 = O_IX(o1);
  ixPo v2 = O_IX(o2);

  return (logical)(v1->ix.ix==v2->ix.ix);
}

ixPo newInteger(integer ix)
{
  return (ixPo)newObject(ixClass,ix);
}

integer ixVal(ixPo i){
  return i->ix.ix;
}
