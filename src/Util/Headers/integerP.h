//
// Created by Francis McCabe on 8/5/18.
//

#ifndef CAFE_INTEGERP_H
#define CAFE_INTEGERP_H

#include "integer.h"
#include "objectP.h"

typedef struct {
  integer ix;
} IxObjectRec;

typedef struct _ix_object_ {
  ObjectRec object;                     /* object level of the integer structure */
  IxObjectRec ix;
} IxObjRecord;

typedef struct {
} IxClassPart;

typedef struct _ix_class_ {
  ObjectClassRec objectPart;
  IxClassPart ixPart;
} IxClassRec;

extern IxClassRec IxClass;

#endif //CAFE_INTEGERP_H
