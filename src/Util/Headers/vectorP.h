//
// Created by Francis McCabe on 7/31/18.
//

#ifndef CAFE_VECTORP_H
#define CAFE_VECTORP_H

#include "vector.h"
#include "objectP.h"

typedef struct {
  objectPo *data;
  integer count;
  integer size;
} VectorObjectRec;

typedef struct _vector_record_ {
  ObjectRec object;                     /* object level of the vector structure */
  VectorObjectRec vect;
} VectorObjRecord;

typedef struct {
} VectorClassPart;

typedef struct _vector_class_ {
  ObjectClassRec objectPart;
  VectorClassPart pairPart;
} VectorClassRec;

extern VectorClassRec VectorClass;

#endif //CAFE_VECTORP_H
