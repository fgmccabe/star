//
// Created by Francis McCabe on 8/25/18.
//

#ifndef STAR_STRNGP_H
#define STAR_STRNGP_H

#include "strng.h"
#include "objectP.h"

typedef struct {
  integer len;
  char *txt;
} StrgObjectRec;

typedef struct _strg_object_ {
  ObjectRec object;                     /* object level of the integer structure */
  StrgObjectRec s;
} StrgObjRecord;

typedef struct {
} StrgClassPart;

typedef struct _strg_class_ {
  ObjectClassRec objectPart;
  StrgClassPart strgPart;
} StrgClassRec;

extern StrgClassRec StrgClass;

#endif //STAR_STRNGP_H
