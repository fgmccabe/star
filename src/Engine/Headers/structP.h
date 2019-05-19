//
// Created by Francis McCabe on 2019-05-17.
//

#ifndef STAR_STRUCTP_H
#define STAR_STRUCTP_H

#include "struct.h"
#include "termP.h"
#include "integer.h"

typedef struct struct_class *structClassPo;

typedef struct struct_field {
  integer size;
  clssPo clss;
} StructField;

typedef struct struct_record {
  structClassPo structClass;
  integer numFields;
  StructField fields[ZEROARRAYSIZE];
} StructRecord;

typedef struct struct_class {
  clssPo clss; // == structClass
} StructClass;

#endif //STAR_STRUCTP_H
