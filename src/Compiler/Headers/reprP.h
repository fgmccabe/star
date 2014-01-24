#ifndef _REPR_P_H_
#define _REPR_P_H_

#include "repr.h"

typedef struct _constructor_spec_ {
  long conIx;
  long fill;
  scav scavenger;
  evac evacuator;
  uniChar [] name;
} ConstructorSpecifier;

typedef struct _constructor_ {
  conSpecPo specifier;
} Constructor;

#endif
