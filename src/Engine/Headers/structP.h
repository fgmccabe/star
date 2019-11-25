//
// Created by Francis McCabe on 11/21/19.
//

#ifndef STAR_STRUCTP_H
#define STAR_STRUCTP_H

#include "struct.h"

typedef struct struct_term {
  structSpecPo spec;                // Overlays clss - because it is the structure's class
  void *data[ZEROARRAYSIZE];
} Struct;

typedef struct struct_spec_el{
  int32 size;

}SpecEl;

#endif //STAR_STRUCTP_H
