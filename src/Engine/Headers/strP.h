//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_STRP_H
#define CAFE_STRP_H

#include "str.h"

// String structure
typedef struct string_struct {
  Term termPart;          // == stringClass
  integer size;
  char data[ZEROARRAYSIZE];
} StringRec;



#endif //CAFE_STRP_H
