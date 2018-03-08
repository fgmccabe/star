//
// Created by Francis McCabe on 1/23/18.
//

#ifndef CAFE_STRP_H
#define CAFE_STRP_H

#include "heap.h"
#include "code.h"
#include "termP.h"
#include "str.h"

typedef struct string_term {
  clssPo clss;                  // == stringClass
  integer length;
  integer hash;
  char txt[ZEROARRAYSIZE];
} StringRecord;

extern void initStr();


#define StringCellCount(len) CellCount(sizeof(StringRecord)+(len)*sizeof(char))

#endif //CAFE_STRP_H
