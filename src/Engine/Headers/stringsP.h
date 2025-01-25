//
// Created by Francis McCabe on 1/23/18.
//

#ifndef STAR_STRINGSP_H
#define STAR_STRINGSP_H

#include "heap.h"
#include "code.h"
#include "termP.h"
#include "strings.h"

typedef struct string_term_ {
  ClassRecord clss;               // == stringClass
  integer length;
  integer hash;
  char txt[ZEROARRAYSIZE];
} StringRecord;

extern void initStrings();

#define StringCellCount(len) CellCount(sizeof(StringRecord)+(len)*sizeof(char))

#endif //STAR_STRINGSP_H
