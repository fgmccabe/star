//
// Created by Francis McCabe on 1/23/18.
//

#ifndef STAR_CHARSP_H
#define STAR_CHARSP_H

#include "heap.h"
#include "code.h"
#include "termP.h"
#include "strings.h"

typedef struct chars_term_ {
  clssPo clss;                  // == stringClass
  integer length;
  integer hash;
  char txt[ZEROARRAYSIZE];
} CharsRecord;

extern void initStrings();

#define CharsCellCount(len) CellCount(sizeof(CharsRecord)+(len)*sizeof(char))

#endif //STAR_CHARSP_H
