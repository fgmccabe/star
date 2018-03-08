//
// Created by Francis McCabe on 3/4/18.
//

#ifndef CAFE_ARRAYP_H
#define CAFE_ARRAYP_H

#include "array.h"
#include "code.h"
#include "termP.h"

typedef struct _list_base_ *basePo;

typedef struct _list_slice_ {
  clssPo clss;                  // == sliceClass
  termPo base;                  // Base of slice
  integer start;                // Where in the base do we start
  integer length;               // How many elements are we
} SliceRecord;

#define ListCellCount CellCount(sizeof(SliceRecord))

typedef struct _list_base_ {
  clssPo clss;                  // == baseClass
  integer length;               // Total number of elements in list
  integer min;                  // Smallest used offset
  integer max;                  // Max used offset
  termPo els[ZEROARRAYSIZE];
} BaseRecord;

extern clssPo baseClass;

extern basePo C_BASE(termPo o);

#define BaseCellCount(len) CellCount(sizeof(BaseRecord)+(len)*sizeof(termPo))

extern termPo allocateList(heapPo H, integer length, logical safeMode);

extern termPo sliceList(heapPo H, listPo list, integer from, integer count);

extern termPo appendToList(heapPo H, listPo list, termPo el);

extern termPo prependToList(heapPo H, listPo list, termPo el);

extern void initLists();

#endif //CAFE_ARRAYP_H
