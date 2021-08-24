//
// Created by Francis McCabe on 8/22/21.
//

#ifndef STAR_BUDDYP_H
#define STAR_BUDDYP_H

#include "buddy.h"
#include "config.h"

typedef struct free_entry_ *freePo;

typedef struct buddy_entry_ {
  integer lgSize;
} BuddyEntry, *buddyPo;

typedef struct free_entry_ {
  BuddyEntry buddy;
  freePo next;
} FreeEntry;

typedef struct buddy_memory_ {
  void *memBase;
  integer size;
  integer minLg;
  integer maxAlloc;
  integer freeListSize;
  freePo freeLists[ZEROARRAYSIZE];
} BuddyMemoryRegion;

extern logical traceBuddyMemory;

#endif //STAR_BUDDYP_H
