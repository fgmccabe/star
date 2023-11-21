//
// Created by Francis McCabe on 8/22/21.
//

#ifndef STAR_BUDDY_H
#define STAR_BUDDY_H

#include "integer.h"
#include "retcode.h"

typedef struct buddy_memory_ *buddyRegionPo;

buddyRegionPo createRegion(integer size, integer minSize);

typedef void* voidPtr;
voidPtr *allocateBuddy(buddyRegionPo region, integer size);
retCode releaseBlock(buddyRegionPo region, voidPtr *block);

logical available(buddyRegionPo region, integer size);
logical inFreeBlock(buddyRegionPo region, voidPtr from);

#ifdef ALLTRACE
#define TRACE_BUDDY_MEMORY
#endif

#endif //STAR_BUDDY_H
