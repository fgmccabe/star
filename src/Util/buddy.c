//
// Created by Francis McCabe on 8/22/21.
//

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "buddyP.h"
#include "ooio.h"

logical traceBuddyMemory = False;

buddyRegionPo createRegion(integer size, integer minSize) {
  integer lg = lg2(size);
  assert(size == (1 << lg));
  integer lg2Min = lg2(minSize);
  assert(minSize == (1 << lg2Min));
  integer lgDiff = lg - lg2Min;

  buddyRegionPo region = malloc(sizeof(BuddyMemoryRegion) + (lgDiff + 1) * sizeof(freePo));

  size_t regionSize = size * sizeof(void *);
  region->memBase = malloc(regionSize);

#ifdef TRACE_BUDDY_MEMORY
  memset(region->memBase, 0x5a, regionSize);
#endif

  region->size = size;
  region->minLg = lg2Min;
  region->maxAlloc = size >> 1;
  region->freeListSize = lgDiff + 1;

  for (integer ix = 0; ix < lgDiff; ix++) {
    region->freeLists[ix] = Null;
  }

  FreeEntry last = {.buddy = {.lgSize = lg}, .next=Null};
  *(freePo) region->memBase = last;
  region->freeLists[lgDiff] = (freePo) region->memBase;

#ifdef TRACE_BUDDY_MEMORY
  if (traceBuddyMemory)
    logMsg(logFile, "buddy region of %d words established\n", size);
#endif
  return region;
}

static integer blockOffset(buddyRegionPo region, freePo block) {
  return (voidPtr *) block - (voidPtr *) region->memBase;
}

static logical validBlockList(freePo block) {
  if (block != Null) {
    integer blockLg = block->buddy.lgSize;

    while (block != Null) {
      if (block->buddy.lgSize != blockLg)
        return False;
      if (block->next != Null && block->next < block)
        return False;
      block = block->next;
    }
  }
  return True;
}

static retCode showBlockList(buddyRegionPo region, freePo block) {
  while (block != Null) {
    outMsg(logFile, "block 0x%x (%d)@0x%x\n%_", block, 1 << block->buddy.lgSize, blockOffset(region, block));
    block = block->next;
  }
  return Ok;
}

static void showRegion(buddyRegionPo region) {
  outMsg(logFile, "region base=0x%x, size=0x%x\n", region->memBase, region->size);
  for (integer ix = 0; ix < region->freeListSize; ix++) {
    freePo list = region->freeLists[ix];
    if (list != Null) {
      outMsg(logFile, "free list @ allocation %d\n", (1 << (ix + region->minLg)));
      showBlockList(region, list);
    }
  }
}

static freePo insertBlock(freePo list, freePo block) {
  if (list == Null) {
    block->next = Null;
    return block;
  } else if (list < block) {
    list->next = insertBlock(list->next, block);
    return list;
  } else {
    block->next = list;
    return block;
  }
}

static void coalesceBlocks(buddyRegionPo region, integer freeIx) {
  freePo *list = &region->freeLists[freeIx];

  integer blockSize = (1 << (freeIx + region->minLg));

  while (*list != Null) {
    assert((*list)->buddy.lgSize == freeIx + region->minLg);
    freePo lst = *list;

    if (lst->next != Null && (blockOffset(region,lst)^blockSize)==blockOffset(region,lst->next)){
      freePo entry = lst;
      *list = entry->next->next; // Chop the newly coalesced block out

      entry->buddy.lgSize++;
      entry->next = Null;

      integer nextIx = freeIx + 1;
      region->freeLists[nextIx] = insertBlock(region->freeLists[nextIx], entry);
      coalesceBlocks(region, nextIx);
    } else
      list = &(*list)->next;
  }
}

// Size in terms of pointer cells
voidPtr *allocateBuddy(buddyRegionPo region, integer size) {
  // include space for the block header
  integer roundDiff = lg2(size * 2) - region->minLg;

  if(roundDiff<0){
    logMsg(logFile,"unable to allocate stack region of %ld words",size);
    syserr("not a valid stack size");
  }

  for (integer ix = roundDiff; ix < region->freeListSize; ix++) {
    if (region->freeLists[ix] != Null) {
      freePo block = region->freeLists[ix];
      region->freeLists[ix] = block->next;
      integer foundLg = ix;

      if (foundLg > roundDiff) { // Do we need to split our block
        integer prevIx = foundLg - 1;
        integer prevLg = prevIx + region->minLg;
        assert(region->freeLists[prevIx] == Null);
        // We split the block we found, put them on the previous freelist
        freePo block2 = (freePo) (((voidPtr *) block) + (1 << prevLg));
        block2->buddy.lgSize = prevLg;
        block->buddy.lgSize = prevLg;

        region->freeLists[prevIx] = insertBlock(insertBlock(region->freeLists[prevIx], block2), block);
        return allocateBuddy(region, size);
      } else {
        assert(foundLg == roundDiff);

#ifdef TRACE_BUDDY_MEMORY
        if (traceBuddyMemory) {
          logMsg(logFile, "allocate %d block at 0x%x", size, block);
//          showRegion(region);
        }
#endif

        return (voidPtr) ((buddyPo) block + 1);
      }
    }
  }

  return Null;
}

retCode releaseBlock(buddyRegionPo region, voidPtr *block) {
  freePo entry = (freePo) (((buddyPo) block) - 1);
  integer freeIx = entry->buddy.lgSize - region->minLg;

#ifdef TRACE_BUDDY_MEMORY
  if (traceBuddyMemory) {
    logMsg(logFile, "release block @ 0x%x", entry);
  }
#endif

  region->freeLists[freeIx] = insertBlock(region->freeLists[freeIx], entry);

#ifdef TRACE_BUDDY_MEMORY
  assert(validBlockList(region->freeLists[freeIx]));
  memset(entry + 1, 0x5a, (1 << entry->buddy.lgSize) - sizeof(FreeEntry));
#endif

  coalesceBlocks(region, freeIx);

  return Ok;
}

// Is a given size available?
logical available(buddyRegionPo region, integer size) {
  // include space for the block header
  integer roundDiff = lg2(size * 2) - region->minLg;
  assert(roundDiff >= 0);

  for (integer ix = roundDiff; ix < region->freeListSize; ix++) {
    if (region->freeLists[ix] != Null)
      return True;
  }
  return False;
}

logical inFreeBlock(buddyRegionPo region, voidPtr from) {
  for(integer fx=0;fx<region->freeListSize;fx++){
    integer buddySize = (1<<(fx+region->minLg));
    freePo free = region->freeLists[fx];
    while(free!=Null){
      if((voidPtr)free<=from && ((voidPtr)free)+buddySize>from)
        return True;
      free = free->next;
    }
  }
  return False;
}
