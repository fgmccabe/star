//
// Created by Francis McCabe on 8/23/21.
//

#include <assert.h>
#include "buddyTest.h"
#include "buddyP.h"

static integer minStackSize = 16;

retCode test_buddy() {
//  traceBuddyMemory = True;

  buddyRegionPo region = createRegion(128, minStackSize);

  assert(available(region, 127));

  voidPtr blk1 = allocateBuddy(region, 15);
  voidPtr blk2 = allocateBuddy(region, 31);
  voidPtr blk3 = allocateBuddy(region, 15);
  voidPtr blk4 = allocateBuddy(region, 31);

  assert(allocateBuddy(region,63) == Null);

  assert(available(region, 31));
  assert(!available(region, 63));

  releaseBlock(region, blk2);
  releaseBlock(region, blk4);
  releaseBlock(region, blk1);
  releaseBlock(region, blk3);

  assert(available(region, 127));

  return Ok;
}

retCode test_many_blocks() {
//  traceBuddyMemory = True;
  integer regionSize = 1<<20;
  integer testBlockCount = 1<<16;

  buddyRegionPo region = createRegion(regionSize, minStackSize);

  assert(available(region, regionSize-1));

  voidPtr blocks[testBlockCount];

  for(integer ix=0;ix<testBlockCount;ix++){
    voidPtr blk = allocateBuddy(region,15);
    assert(blk!=Null);
    blocks[ix] = blk;
  }

  assert(!available(region, 15));

  for(integer ix=0;ix<testBlockCount;ix++)
    releaseBlock(region, blocks[ix]);

  assert(available(region, regionSize-1));

  return Ok;
}
