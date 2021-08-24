//
// Created by Francis McCabe on 8/23/21.
//

#include <assert.h>
#include "buddyTest.h"
#include "buddyP.h"

static integer maxStackSize = 128;
static integer minStackSize = 16;

retCode test_buddy() {
  traceBuddyMemory = True;

  integer regionSize = (1 << lg2(maxStackSize));
  buddyRegionPo region = createRegion(regionSize, minStackSize, 2);

  assert(available(region, 127));

  voidPtr blk1 = allocateBuddy(region, 15);
  voidPtr blk2 = allocateBuddy(region, 31);
  voidPtr blk3 = allocateBuddy(region, 15);
  voidPtr blk4 = allocateBuddy(region, 31);

  assert(!available(region, 63));

  release(region, blk2);
  release(region, blk4);
  release(region, blk1);
  release(region, blk3);

  assert(available(region, 127));

  return Ok;
}
