//
// Created by Francis McCabe on 12/04/25.
//

#include <formioP.h>
#include <assert.h>
#include "setTests.h"
#include "setP.h"

static setPo testSet = Null;

static void setupTests() {
  testSet = createSet(0);
}

static void tearDownTests() {
  deleteSet(testSet);
}

static retCode createAndDestroy() {
  setPo set = createSet(0);
  assert(set!=Null);
  deleteSet(set);
  return Ok;
}

static retCode addABunch() {
  setPo set = createSet(0);
  assert(set!=Null);

  for (int32 ix = 0; ix < 1024; ix++)
    addToSet(set, ix * 3);

  for (int32 ix = 0; ix < 1024; ix++)
    assert(inSet(set,ix*3));

  deleteSet(set);
  return Ok;
}

static retCode add64() {
  setPo set = createSet(0);
  assert(set!=Null);

  for (int32 ix = 0; ix < 64; ix++)
    addToSet(set, ix);

  assert(set->data[0] == (uinteger)(-1));
  assert(set->count==1);

  for (int32 ix = 0; ix < 64; ix++)
    assert(inSet(set,ix));

  deleteSet(set);
  return Ok;
}

static retCode addEvens() {
  setPo set = createSet(0);
  assert(set!=Null);

  for (int32 ix = 0; ix < 32; ix++)
    addToSet(set, ix * 2);

  assert(set->count==1);

  for (int32 ix = 0; ix < 32; ix++) {
    assert(inSet(set,ix*2));
    assert(!inSet(set,ix*2+1));
  }

  deleteSet(set);
  return Ok;
}

static retCode addANegativeBunch() {
  setPo set = createSet(0);
  assert(set!=Null);

  assert(setIsEmpty(set));

  for (int32 ix = 0; ix < 128; ix++)
    addToSet(set, ix * 3);

  for (int32 ix = -1; ix >= -128; ix--)
    addToSet(set, ix * 3);

  for (int32 ix = -128; ix < 128; ix++)
    assert(inSet(set,ix*3));

  assert(!setIsEmpty(set));

  deleteSet(set);
  return Ok;
}

static retCode removeABunch() {
  setPo set = createSet(0);
  assert(set!=Null);

  for (int32 ix = 0; ix < 128; ix++)
    addToSet(set, ix * 3);

  // showSet(logFile, set);

  for (int32 ix = 0; ix < 128; ix++) {
    assert(inSet(set,ix*3));

    removeFromSet(set, ix * 3);
    assert(!inSet(set,ix*3));
  }

  // showSet(logFile, set);

  assert(setIsEmpty(set));

  deleteSet(set);
  return Ok;
}

retCode setTests() {
  setupTests();
  tryRet(run_test(createAndDestroy));
  tryRet(run_test(add64));
  tryRet(run_test(addEvens));
  tryRet(run_test(addABunch));
  tryRet(run_test(addANegativeBunch));
  tryRet(run_test(removeABunch));
  tearDownTests();
  return Ok;
}
