//
// Created by Francis McCabe on 12/04/25.
//

#include <formioP.h>
#include <assert.h>
#include "setTests.h"
#include "setP.h"

static setPo testSet = Null;

static void setupTests() {
  testSet = newSet();
}

static void tearDownTests() {
  deleteSet(testSet);
}

static retCode createAndDestroy() {
  setPo set = newSet();
  assert(set!=Null);
  deleteSet(set);
  return Ok;
}

static retCode addABunch() {
  setPo set = newSet();
  assert(set!=Null);

  for (int32 ix = 0; ix < 1024; ix++)
    addToSet(set, ix * 3);

  for (int32 ix = 0; ix < 1024; ix++)
    assert(inSet(set,ix*3));

  deleteSet(set);
  return Ok;
}

static retCode add64() {
  setPo set = newSet();
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
  setPo set = newSet();
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
  setPo set = newSet();
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
  setPo set = newSet();
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

setPo iota(int32 min, int32 max, int32 step) {
  setPo set = newSet();
  for (int32 ix = min; ix <= max; ix += step)
    addToSet(set, ix);
  return set;
}

logical isDense(setPo set, int32 min, int32 max) {
  for (int32 ix = min; ix <= max; ix++)
    if (!inSet(set, ix))
      return False;
  return True;
}

static retCode unionTest() {
  setPo lhs = iota(0, 256, 2);
  setPo rhs = iota(1, 255, 2);

  setPo unSets = unionSet(lhs, rhs);

  check(isDense(unSets, 0, 256), "incorrect union");
  deleteSet(unSets);

  setPo selSet = unionSet(lhs, lhs);
  check(equalSets(lhs,selSet), "self union should be same");
  deleteSet(selSet);

  deleteSet(lhs);
  deleteSet(rhs);
  return Ok;
}

static retCode sectTest() {
  setPo lhs = iota(0, 256, 2);
  setPo rhs = iota(1, 255, 2);

  setPo unSets = intersectSet(lhs, rhs);

  check(setIsEmpty(unSets), "incorrect intersection");
  deleteSet(unSets);

  setPo selSet = intersectSet(lhs, lhs);
  check(equalSets(lhs,selSet), "self intersection should be same");
  deleteSet(selSet);

  deleteSet(lhs);
  deleteSet(rhs);
  return Ok;
}

static retCode rangeTest() {
  setPo iotaSet = iota(0, 128, 10);

  showSet(logFile, iotaSet);

  check(inSetRange(iotaSet, 2,11),"[2,11) should intersect");
  check(!inSetRange(iotaSet, 2,4),"[2,4) should not intersect");
  check(inSetRange(iotaSet, 60,65),"[60,65) should intersect");
  check(!inSetRange(iotaSet, 61,64),"[61,64) should not intersect");

  deleteSet(iotaSet);
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
  tryRet(run_test(unionTest));
  tryRet(run_test(sectTest));
  tryRet(run_test(rangeTest));
  tearDownTests();
  return Ok;
}
