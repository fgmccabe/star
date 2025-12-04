//
// Created by Francis McCabe on 12/04/25.
//

#include <formioP.h>
#include <assert.h>
#include "setTests.h"

static void setupTests() {
}

static void tearDownTests() {
}

static retCode createAndDestroy(){
  setPo set =  createSet(0, 128, False);
  assert(set!=Null);
  deleteSet(set);
  return Ok;
}

retCode setTests() {
  setupTests();
  tryRet(run_test(createAndDestroy));
  tearDownTests();
  return Ok;
}