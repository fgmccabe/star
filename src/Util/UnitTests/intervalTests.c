//
// Created by Francis McCabe on 12/04/25.
//

#include <formioP.h>
#include <assert.h>
#include "intervalTests.h"

static intervalSetPo testSet = Null;

static void setupTests() {
  testSet = newIntervalSet();
}

static void tearDownTests() {
  deleteIntervalSet(testSet);
}

static retCode createAndDestroy() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);
  deleteIntervalSet(set);
  return Ok;
}

static retCode addABunch() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);

  for (int32 ix = 0; ix < 1024; ix++)
    addToIntervalSet(set, ix * 3);

  assert(checkIntervalSet(set));

  outMsg(logFile, "addABunch:");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  for (int32 ix = 0; ix < 1024; ix++)
    assert(inIntervalSet(set,ix*3));

  deleteIntervalSet(set);
  return Ok;
}

static retCode add64() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);

  for (int32 ix = 0; ix < 64; ix++)
    addToIntervalSet(set, ix);

  assert(checkIntervalSet(set));

  outMsg(logFile, "add64:");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  for (int32 ix = 0; ix < 64; ix++)
    assert(inIntervalSet(set,ix));

  deleteIntervalSet(set);
  return Ok;
}

static retCode addEvens() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);

  for (int32 ix = 0; ix < 32; ix++)
    addToIntervalSet(set, ix * 2);

  assert(checkIntervalSet(set));

  outMsg(logFile, "addEvens:");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  for (int32 ix = 0; ix < 32; ix++) {
    assert(inIntervalSet(set,ix*2));
    assert(!inIntervalSet(set,ix*2+1));
  }

  deleteIntervalSet(set);
  return Ok;
}

static retCode addANegativeBunch() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);

  assert(intervalSetIsEmpty(set));

  for (int32 ix = 0; ix < 32; ix++)
    addToIntervalSet(set, ix * 3);

  assert(checkIntervalSet(set));

  outMsg(logFile, "addNegatives(1):");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  for (int32 ix = -1; ix >= -32; ix--)
    addToIntervalSet(set, ix * 3);

  assert(checkIntervalSet(set));

  outMsg(logFile, "addNegatives(2):");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");
  for (int32 ix = -32; ix < 32; ix++)
    assert(inIntervalSet(set,ix*3));

  assert(!intervalSetIsEmpty(set));

  assert(checkIntervalSet(set));

  deleteIntervalSet(set);
  return Ok;
}

static retCode removeABunch() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);

  for (int32 ix = 0; ix < 128; ix++)
    addToIntervalSet(set, ix * 3);

  checkIntervalSet(set);

  outMsg(logFile, "removeABunch:");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  for (int32 ix = 0; ix < 128; ix++) {
    assert(inIntervalSet(set,ix*3));

    removeFromIntervalSet(set, ix * 3);
    assert(!inIntervalSet(set,ix*3));
  }

  assert(checkIntervalSet(set));

  outMsg(logFile, "bunchRemoved:");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  assert(intervalSetIsEmpty(set));

  deleteIntervalSet(set);
  return Ok;
}

static retCode findSome() {
  intervalSetPo set = newIntervalSet();
  assert(set!=Null);

  for (int32 ix = 0; ix < 16; ix++)
    addToIntervalSet(set, ix * 2);

  assert(checkIntervalSet(set));

  outMsg(logFile, "we got some:");
  showIntervalSet(logFile, set);
  outStr(logFile, "\n");

  int32 ix;

  tryRet(findElement(set,0,&ix));
  assert(ix==0);

  tryRet(findElement(set,1,&ix));
  assert(ix==2);

  assert(findElement(set,64,&ix)==Error);

  assert(findSpace(set,0)==1);
  assert(findSpace(set,1)==1);


  assert(findSpace(set,15)==15);
  assert(findSpace(set,16)==17);

  deleteIntervalSet(set);
  return Ok;
}

retCode intervalTests() {
  setupTests();
  tryRet(run_test(createAndDestroy));
  tryRet(run_test(add64));
  tryRet(run_test(addEvens));
  tryRet(run_test(addABunch));
  tryRet(run_test(addANegativeBunch));
  tryRet(run_test(removeABunch));
  tryRet(run_test(findSome));
  tearDownTests();
  return Ok;
}
