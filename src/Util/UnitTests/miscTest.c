//
// Created by Francis McCabe on 12/29/21.
//

#include "miscTest.h"

#include <formioP.h>
#include <assert.h>
#include "miscTest.h"

static void setupTests() {
}

static void tearDownTests() {
}

retCode lg2Tests() {
  integer cx = 1;
  for (integer i = 0; i < 63; i++) {
    assert(lg2(cx << i) == i);
  }
  return Ok;
}

retCode gcdTests() {
  integer a = 1071;
  integer b = 462;

  integer gcd = intGCD(a, b);

  if (debugUnitTests)
    outMsg(logFile, "gcd(%d,%d) is %d\n%_", a, b, gcd);

  assert(gcd == 21);
  return Ok;
}

retCode miscTests() {
  setupTests();
  tryRet(run_test(lg2Tests));
  tryRet(run_test(gcdTests));
  tearDownTests();
  return Ok;
}
