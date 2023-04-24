//
// Created by Francis McCabe on 12/29/21.
//

#include "miscTest.h"

#include <formioP.h>
#include <assert.h>

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
  if (debugUnitTests)
    outMsg(logFile, "gcd(%d,%d) is %d\n%_", 1071, 462, intGCD(1071, 462));

  assert(intGCD(1071, 462) == 21);
  assert(intGCD(1071, -462) == 21);
  assert(intGCD(2, 3) == 1);
  return Ok;
}

retCode suffix(char *s1, char *s2) {
  if (uniIsSuffix(s1, uniStrLen(s1), s2, uniStrLen(s2)))
    return Ok;
  else
    return Fail;
}

retCode find(char *s1, char *s2) {
  if (uniSearch(s1, uniStrLen(s1), 0, s2, uniStrLen(s2)) >= 0)
    return Ok;
  else
    return Fail;
}

retCode suffixTest() {
  tryRet(suffix("", ""));
  tryRet(suffix("foo", "barfoo"));
  tryRet(suffix("bar", "barfoobar"));

  negRet(suffix("barfoobar", "bar"));
  negRet(suffix("barfoo", "bar"));
  negRet(suffix("abar", "bar"));

  tryRet(find("ac1.star","star"));
  return Ok;
}

retCode miscTests() {
  setupTests();
  tryRet(run_test(lg2Tests));
  tryRet(run_test(gcdTests));
  tryRet(run_test(suffixTest));
  tearDownTests();
  return Ok;
}
