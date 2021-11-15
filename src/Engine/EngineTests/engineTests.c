//
// Created by Francis McCabe on 8/23/21.
//

#include "Headers/engineTests.h"
#include "bcdTest.h"

static retCode emptyTest() {
  return Ok;
}

retCode all_tests() {
  tryRet(run_test(emptyTest));
  tryRet(run_test(bcdTests));
  return Ok;
}

