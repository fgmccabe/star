//
// Created by Francis McCabe on 8/23/21.
//

#include "engineTests.h"

static retCode emptyTest() {
  return Ok;
}

retCode all_tests() {
  tryRet(run_test(emptyTest));
  return Ok;
}

