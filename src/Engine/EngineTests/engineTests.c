//
// Created by Francis McCabe on 8/23/21.
//

#include "engineTests.h"
#include "vectorTests.h"

static retCode emptyTest() {
  return Ok;
}

retCode all_tests() {
  tryRet(run_test(emptyTest));
  tryRet(vector_tests());
  return Ok;
}

