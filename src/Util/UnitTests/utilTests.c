//
// Created by Francis McCabe on 8/23/21.
//

#include "utilTests.h"

retCode all_tests() {
  tryRet(run_test(test_buddy));
  tryRet(run_test(test_many_blocks));
  return Ok;
}
