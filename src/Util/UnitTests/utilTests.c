//
// Created by Francis McCabe on 8/23/21.
//

#include "utilTests.h"

retCode all_tests() {
  tryRet(run_test(test_buddy));
  tryRet(run_test(test_many_blocks));
  tryRet(run_test(multiTests));
  tryRet(run_test(miscTests));
  tryRet(run_test(matchTests));
  tryRet(run_test(timerTests));
  return Ok;
}
