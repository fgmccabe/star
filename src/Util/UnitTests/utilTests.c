//
// Created by Francis McCabe on 8/23/21.
//

#include "utilTests.h"
#include "buddyTest.h"
#include "intervalTests.h"
#include "unitTests.h"
#include "multiTests.h"
#include "miscTest.h"
#include "matchTests.h"
#include "setTests.h"
#include "intervalTests.h"
#include "timertests.h"

retCode all_tests() {
  tryRet(run_test(test_buddy));
  tryRet(run_test(test_many_blocks));
  tryRet(run_test(multiTests));
  tryRet(run_test(miscTests));
  tryRet(run_test(matchTests));
  tryRet(run_test(setTests));
  tryRet(run_test(intervalTests));
  tryRet(run_test(timerTests));
  return Ok;
}
