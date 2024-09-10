//
// Created by Francis McCabe on 09/10/2024
//


#include "regset.h"
#include "unitTests.h"
#include "arm64P.h"
#include "macros.h"
#include "test_infra.h"

retCode test_set1(){
  assemCtxPo ctx = createCtx();
  
  uint8 tgt[] = {};

  return checkCode(tgt, NumberOf(tgt), ctx);
}


retCode regset_tests() {
  tryRet(run_test(test_set1));

  return Ok;
}
  

