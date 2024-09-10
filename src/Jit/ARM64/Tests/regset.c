//
// Created by Francis McCabe on 09/10/2024
//


#include "regset.h"
#include "unitTests.h"
#include "arm64P.h"
#include "macros.h"
#include "test_infra.h"

static registerMap mapFromArray(armReg regs[], int count){
  registerMap set = emptyRegSet();

  for(int ix=0;ix<count;ix++)
    set = addReg(set, regs[ix]);

  return set;
}

retCode test_set1(){
  assemCtxPo ctx = createCtx();

  armReg someRegs[] = {X0, X3, X4, X5, X10, X20, X17};

  registerMap map = mapFromArray(someRegs,NumberOf(someRegs));

  saveRegisters(ctx,map);
  
  uint8 tgt[] = {};

  return checkCode(tgt, NumberOf(tgt), ctx);
}


retCode regset_tests() {
  tryRet(run_test(test_set1));

  return Ok;
}
  

