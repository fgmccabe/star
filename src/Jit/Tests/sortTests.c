//
// Created by Francis McCabe on 10/24/25.
//

#include "unitTests.h"
#include "sortTests.h"

#define Arg(Src,Dst) ((ArgSpec){.src=Src,.dst=Dst,.mark=True,.group=-1})

static retCode test_emptyset()
{
  return Ok;
}

static retCode checkResult(logical truth, char* msg)
{
  if (truth)
    return Ok;
  else{
    outMsg(logFile, msg);
    return Error;
  }
}

static retCode test_single()
{
  ArgSpec specs[1] = {Arg(RG(R1), RG(R2))};
  int32 groups = sortSpecs(specs, NumberOf(specs));

  tryRet(checkResult(groups==1,"expecting 1 group"));
  return Ok;
}

static retCode test_double()
{
  ArgSpec specs[] = {Arg(RG(R1), RG(R2)),Arg(RG(R2), RG(R3))};
  int32 count = NumberOf(specs);
  int32 groups = sortSpecs(specs, count);

  tryRet(checkResult(groups==1,"expecting 2 groups"));

  argSpecPo gp1[1], gp2[1];

  collectGroup(specs,count,0,gp1);
  collectGroup(specs,count,1,gp2);

  tryRet(checkResult(gp1[0]==&specs[0],"wrong group order"));

  return Ok;
}

retCode all_tests()
{
  tests_run = 0;

  tryRet(run_test(test_emptyset));
  tryRet(run_test(test_single));
  tryRet(run_test(test_double));

  return Ok;
}
